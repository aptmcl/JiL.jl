#=
The JiL 'compiler' operates during the parsing phase, injecting top-level macros 
and the corresponding macrocalls in the Julia code stream, which is then compiled 
and loaded as regular Julia code.

However, for the JiL compiler to operate properly, it needs to remember some of
the information that it collected during previous parse/compilation phases so that
it can properly process subsequent macrocalls, even in different modules that
use/import the one where the macros are defined.

This means that we need the compile-time environment to survive that compilation
phase so that it is available in subsequent compilations. The good news is that
we only need to be concerned with the global environment, as local macros are
processed locally and can then be forgotten.

One way to achieve this is to inject in Julia's code stream additional instructions
to restore the environment, possibly, overriding the one that was generated during a
previous compilation.

One additional problem is that we should take proper care of modules, which we
are not yet considering.
=#

abstract type Scope end

struct ModuleScope <: Scope
  name::Symbol
  defs::Dict{Symbol, Any}
  # Julia macros can only be defined at the top level.
  # This means that local macros must be lifted.
  lifted_forms::Vector{Expr}
  exports::Vector{Symbol}
end

new_module_scope(name) =
  ModuleScope(name, Dict{Symbol, Any}(), Expr[], Symbol[])

module_name(m::ModuleScope) = m.name

const JiL_scope = new_module_scope(nameof(@__MODULE__))

print_scope_defs(io, defs) = begin
  for (k, v) in defs
      println(io, "  [$k => $(typeof(v))]")
    end
end

Base.show(io::IO, scope::ModuleScope) =
  begin
    println(io, "Scope:", scope.name)
    #print_scope_defs(io, scope.defs)
  end

struct LocalScope <: Scope
  parent::Scope
  defs::Dict{Symbol, Any}
end

Base.show(io::IO, scope::LocalScope) =
  begin
    show(io, scope.parent)
    println(io, "  ------")
    print_scope_defs(io, scope.defs)
  end

new_local_scope(names, inits, parent) =
  let frobnicate(names, inits, newnames, newinits) =
        isempty(names) ?
          (newnames, newinits) :
          let name = names[1]
            name isa Expr && name.head == :tuple ?
              frobnicate([name.args..., names[2:end]...], [[:(init[$i]) for i in 1:length(name.args)]..., inits[2:end]], newnames, newinits) :
              frobnicate(names[2:end], inits[2:end], [names[1], newnames...], [inits[1], newinits...])
          end
    LocalScope(parent, Dict{Symbol, Any}(map(=>, frobnicate(names, inits, [], [])...)))
  end

top_scope(s::ModuleScope) = s
top_scope(s::LocalScope) = top_scope(s.parent)

lift!(form, scope) = 
  let scope = top_scope(scope)
    #println("Injecting $form in $scope")
    push!(scope.lifted_forms, form)
  end

scope_module_name(s) = top_scope(s).name

# Bindings

add_binding!(name, init, scope) =
  scope.defs[name] = init

get_binding(name, scope::ModuleScope) =
  get(scope.defs, name) do
    #This is used to detect JiL macros, so I'm not sure it makes sense to search 
    #for Julia bindings, as they might not even exist yet.
    nothing
    #=let token = string(name)
      '.' in token ?
        let (modname, name) = split(token, '.')
          getglobal(eval(Symbol(modname)), Symbol(name))
        end :
      getglobal(Main, name)
    end =#
  end

get_binding(name, scope::LocalScope) =
  get(scope.defs, name) do
    get_binding(name, scope.parent)
  end

islocal(name, scope::ModuleScope) =
  haskey(scope.defs, name) ? false : error("Unbound variable $name")

islocal(name, scope::LocalScope) =
  haskey(scope.defs, name) ? true : islocal(name, scope.parent)

# For tuple assignment
islocal(names::Expr, scope::LocalScope) = all(v -> islocal(v, scope), names.args)

#=
Defined functions and macros need to be registered so that we know
what to do when they are called.
=#
# Functions
struct JiLFunction
  scope
  parameters
  body
end

Base.show(io::IO, f::JiLFunction) =
  begin
    println(io, "(lambda $(f.parameters) -> $(f.body))")
  end

# Macros
# For now, nothing fancy, no phases, no hygiene, just CL-like macros
struct JiLMacro
  name
  scope
  parameters
  body
end

Base.show(io::IO, m::JiLMacro) =
  begin
    println(io, "(macro $(m.name) $(m.parameters) $(m.body))")
  end

# For now, nothing fancy, no phases, no hygiene, just CL-like symbol macros
struct JiLSymbolMacro
  name
  scope
  body
end

Base.show(io::IO, m::JiLSymbolMacro) =
  begin
    println(io, "(symbol-macro $(m.name) $(m.body))")
  end
######################################################
# The fundamental operation is the conversion to Julia
tojulia(form::Any, scope) = error("Unknown form '$(form)' of type $(typeof(form))")

# Currying
tojulia(scope::Scope) = form -> tojulia(form, scope)

# Top-level forms need to be treated specially, as they need to include 
# all lifted forms during the conversion (mainly local macros that need to be at toplevel).
tojulia_toplevel(form, scope::Scope) =
  let ast = tojulia(form, scope),
      topscope = top_scope(scope),
      res = maybe_wrap_toplevel([topscope.lifted_forms..., ast])
    empty!(topscope.lifted_forms)
    res
  end

tojulia_toplevel(form, mod::Module) =
  tojulia_toplevel(form, find_or_create_JiL_scope(mod))


# If we don't know the module, we assume that it is the JiL_scope
#tojulia_toplevel(form) =
#  tojulia_toplevel(form, JiL_scope)


# Self-evaluating forms
tojulia(form::Number, scope) = form

tojulia(form::Char, scope) = form

tojulia(form::AbstractString, scope) = form

tojulia(form::Nil, scope) = form

tojulia(form::Array, scope) = form

#= 
Due to the presence of symbol macros, We need to distinguish a symbol being assigned 
from a symbol being evaluated. Only in latter should we attempt to expand symbol macros. 
We are not considering the CL exception of generalized assignment of symbol macros.
=#

#=
Due to the fact that dots are a legitimate character in Scheme, 
we should not do this:
tojulia_var(form::Symbol, scope) =
  let token = String(form)
    '.' in token ?
      let (pkg, name) = split(token, '.')
        Expr(:., Symbol(pkg), QuoteNode(Symbol(name)))
      end :
      form
  end
=#

tojulia_var(form::Symbol, scope) =
  form

#=
We also want to allow tuples to be used in let forms, as in let-values
=#
tojulia_var(form::Cons{Symbol}, scope) =
  Expr(:tuple, [tojulia_var(v, scope) for v in form]...)

tojulia(form::Symbol, scope) =
  tojulia_var_bound_to(form, get_binding(form, scope), scope)

tojulia_var_bound_to(form, init::JiLSymbolMacro, scope) =
  tojil_macrocall(form, init, (), scope)

tojulia_var_bound_to(form, init::Any, scope) =
  tojulia_var(form, scope)

# Cons means that we have a combination (either a special form, or a macro or function call)
tojulia(form::Cons, scope) =
  tojulia(form.head, form.tail, scope)

tojulia(op::Cons, args, scope) = # higher-order code
  tojulia_functioncall(op, args, scope)

tojulia(op::Symbol, args, scope) =
  String(op)[1] == '@' ? #julia macro
    tojulia_macrocall(op, args, scope) :
    tojulia(Val{op}(), args, scope) # To dispatch on the operator

# Special forms are going to be detected, e.g., through type Val{:if}
# Everything else is treated here as a function/macro call.
tojulia(::Val{op}, args, scope) where {op} =
  to_julia_op_bound_to(op, get_binding(op, scope), args, scope)

to_julia_op_bound_to(op, init::JiLMacro, args, scope) =
  tojil_macrocall(op, init, args, scope)

to_julia_op_bound_to(op, init::Any, args, scope) =
  tojulia_functioncall(op, args, scope)

tojulia_macrocall(op, args, scope) =
  Expr(:macrocall, tojulia(op, scope), LineNumberNode(1, :JiL), tojulia_vector(args, scope)...)

tojulia_functioncall(op, args, scope) =
  Expr(:call, tojulia(op, scope), tojulia_vector(args, scope)...)



tojulia(::Val{:(.)}, (inst, field), scope) =
  Expr(:(.), tojulia(inst, scope), QuoteNode(field))

tojulia(::Val{:(...)}, (name,), scope) =
  Expr(:(...), tojulia(name, scope))

tojulia(::Val{:kw}, (name, init), scope) =
  Expr(:kw, tojulia(name, scope), tojulia(init, scope))

tojulia_vector(val, scope) =
  [tojulia(e, scope) for e in val]

tojulia_tuple(val, scope) =
  tuple(tojulia_vector(val, scope)...)

tojulia(::Val{:quote}, (expr,), scope) =
  QuoteNode(expr)

tojulia(::Val{:quasiquote}, (expr, ), scope) =
  tojulia(expand_quasiquote(list(:quasiquote, expr)), scope)

# This must also be a function
tojulia(::Val{:(=)}, args, scope) =
  :(==($(tojulia_vector(args, scope)...)))

# Syntax for vectors (useful in Julia macros)
tojulia(::Val{:vect}, args, scope) =
  Expr(:vect, tojulia_vector(args, scope)...)

# Syntax for tuples (useful in Julia macros)
tojulia(::Val{:tuple}, args, scope) =
  Expr(:tuple, tojulia_vector(args, scope)...)

# Parameters
# Given that parameters might refer to the previous ones, this needs to incrementally extend the scope

# To handle (lambda foo (cons 1 foo)) and (lambda (foo . bar) (cons foo bar))  
tojulia_parameters(name::Symbol, body, scope) =
  let (param, body, scope) = tojulia_parameter(Val(:(...)), (name, ), body, scope)
    [param], list(:let, list(list(name, list(:list, list(:(...), name)))), body), scope
  end

tojulia_parameters(parameters::Pair, body, scope) =
  let (param, body, scope) = tojulia_parameter(parameters.car, body, scope),
      (params, body, scope) = tojulia_parameters(parameters.cdr, body, scope)
    ([param, params...], body, scope)
  end

tojulia_parameters(parameters, body, scope) =
  isempty(parameters) ?
    ([], body, scope) :
    let p = parameters.head,
        ps = parameters.tail
      p isa Cons && p.head === :parameters ? # We need to process the rest first
        let (params, body, scope) = tojulia_parameters(ps, body, scope),
            (param, body, scope) = tojulia_parameter(p, body, scope)
          ([param, params...], body, scope)
        end :
        let (param, body, scope) = tojulia_parameter(p, body, scope),
            (params, body, scope) = tojulia_parameters(ps, body, scope)
          ([param, params...], body, scope)
        end
    end

tojulia_parameter(name::Symbol, body, scope) =
  tojulia(name, scope),
  body, 
  new_local_scope([name], [missing], scope)

tojulia_parameter(p::Cons, body, scope) =
  tojulia_parameter(Val{p.head}(), p.tail, body, scope)

tojulia_parameter(::Val{:(...)}, (name,), body, scope) =
  Expr(:(...), tojulia(name, scope)),
  body, 
  new_local_scope([name], [missing], scope)

tojulia_parameter(::Val{:kw}, (name, init), body, scope) =
  Expr(:kw, tojulia(name, scope), tojulia(init, scope)),
  body,
  new_local_scope([name], [init], scope)

tojulia_parameter(::Val{:parameters}, parameters, body, scope) =
  let (params, body, scope) = tojulia_parameters(parameters, body, scope)
    Expr(:parameters, params...), body, scope
  end

tojulia(::Val{:var"&key"}, expr, body, scope) =
  let key_params = [p isa Cons && !(p.head === :(...)) ?
                      Expr(:kw, tojulia(p.head, scope), tojulia(p.tail.head, scope)) :
                      tojulia(p, scope)
                    for p in expr]
    Expr(:(parameters), key_params...)
  end

tojulia_parameter(::Val{:(::)}, (name, type), body, scope) =
  Expr(:(::), tojulia(name, scope), tojulia(type, scope)),
  body,
  scope

# Anonymous functions
tojulia(::Val{:λ}, (params, body), scope) = 
  tojulia(Val{:lambda}(), (params, body), scope)

tojulia(::Val{:lambda}, (parameters, body), scope) =
  let (params, body, scope) = tojulia_parameters(parameters, body, scope)
    :(($(params...),) -> $(tojulia(body, scope)))
  end

is_identifier(obj) = obj isa Symbol || (obj isa Cons && obj.head === :(.))

# Definitions
tojulia(::Val{:def}, (sig, inits...), scope) =
  let inits = collect(inits),
      init = length(inits) == 1 ? inits[1] : error("Incorrect number of arguments to def")
    if is_identifier(sig)
      let val = tojulia(init, scope)
        add_binding!(sig, val, scope)
        :($(tojulia_var(sig, scope)) = $val)
      end
    elseif sig isa Cons
      if sig.head isa Symbol # A named function definition
        let (name, parameters, body) = (sig.head, sig.tail, init),
            (params, body, body_scope) = tojulia_parameters(sig.tail, body, scope)
          add_binding!(name, JiLFunction(params, body, scope), scope)
          :($(tojulia_var(name, scope))($(params...),) = $(tojulia(body, body_scope)))
        end
      else
        tojulia(list(:def, head(sig), list(:lambda, tail(sig), init)), scope)
      end
    elseif sig isa Pair # Special-casing pairs is painful
      if sig.car isa Symbol # A named function definition
        let (name, parameters, body) = (sig.car, list(:(...), sig.cdr), init),
            (params, body, body_scope) = tojulia_parameters(sig.cdr, body, scope)
          add_binding!(name, JiLFunction(params, body, scope), scope)
          :($(tojulia_var(name, scope))($(params...),) = $(tojulia(body, body_scope)))
        end
      else
        tojulia(list(:def, head(sig), list(:lambda, tail(sig), init)), scope)
      end      
    else
      error("Unknown form $(list(:def, sig, init))")
    end
  end

tojulia(::Val{:defconst}, expr, scope) =
  Expr(:const, tojulia(Val(:def), expr, scope))

tojulia(::Val{:var"module-def"}, (mod, sig, body), scope) =
  let (name, parameters) = (sig.head, sig.tail),
      (params, body, body_scope) = tojulia_parameters(sig.tail, body, scope)
    :($(tojulia(mod, scope)).$(tojulia_var(name, scope))($(params...),) = $(tojulia(body, body_scope)))
  end

# Conditional expressions
tojulia(::Val{:if}, (cond, conseq, altern), scope) =
  :($(tojulia(cond, scope)) ? $(tojulia(conseq, scope)) : $(tojulia(altern, scope)))

# Abstract types
tojulia(::Val{:type}, (name, supers...), scope) =
  let supers = collect(supers),
      jname = tojulia_var(name, scope),
      nsupers = length(supers)
    nsupers == 0 ?
      :(abstract type $jname end) :
      nsupers == 1 ?
        :(abstract type $jname $(tojulia(first(super), scope)) end) :
        error("Can only have one supertype!")
  end

struct_data(name_super, slots, scope) =
  let slots = collect(slots),
      (jname, super) = name_super isa Symbol ? 
                         (tojulia_var(name_super, scope), Any) : 
                         (tojulia_var(head(name_super), scope), tojulia(head(read(name_super)), scope))
    jname, super, [tojulia_var(slot, scope) for slot in slots]
  end

tojulia(::Val{:struct}, (name_super, slots...), scope) =
  let (jname, super, slots) = struct_data(name_super, slots, scope)
    :(struct $jname <: $(super); $(slots...) end)
  end

tojulia(::Val{:var"mutable-struct"}, (name_super, slots...), scope) =
  let (jname, super, slots) = struct_data(name_super, slots, scope)
    :(mutable struct $jname <: $(super); $(slots...) end)
  end

#=
Access to a struct slot needs special syntax.
For now, we might just use Julia's getfield.
=#


####################################################################
#=
Macros are an interesting challenge because we want them to accept sexps as arguments
but Julia macros want to accept Exprs as arguments.
There is a subset of macrocalls that fit that bill, e.g., those where each argument
is translatable to Julia. These cases can be treated by implementing JiL macros as
just Julia macros (modulo the lifting needed to make them toplevel, as this is mandatory
in Julia) and then we just need to translate each argument to julia before we wrap all 
of them in the Julia macrocall.
However, there are macros where the arguments are not translatable to Julia, such as
cond. In the cond case, each clause is a list of sexps but it must not be treated as a
function call, even though it resembles one.

A second problem, which I was hoping to avoid by resorting to Julia macros, is hygiene.

For now, let's just experiment a bit.
=#  
find_or_create_JiL_scope(mod) = begin #print("Finding jil scope in $mod ")
  isdefined(mod, :JiL_scope) ?
    (#=println("FOUND"); =#mod.JiL_scope) :
    let modscope = nameof(mod) === :Main ? JiL_scope : new_module_scope(nameof(mod))
      #println("NOT FOUND")
      #=
      Should we add bindings?
      for exp in used_scope.exports
        add_binding!(exp, get_binding(exp, used_scope), scope)
      end =#
      mod.eval(:(global JiL_scope = $(QuoteNode(modscope))))
      modscope
    end
    end

tojulia(::Val{:macro}, (name, params, body), scope) =
  let jname = tojulia_var(name, scope),
      uniquejname = Symbol(jname, gensym()),
      macroname = Symbol("@", uniquejname),
      (juparams, body, body_scope) = tojulia_parameters(params, body, scope),
#      mbody = :($tojulia_toplevel($(tojulia(body, scope)), __module__)),
#      newmacro = :(macro $(uniquejname)($(juparams...))
      mbody = :($tojulia_toplevel($(tojulia(body, body_scope)), __scope__)),
      newmacro = :(macro $(uniquejname)(__scope__, $(juparams...))
         esc($mbody)
      end)
    debug_lisp_to_julia && println(" => ", newmacro)
    add_binding!(name, JiLMacro(macroname, scope, params, body), scope)
    #println("Lifting $newmacro to scope $scope")
    lift!(newmacro, scope)
    if scope isa ModuleScope
      lift!(:($jname = $macroname), scope)  # original name, to be used for exports
    end
    :nothing
  end

#=
tojulia_macrocall(name, jilmacro, args, scope) =
  Expr(:macrocall, name, LineNumberNode(1, :JiL), tojulia_vector(args, scope)...)
=#

#=
As discussed previously, this does not address the case that, in general, JiL macro
arguments are not proper JiL expressions. One way to solve this is to force the s-expr
structure (Cons, Pair, whatever) to enter the macro call. If Julia accepts it, then
we might have a solution.

A more serious problem is that of macros that define macros (e.g., defmacro). As
it is only by parsing the (macro foo (...) ...) form that the JiL compiler becomes aware that there
is a macro named foo and that all calls to foo are actually macrocalls, we need to
ensure that macrocalls are expanded until they cease to be macrocalls.
=#

#=
Which module should we used for the macroexpansion?
Idealy, it should be the current one, but we are still in the middle of its processing, so it does not exist yet.
An alternative is to use the module where the macro was defined.
Another alternative is to use Main.
=#

# We need a module to expand macros, even if only temporarily
module Scratch
  JiL_scope = nothing
end


tojil_macrocall(name, jilmacro, args, scope) = begin
  Expr(:macrocall, jilmacro.scope isa ModuleScope ? name : jilmacro.name, LineNumberNode(1, :JiL), scope, args...)
end

######################################
#=
Given that JiL macros use Julia macros, we can define macroexpand using the 
primitive macro-expansion mechanism:

(macro macro-expand (form)
  `(@macroexpand ,form))

However, Julia already has Base.macroexpand and this is going to create a conflict,
forcing users to qualify the use.

On the other hand, if we decide to implement JiL macros differently, we will have 
to change the macro-expansion, so it makes sense to have native support for it.
=#

clean_expr!(ex) = begin
  if ex isa Expr
    if ex.head === :block || ex.head === :quote
      filter!(ex.args) do x
          isa(x, Expr) && x.head === :line && return false
          isa(x, LineNumberNode) && return false
          return true
      end
    end
    for subex in ex.args
      subex isa Expr && clean_expr!(subex)
    end
    for (i, subex) in enumerate(ex.args)
      if subex isa Expr && subex.head === :block && length(subex.args) == 1
        ex.args[i] = subex.args[1]
      end
    end
  elseif ex isa Core.CodeInfo
      ex.debuginfo = Core.DebugInfo(ex.debuginfo.def) # TODO: filter partially, but keep edges
  end
  return ex
end

tojulia(::Val{:macroexpand}, (form, ), scope) =
  :(JiL.clean_expr!(@macroexpand($(tojulia_toplevel(form, scope)))))

tojulia(::Val{:macroexpand1}, (form, ), scope) =
  :(JiL.clean_expr!(@macroexpand1($(tojulia_toplevel(form, scope)))))

###########################################

tojulia(::Val{:macrolet}, (binds, body...), scope) =
  let local_scope = new_local_scope([], [], scope)
    for bind in binds
      tojulia(Val{:macro}(), bind, local_scope) # They are going to be lifted
    end
    tojulia(list(:begin, body...), local_scope)
  end

####################################################################
# Symbol Macros

tojulia(::Val{:var"symbol-macro"}, (name, body), scope) =
  let jname = tojulia_var(name, scope),
      uniquejname = Symbol(jname, gensym()),
      macroname = Symbol("@", uniquejname),
      newmacro = :(macro $(uniquejname)(__scope__)
         esc($tojulia_toplevel($(tojulia(list(:quote, body), scope)), __scope__))
      end)
    #debug_lisp_to_julia && println(" => ", newmacro)
    add_binding!(name, JiLSymbolMacro(macroname, scope, body), scope)
    scope isa ModuleScope ?
      Expr(:toplevel, newmacro, :($jname = $macroname)) :
      (lift!(newmacro, scope); :nothing)
  end

#=
(symbol-macrolet ((x 'foo))
   (list x (let ((x 'bar)) x)))
=#
tojulia(::Val{:var"symbol-macrolet"}, (binds, body...), scope) =
  let local_scope = new_local_scope([], [], scope)
    for bind in binds
      tojulia(Val{:var"symbol-macro"}(), bind, local_scope) # They are going to be lifted
    end
    tojulia(list(:begin, body...), local_scope)
  end


# Globals
tojulia(::Val{:global}, (form, ), scope) = 
  :(global $(tojulia(form, scope)))

# Blocks
tojulia(::Val{:begin}, forms, scope) =
  :(begin $(tojulia_vector(forms, scope)...) end)

# assignment
# HACK This needs to check if the binding is global!!!!
tojulia(::Val{:set!}, (var, expr), scope) =
  let var = tojulia_var(var, scope), # This handles tuples
      init = tojulia(expr, scope)
    islocal(var, scope) ?
     :($var = $init) :
     :(global $var = $init)
  end

# to use tuple syntax
tojulia(::Val{:values}, vals, scope) =
  Expr(:tuple, map(tojulia(scope), vals)...)


# Local scopes
tojulia(::Val{:let}, forms, scope) =
  forms[1] isa Symbol ? # Named let
    tojulia(Val(:namedlet), forms, scope) :
    let (binds, body...) = forms,
        names = [tojulia_var(head(bind), scope) for bind in binds],
        inits = [tojulia(head(tail(bind)), scope) for bind in binds],
        scope = new_local_scope(names, inits, scope)
      :(let ($(names...),) = ($(inits...),)
        $(tojulia_vector(body, scope)...)
        end)
    end

tojulia(::Val{:namedlet}, (name, binds, body...), scope) =
  let name = tojulia_var(name, scope),
      names = [tojulia_var(head(bind), scope) for bind in binds],
      inits = [tojulia(head(tail(bind)), scope) for bind in binds],
      scope = new_local_scope([name, names...], [missing, inits...], scope)
    :(let $name = missing
        $name($(names...), ) = begin
          $(tojulia_vector(body, scope)...)
        end
        $name($(inits...),)
      end)
  end

tojulia(::Val{:var"let*"}, (binds, body...), scope) =
  let tojulia_binds(binds, scope) = 
        isempty(binds) ?
          ([], scope) :
          let bind = head(binds),
              name = tojulia_var(head(bind), scope),
              init = tojulia(head(tail(bind)), scope),
              scope = new_local_scope([name], [init], scope),
              (julia_binds, scope) = tojulia_binds(tail(binds), scope)
            ([:($(name) = $(init)), julia_binds...], scope)
          end
    let (julia_binds, scope) = tojulia_binds(binds, scope)
      :(let $(julia_binds...)
         $(tojulia_vector(body, scope)...)
        end)
    end
  end

# let already supports tuples
tojulia(::Val{:var"let-values"}, (binds, body...), scope) =
  tojulia(Val(:let), (binds, body...), scope)

# let* already supports tuples
tojulia(::Val{:var"let*-values"}, (binds, body...), scope) =
  tojulia(Val(:var"let*"), (binds, body...), scope)

tojulia(::Val{:and}, exprs, scope) =
  Expr(:&&, tojulia_vector(exprs, scope)...)
  
tojulia(::Val{:or}, exprs, scope) =
  Expr(:||, tojulia_vector(exprs, scope)...)

# Iteration

tojulia(::Val{:for}, ((name, seq), body), scope) =
  let jname = tojulia_var(name, scope),
      jseq = tojulia(seq, scope),
      body_scope = new_local_scope([jname], [missing], scope)
    :(for $jname in $jseq; $(tojulia(body, body_scope)) end)
  end

# Modules
tojulia(::Val{:module}, (name, forms...), scope) =
  let jname = tojulia_var(name, scope),
      scope = new_module_scope(jname),
      tojulia_forms = reduce(vcat, [tojulia_toplevel(form, scope).args for form in forms])
    Expr(:module,
         true, 
         jname,
         Expr(:block, :(JiL_scope = $(QuoteNode(scope))), tojulia_forms...))
  end

tojulia(::Val{:using}, (form, ), scope) =
  #=
  When a module is used, we need to extract the JiL scope to identify all used macros.
  =#
  let jmodname = tojulia_module_path(form, scope),
      mod = get_module_from_path(form)
    if isdefined(mod, :JiL_scope)
      let used_scope = getglobal(mod, :JiL_scope)
        for exp in used_scope.exports
          add_binding!(exp, get_binding(exp, used_scope), scope)
        end
      end
    end
    Expr(:using, jmodname)
  end

get_module_from_path(path::Symbol, top=Main) =
  get_module_from_path(String(path), top)

get_module_from_path(path, top) =
  let (first, rest...) = split(path, '.', limit=2)
    first == "" ? # e.g., .Foo
      get_module_from_path(rest[1], parentmodule(top)) :
      let mod_id = Symbol(first)
        mod_id in names(top, imported=true) ?
          let mod = getfield(top, mod_id)
            rest == [] ? mod : get_module_from_path(rest[1], mod)
          end :
          error("Unknown module $(path)")
      end
  end

tojulia_module_path(sym, scope) =
  let token = String(sym),
      parts = split(token, '.')
    Expr(:., [part == "" ? :(.) : Symbol(part) for part in parts]...)
  end

tojulia(::Val{:export}, names, scope) =
  let names = names #=[let init = get_binding(name, scope)
                 init isa JiLMacro ?
                 init.name : name
               end for name in names]=#
    append!(scope.exports, names)
    Expr(:export, [tojulia_var(name, scope) for name in names]...)
  end

#=
To help the generation of Julia code, we're going to provide
a kind of "inline assembly".
=#

tojulia(::Val{:julia}, (str,), scope) =
  Meta.parse(str)

#=
Scheme's try-catch:
=#

tojulia(::Val{:try}, (expr, var, catch_expr), scope) =
  Expr(:try, 
       tojulia(expr, scope),
       tojulia_var(var, scope),
       Expr(:block, # Yes, the block is needed.
            tojulia(catch_expr, new_local_scope([var], [missing], scope))))