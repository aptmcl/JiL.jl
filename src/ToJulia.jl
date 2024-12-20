abstract type Scope end

struct GlobalScope <: Scope
  defs::Dict{Symbol, Any}
end

const null_scope = GlobalScope(Dict{Symbol, Any}())
const global_scope = GlobalScope(Dict{Symbol, Any}())

print_scope_defs(defs) = begin
  for (k, v) in defs
      println("  [$k => $v]")
    end
end

Base.show(io::IO, scope::GlobalScope) =
  begin
    println(io, "Scope:")
    print_scope_defs(scope.defs)
  end

struct LocalScope <: Scope
  parent::Scope
  defs::Dict{Symbol, Any}
end

Base.show(io::IO, scope::LocalScope) =
  begin
    show(io, scope.parent)
    println(io, "  ------")
    print_scope_defs(scope.defs)
  end

new_scope(names, inits, parent) =
  LocalScope(parent, Dict{Symbol, Any}(name=>init for (name, init) in zip(names, inits)))

add_binding!(name, init, scope) =
  scope.defs[name] = init

get_binding(name, scope::GlobalScope) =
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

# functions
struct JiLFunction
  scope
  parameters
  body
end

Base.show(io::IO, f::JiLFunction) =
  begin
    println(io, "(lambda $(f.parameters) -> $(f.body))")
  end

# Currying
tojulia(scope::Scope) = form -> tojulia(form, scope)

# Julia macros can only be defined at the top level.
# This means that local macros must be lifted.
const lifted_forms = []

lift!(form) = begin
  push!(lifted_forms, form)
  end

tojulia_toplevel(form) = begin
  empty!(lifted_forms)
  let ast = tojulia(form, global_scope)
    Expr(:toplevel, lifted_forms..., ast) 
  end
end

tojulia(form::Any, scope) = form

#= 
Due to the presence of symbol macros, We need to distinguish a symbol being assigned from a symbol being evaluated.
Only in latter should we attempt to expand symbol macros. 
We are not considering the CL exception of generalized assignment of symbol macros.
=#

tojulia_var(form::Symbol, scope) =
  let token = String(form)
    '.' in token ?
      let (pkg, name) = split(token, '.')
        Expr(:., Symbol(pkg), QuoteNode(Symbol(name)))
      end :
      form
  end

tojulia(form::Symbol, scope) =
  let init = get_binding(form, scope)
    init isa JiLSymbolMacro ?
      tojulia_macrocall(form, init, (), scope) :
      let token = String(form)
        '.' in token ?
          let (pkg, name) = split(token, '.')
            Expr(:., Symbol(pkg), QuoteNode(Symbol(name)))
          end :
          form
      end
  end


tojulia(form::Nil, scope) = form

tojulia(form::Cons, scope) =
  let head = form.head,
      tail = form.tail
    if head isa Symbol
      tojulia(Val{head}(), tail, scope) # To dispatch on the operator
    elseif head isa Cons
      Expr(:call, tojulia(head, scope), tojulia_vector(tail, scope)...)
    else
      error("Unknown form  $form")
    end
  end

# Fallback: everything is treated as a function/macro call.
tojulia(::Val{op}, args, scope) where {op} =
  op isa Symbol && String(op)[1] == '@' ? #julia macro
    Expr(:macrocall, tojulia(op, scope), LineNumberNode(1, :JiL), tojulia_vector(args, scope)...) :
    let init = get_binding(op, scope)
      #println("Binding for $op is $init")
      init isa JiLMacro ?
        tojulia_macrocall(op, init, args, scope) :
        Expr(:call, tojulia(op, scope), tojulia_vector(args, scope)...)
    end

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

tojulia(::Val{:(=)}, args, scope) =
  :(==($(tojulia_vector(args, scope)...)))

# Parameters
# Given that parameters might refer to the previous ones, this needs to incrementally extend the scope
tojulia_parameters(parameters, scope) =
  isempty(parameters) ?
    ([], scope) :
    let (p, ps...) = parameters
      p isa Cons && p.head === :parameters ? # We need to process the rest first
        let (params, scope) = tojulia_parameters(ps, scope),
            (param, scope) = tojulia_parameter(p, scope)
          ([param, params...], scope)
        end :
        let (param, scope) = tojulia_parameter(p, scope),
            (params, scope) = tojulia_parameters(ps, scope)
          ([param, params...], scope)
        end
    end
      
tojulia_parameter(name::Symbol, scope) =
  tojulia(name, scope), 
  new_scope([name], [missing], scope)

tojulia_parameter(p::Cons, scope) =
  tojulia_parameter(Val{p.head}(), p.tail, scope)

tojulia_parameter(::Val{:(...)}, (name,), scope) =
  Expr(:(...), tojulia(name, scope)),
  new_scope([name], [missing], scope)

tojulia_parameter(::Val{:kw}, (name, init), scope) =
  Expr(:kw, tojulia(name, scope), tojulia(init, scope)),
  new_scope([name], [init], scope)

tojulia_parameter(::Val{:parameters}, parameters, scope) =
  let (params, scope) = tojulia_parameters(parameters, scope)
    Expr(:parameters, params...), scope
  end

tojulia(::Val{Symbol("&key")}, expr, scope) =
  let key_params = [p isa Cons && !(p.head === :(...)) ?
                      Expr(:kw, tojulia(p.head, scope), tojulia(p.tail.head, scope)) :
                      tojulia(p, scope)
                    for p in expr]
    Expr(:(parameters), key_params...)
  end

# Anonymous functions
tojulia(::Val{:λ}, (params, body), scope) = 
  tojulia(Val{:lambda}(), (params, body), scope)

tojulia(::Val{:lambda}, (parameters, body), scope) =
  let (params, scope) = tojulia_parameters(parameters, scope)
    :(($(params...),) -> $(tojulia(body, scope)))
  end

# Definitions
tojulia(::Val{:def}, (sig, inits...), scope) =
  let inits = collect(inits),
      init = length(inits) == 1 ? inits[1] : error("Incorrect number of arguments to def")
    if sig isa Symbol
      let val = tojulia(init, scope)
        add_binding!(sig, val, scope)
        :($(tojulia_var(sig, scope)) = $val)
      end
    elseif sig isa Cons
      if sig.head isa Symbol # A named function definition
        let (name, parameters, body) = (sig.head, sig.tail, init),
            (params, body_scope) = tojulia_parameters(sig.tail, scope)
          add_binding!(name, JiLFunction(params, body, scope), scope)
          :($(tojulia_var(name, scope))($(params...),) = $(tojulia(body, body_scope)))
        end
      else
        tojulia(list(:def, head(sig), list(:lambda, tail(sig), init), scope))
      end
    else
      error("Unknown form $(list(:def, sig, init))")
    end
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

tojulia(::Val{:struct}, (name_super, slots...), scope) =
  let slots = collect(slots),
      (jname, super) = name_super isa Symbol ? 
                         (tojulia_var(name_super, scope), Any) : 
                         (tojulia_var(head(name_super), scope), tojulia(head(read(name_super)), scope))
    :(struct $jname <: $(super); $([tojulia_var(slot, scope) for slot in slots]...) end)
  end

####################################################################
# Macros
# For now, nothing fancy, no phases, no hygiene, just CL-like macros
struct JiLMacro
  name
  scope
  parameters
  body
  expander
end

Base.show(io::IO, m::JiLMacro) =
  begin
    println(io, "(macro $(m.name) $(m.parameters) $(m.body))")
  end

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

A second problem, which I was hoping to avoid by resorting to Julia macros, is 
assigning the correct modules to each identifier.

A third problem, which, again, I was hopping to avoid by resorting to Julia macros,
if hygiene.

Jumping over the first problem, here is a possible solution.
=#
tojulia(::Val{:macro}, (name, params, body), scope) =
  let jname = tojulia_var(name, scope),
      uniquejname = Symbol(jname, gensym()),
      macroname = Symbol("@", uniquejname),
      (juparams, body_scope) = tojulia_parameters(params, scope),
      newmacro = :(macro $(uniquejname)($(juparams...),)
         esc(JiL.tojulia($(tojulia(body, scope)), JiL.global_scope))
      end)
    #debug_lisp_to_julia && println(" => ", newmacro)
    add_binding!(name, JiLMacro(macroname, scope, params, body, newmacro), scope)
    scope === global_scope ?
      Expr(:toplevel, newmacro, :($jname = $macroname)) :
      (lift!(newmacro); :nothing)
  end

#=
tojulia_macrocall(jilmacro, args, scope) =
  Expr(:macrocall, jilmacro.name, LineNumberNode(1, :JiL), tojulia_vector(args, scope)...)
=#  

#=
As discussed previously, this does not address the case that, in general, JiL macro
arguments are not proper JiL expressions. One way to solve this is to force the s-expr
structure (Cons, Pair, whatever) to enter the macro call. If Julia accepts it, then
we might have a solution.
=#

tojulia_macrocall(name, jilmacro, args, scope) =
  Expr(:macrocall, jilmacro.scope == global_scope ? name : jilmacro.name, LineNumberNode(1, :JiL), args...)
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
  elseif ex isa CodeInfo
      ex.debuginfo = Core.DebugInfo(ex.debuginfo.def) # TODO: filter partially, but keep edges
  end
  return ex
end

tojulia(::Val{:macroexpand}, (form, ), scope) =
  :(JiL.clean_expr!(@macroexpand($(tojulia(form, scope)))))

###########################################

tojulia(::Val{:macrolet}, (binds, body...), scope) =
  let local_scope = new_scope([], [], scope)
    for bind in binds
      tojulia(Val{:macro}(), bind, local_scope) # They are going to be lifted
    end
    tojulia(list(:begin, body...), local_scope)
  end

####################################################################
# Symbol Macros
# For now, nothing fancy, no phases, no hygiene, just CL-like macros
struct JiLSymbolMacro
  name
  scope
  body
  expander
end

Base.show(io::IO, m::JiLSymbolMacro) =
  begin
    println(io, "(symbol-macro $(m.name) $(m.body))")
  end


tojulia(::Val{:var"symbol-macro"}, (name, body), scope) =
  let jname = tojulia_var(name, scope),
      uniquejname = Symbol(jname, gensym()),
      macroname = Symbol("@", uniquejname),
      newmacro = :(macro $(uniquejname)()
         esc(JiL.tojulia($(tojulia(list(:quote, body), scope)), JiL.global_scope))
      end)
    #debug_lisp_to_julia && println(" => ", newmacro)
    add_binding!(name, JiLSymbolMacro(macroname, scope, body, newmacro), scope)
    scope === global_scope ?
      Expr(:toplevel, newmacro, :($jname = $macroname)) :
      (lift!(newmacro); :nothing)
  end

#=
(symbol-macrolet ((x 'foo))
   (list x (let ((x 'bar)) x)))
=#
tojulia(::Val{:var"symbol-macrolet"}, (binds, body...), scope) =
  let local_scope = new_scope([], [], scope)
    for bind in binds
      tojulia(Val{:var"symbol-macro"}(), bind, local_scope) # They are going to be lifted
    end
    tojulia(list(:begin, body...), local_scope)
  end


# Globals
tojulia(::Val{:global}, (form, ), scope) = 
  :(global $(tojulia(form, scope)))

# Modules
tojulia(::Val{:using}, (form, ), scope) =
  :(using $(tojulia(form, scope)))

# Blocks
tojulia(::Val{:begin}, forms, scope) =
  :(begin $(tojulia_vector(forms, scope)...) end)

# Local scopes
tojulia(::Val{:let}, (binds, body...), scope) =
  let names = [tojulia_var(head(bind), scope) for bind in binds],
      inits = [tojulia(head(tail(bind)), scope) for bind in binds],
      scope = new_scope(names, inits, scope)
    :(let ($(names...),) = ($(inits...),)
      $(tojulia_vector(body, scope)...)
      end)
  end

tojulia(::Val{Symbol("let*")}, (binds, body...), scope) =
  let tojulia_binds(binds, scope) = 
        isempty(binds) ?
          ([], scope) :
          let bind = head(binds),
              name = tojulia_var(head(bind), scope),
              init = tojulia(head(tail(bind)), scope),
              scope = new_scope([name], [init], scope),
              (julia_binds, scope) = tojulia_binds(tail(binds), scope)
            ([:($(name) = $(init)), julia_binds...], scope)
          end
    let (julia_binds, scope) = tojulia_binds(binds, scope)
      :(let $(julia_binds...)
         $(tojulia_vector(body, scope)...)
        end)
    end
  end

tojulia(::Val{:and}, exprs, scope) =
  Expr(:&&, tojulia_vector(exprs, scope)...)
  
tojulia(::Val{:or}, exprs, scope) =
  Expr(:||, tojulia_vector(exprs, scope)...)


tojulia(::Val{:export}, names, scope) =
  let names = names #=[let init = get_binding(name, scope)
                 init isa JiLMacro ?
                 init.name : name
               end for name in names]=#
    Expr(:export, [tojulia_var(name, scope) for name in names]...)
  end