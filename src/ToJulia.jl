abstract type Scope end

struct GlobalScope <: Scope
  defs::Dict{Symbol, Any}
end

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

new_scope(parent, names, inits) =
  LocalScope(parent, Dict{Symbol, Any}(name=>init for (name, init) in zip(names, inits)))

add_binding!(name, init, scope) =
  scope.defs[name] = init

get_binding(name, scope::GlobalScope) =
  get(scope.defs, name) do 
    getglobal(Main, name)
  end

get_binding(name, scope::LocalScope) =
  get(scope.defs, name) do
    get_binding(name, scope.parent)
  end

# Currying
tojulia(scope::Scope) = form -> tojulia(form, scope)

tojulia(form) = tojulia(form, global_scope)

tojulia(form::Any, scope) = form

tojulia(form::Symbol, scope) = 
  let token = string(form)
    '.' in token ?
      let (pkg, name) = split(token, '.')
        Expr(:., Symbol(pkg), QuoteNode(Symbol(name)))
      end :
      form
  end

tojulia(form::Nil, scope) = form

tojulia(form::Cons, scope) =
  let head = form.head,
      tail = form.tail
    if head isa Symbol
      tojulia(Val{head}(), tail, scope)
    elseif head isa Cons
      Expr(:call, tojulia(head, scope), tojulia_vector(tail, scope)...)
    else
      error("Unknown form  $form")
    end
  end

tojulia_vector(val, scope) =
  collect(Any, map(tojulia(scope), val))

tojulia_tuple(val, scope) =
  tuple(tojulia_vector(val, scope)...)

tojulia(::Val{:quote}, (arg,), scope) =
  QuoteNode(arg)

tojulia(::Val{:(=)}, args, scope) =
  :(==($(tojulia_vector(args, scope)...)))

# Assignments
tojulia(::Val{:def}, (sig, init), scope) =
  if sig isa Symbol
    add_binding!(sig, init, scope)
    :($(tojulia(sig, scope)) = $(tojulia(init, scope)))
  elseif sig isa Cons
    tojulia(list(:def, head(sig), list(:lambda, tail(sig), init), scope))
  else
    error("Unknown form $(list(:def, sig, init))")
  end

# Conditional expressions
tojulia(::Val{:if}, (cond, conseq, altern), scope) =
  :($(tojulia(cond, scope)) ? $(tojulia(conseq, scope)) : $(tojulia(altern, scope)))

# Anonymous functions
tojulia(::Val{:λ}, (params, body), scope) = 
  tojulia(Val{:lambda}(), (params, body))

tojulia(::Val{:lambda}, (params, body), scope) =
  :(($(tojulia_vector(params, scope)...),) -> $(tojulia(body, scope)))

# Macros
# For now, nothing fancy, no phases, no hygiene, just CL-like macros
struct JiLMacro
  scope
  expander
end

tojulia(::Val{:macro}, (params, body), scope) =
  #The use of eval makes the macro belong to a different world 
  JiLMacro(scope, eval(:(($(tojulia_vector(params, scope)...),) -> $(tojulia(body, scope)))))

isa_jil_macro(val) =
  val isa JiLMacro  

jil_macroexpand(jilmacro, args, scope) =
  Base.invokelatest(jilmacro.expander, args...) # The macro was compiled in an older world


# Globals
tojulia(::Val{:global}, (form, ), scope) = 
  :(global $(tojulia(form, scope)))

# Modules
tojulia(::Val{:using}, (form, ), scope) =
  :(using $(tojulia(form, scope)))

# Blocks
tojulia(::Val{:begin}, forms, scope) =
  :(begin $(tojulia_vector(forms, scope)...) end)

remove_macro_bindings(names, inits) =
  let names_inits = [(name, init) for (name, init) in zip(names, inits) if !isa_jil_macro(init)], 
      let_names = map(t->t[1], names_inits),
      let_inits = map(t->t[2], names_inits)
    (let_names, let_inits)
  end

# Local scopes
tojulia(::Val{:let}, (binds, body...), scope) =
  let names = tojulia_tuple(map(head, binds), scope),
      inits = tojulia_tuple(map(head ∘ tail, binds), scope),
      scope = new_scope(scope, names, inits),
      (let_names, let_inits) = remove_macro_bindings(names, inits) # We don't need the macros in the generated Julia code.
    :(let ($(let_names...),) = ($(let_inits...),)
      $(tojulia_vector(body, scope)...)
      end)
  end

tojulia(::Val{Symbol("let*")}, (binds, body...), scope) =
  let tojulia_binds(binds, scope) = 
        isempty(binds) ?
          ([], scope) :
          let bind = head(binds),
              name = tojulia(head(bind), scope),
              init = tojulia(head(tail(bind)), scope),
              scope = new_scope(scope, [name], [init]),
              (julia_binds, scope) = tojulia_binds(tail(binds), scope)
            isa_jil_macro(init) ? # We don't need the macros in the generated Julia code.
              (julia_binds, scope) :
              ([:($(name) = $(init)), julia_binds], scope)
          end
    let (julia_binds, scope) = tojulia_binds(binds, scope)
      :(let $(julia_binds)
         $(tojulia_vector(body, scope)...)
        end)
    end
  end

# Fallback: everything is treated as a function/macro call.
tojulia(::Val{op}, args, scope) where {op} =
  op isa Symbol && String(op)[1] == '@' ? #julia macro
    Expr(:macrocall, tojulia(op, scope), LineNumberNode(1, :JiL), tojulia_vector(args, scope)...) :
    let init = get_binding(op, scope)
      init isa JiLMacro ?
        tojulia(jil_macroexpand(init, args, scope), scope) :
        Expr(:call, tojulia(op, scope), tojulia_vector(args, scope)...)
    end

#=
Here is one idea for macros:
 - use name mangling to define, in the current lexical scope, the macro as a regular function.
 - the lowering process keeps track of the scopes, so that the definition is visible during code generation.
 - whenever a call (of a symbol) is processed, check the scopes to see if the corresponding definition is a macro and expand accordingly.
=#

