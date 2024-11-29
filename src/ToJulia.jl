tojulia(form::Any) = form

tojulia(form::Symbol) = 
  let token = string(form)
    '.' in token ?
      let (pkg, name) = split(token, '.')
        Expr(:., Symbol(pkg), QuoteNode(Symbol(name)))
      end :
      form
  end

tojulia(form::Nil) = form

tojulia(form::Cons) =
  let head = form.head,
      tail = form.tail
    if head isa Symbol
      tojulia(Val{head}(), tail)
    elseif head isa Cons
      Expr(:call, tojulia(head), tojulia_vector(tail)...)
    else
      error("Unknown form  $form")
    end
  end

tojulia_vector(val) =
  collect(Any, map(tojulia, val))

tojulia_tuple(val) =
  tuple(tojulia_vector(val)...)

tojulia(::Val{:quote}, (arg,)) =
  arg

tojulia(::Val{:(=)}, args) =
  :(==($(tojulia_vector(args)...)))

# Assignments
tojulia(::Val{:def}, (sig, init)) =
  if sig isa Symbol
    :($(tojulia(sig)) = $(tojulia(init)))
  elseif sig isa Cons
    tojulia(list(:def, head(sig), list(:lambda, tail(sig), init)))
  else
    error("Unknown form $(list(:def, sig, init))")
  end

# Conditional expressions
tojulia(::Val{:if}, (cond, conseq, altern)) =
  :($(tojulia(cond)) ? $(tojulia(conseq)) : $(tojulia(altern)))

# Anonymous functions
tojulia(::Val{:λ}, (params, body)) = 
  tojulia(Val{:lambda}(), (params, body))

tojulia(::Val{:lambda}, (params, body)) =
  :(($(tojulia_vector(params)...),) -> $(tojulia(body)))

# Globals
tojulia(::Val{:global}, (form, )) = 
  :(global $(tojulia(form)))

# Modules
tojulia(::Val{:using}, (form, )) =
  :(using $(tojulia(form)))

# Blocks
tojulia(::Val{:begin}, forms) =
  :(begin $(tojulia_vector(forms)...) end)

# Local scopes
tojulia(::Val{:let}, (binds, body...)) =
  :(let ($(tojulia_tuple(map(head, binds))...),) = ($(tojulia_tuple(map(head ∘ tail, binds))...),)
    $(tojulia_vector(body)...)
    end)

tojulia(::Val{Symbol("let*")}, (binds, body...)) =
  :(let $([:($(tojulia(head(bind))) = $(tojulia(head(tail(bind))))) for bind in binds]...)
    $(tojulia_vector(body)...)
    end)


# Fallback: everything is treated as a function call.
tojulia(::Val{op}, args) where {op} = 
  Expr(:call, tojulia(op), tojulia_vector(args)...)
  

