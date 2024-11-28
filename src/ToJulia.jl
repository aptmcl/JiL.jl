tojulia(form::Any) = form

tojulia(form::Symbol) = form

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

tojulia(::Val{:def}, (sig, init)) =
  if sig isa Symbol
    :($(tojulia(sig)) = $(tojulia(init)))
  elseif sig isa Cons
    tojulia(list(:def, head(sig), list(:lambda, tail(sig), init)))
  else
    error("Unknown form $(list(:def, sig, init))")
  end

tojulia(::Val{:if}, (cond, conseq, altern)) =
  :($(tojulia(cond)) ? $(tojulia(conseq)) : $(tojulia(altern)))

tojulia(::Val{:λ}, (params, body)) = 
  tojulia(Val{:lambda}(), (params, body))

tojulia(::Val{:lambda}, (params, body)) =
    :(($(tojulia_vector(params)...),) -> $(tojulia(body)))



# Fallback: everything is treated as a function call.
tojulia(::Val{op}, args) where {op} = 
  Expr(:call, op, tojulia_vector(args)...)
  

