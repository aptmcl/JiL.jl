export List, Nil, Cons, list, cons, nil, head, tail

abstract type List{T} end

struct Nil{T} <: List{T} end

struct Cons{T} <: List{T}
  head::T
  tail::List{T}
end

Base.Pair(h::T, t::List{L}) where {T, L} = Cons{typejoin(T, L)}(h, t)

const nil = Nil{Union{}}()

cons(h::T, t::List{T}) where {T} = Cons{T}(h, t)

cons(h::T, t::List{L}) where {T, L} =
  let Type = typejoin(T, L)
    Cons{Type}(convert(Type, h), convert(List{Type}, t))
  end

(::Colon)(h::T, t::List{L}) where {T, L} = cons(h, t)

Base.convert(::Type{List{T}}, l::Nil{Union{}}) where {T} = Nil{T}()
Base.convert(::Type{List{T1}}, l::Nil{T2}) where {T1, T2 <: T1} = Nil{T1}()
Base.convert(::Type{List{T1}}, l::Cons{T2}) where {T1, T2 <: T1} = Cons{T1}(l.head, l.tail)
Base.convert(::Type{List}, t::Tuple) = list(t...)

list() = nil

Base.isempty(lst::Nil) = true
Base.isempty(lst::Cons) = false

Base.firstindex(lst::List{T}) where T = 1

Base.getindex(lst::Nil, i) = throw(BoundsError(lst, i))
Base.getindex(lst::Cons, i) = i == 1 ? lst.head : getindex(lst.tail, i-1)

Base.eltype(lst::List{T}) where T = T

list(elts...) =
  let l = nil
    for i = length(elts):-1:1
      l = cons(elts[i], l)
    end
    l
  end

list(gen::Base.Generator) =
  let iter(next) =
        isnothing(next) ?
          nil :
          let (e, state) = next
            cons(e, iter(iterate(gen, state)))
          end
    iter(iterate(gen))
  end


head(x::Cons{T}) where {T} = x.head::T
tail(x::Cons{T}) where {T} = x.tail

Base.first(x::Cons) = x.head
Base.Iterators.drop(x::Cons, n::Integer) = n == 0 ? x : drop(x.tail, n-1)
Base.iterate(l::List, ::Nil) = nothing
Base.iterate(l::List, state::Cons = l) = state.head, state.tail

import Base.==
==(x::Nil, y::Nil) = true
==(x::Cons, y::Cons) = (x.head == y.head) && (x.tail == y.tail)

print_list_type = false

Base.show(io::IO, lst::Nil) = 
  print_list_type ? 
    print(io, "List()") :
    print(io, "()")
Base.show(io::IO, lst::Cons{T}) where T =
  begin
    print_list_type ?
      print(io, "List{$T}(") :
      print(io, "(")
    show_maybe_symbol(io, head(lst))
    for e in tail(lst)
      #print(io, ", ")
      print(io, " ")
      show_maybe_symbol(io, e)
    end
    print(io, ")")
  end
show_maybe_symbol(io, e) =
  e isa Symbol ? 
    print(io, e) :
    show(io, e)

Base.length(l::Nil) = 0
Base.length(l::Cons) =
  let n = 0
    for i in l
      n += 1
    end
    n
  end

Base.last(l::Cons) =
  isempty(l.tail) ?
    l.head :
    last(l.tail)

Base.map(f::Base.Callable, lst::List) = list(f(e) for e in lst)
Base.filter(f::Function, lst::List) = list(e for e in lst if f(e))

# This amounts to type piracy, so let's avoid it.
#Base.cat() = list()
Base.cat(lst::List, lsts::List...) =
  let T = typeof(lst).parameters[1]
    n = length(lst)
    for l in lsts
      T2 = typeof(l).parameters[1]
      T = typejoin(T, T2)
      n += length(l)
    end
    elems = Vector{T}(undef, n)
    i = 1
    for e in lst
      elems[i] = e
      i += 1
    end
    for lst in lsts
      for e in lst
      elems[i] = e
      i += 1
      end
    end
    let l = nil(T)
      for i = i-1:-1:1
        l = cons(elems[i], l)
      end
      l
    end
  end

# Lists can be converted to Arrays
Base.convert(::Type{Array{S,1}}, l::List{T}) where {S, T <: S} = collect(T, l)
