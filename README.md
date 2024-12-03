# JiL

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://aptmcl.github.io/JiL.jl/stable/)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://aptmcl.github.io/JiL.jl/dev/)
[![Build Status](https://github.com/aptmcl/JiL.jl/actions/workflows/CI.yml/badge.svg?branch=master)](https://github.com/aptmcl/JiL.jl/actions/workflows/CI.yml?query=branch%3Amaster)
[![Coverage](https://codecov.io/gh/aptmcl/JiL.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/aptmcl/JiL.jl)

JiL means _JiL is Lisp_.

Jil is a Lisp syntax for Julia.

On the most basic level, JiL is about supporting a fully-parenthesized prefix syntax for Julia.

However, JiL is much more than just that. In the good tradition of Lisp, we plan to develop JiL on top of JiL, improving the syntax and supporting different programming approaches.

## Installation

Install with the Julia package manager [Pkg](https://pkgdocs.julialang.org/):

```jl
pkg> add JiL  # Press ']' to enter the Pkg REPL mode.
```
or
```jl
julia> using Pkg; Pkg.add("JiL")
```

## Usage

```jl
using JiL

# You can use Julia syntax
julia> 1 + 2 + 3
6

julia> 1 + 2*3^4
163

# But you can also use Lisp syntax
julia> (+ 1 2 3)
6

julia> (+ 1 (* 2 (^ 3 4)))
163

# You can define functions using Julia syntax
julia> foo(x, y, z) = x + y + z
foo (generic function with 1 method)

# But you can also define functions using Lisp syntax
julia> (def (foo x y z) (+ x y z))
foo (generic function with 1 method)

# You can call it using Julia syntax
julia> foo(1, 2, 3)
6

# But you can also call it using Lisp syntax
julia> (foo 1 2 3)
6

# Optional parameters
julia> (def (bar x (kw y 1)) (list x y))
bar (generic function with 2 methods)

julia> (bar 2 3)
(2 3)

julia> (bar 2)
(2 1)

# Vargars
julia> (def (mytuple (... args)) args)
mytuple (generic function with 1 method)

# Note: the arguments are collected in a tuple
julia> (mytuple 1 2 3)
(1, 2, 3)

# Mandatory and varargs
julia> (def (baz x (... others)) (list x others))
baz (generic function with 1 method)

# Note the result: a list of two elements, the second being a tuple
julia> (baz 1 2 3 4)
(1 (2, 3, 4))

# Splatting
julia> (def (mylist (... args)) (list (... args)))
mylist (generic function with 1 method)

julia> (mylist 1 2 3)
(1 2 3)

# Keyword parameters must come first
julia> (def (kkk (parameters (kw y 2) z (... others)) x) (list x y z others))
kkk (generic function with 1 method)

# Just like keyword arguments
julia> (kkk (kw z 3) (kw a 1) (kw b 2) 1)
(1 2 3 Base.Pairs(:a => 1, :b => 2))

# So far, this is a pretty annoying syntax. The good news is that we can also define macros, 
# so we can start improving it.
# For example, we can implement a Scheme-like define form:
(def define (macro (sig form (... extra_forms))
  (let ()
    (def (to-jil-sig sig)
      (if (isa sig Nil)
        (list)
        (if (isa (cdr sig) Symbol)
          (list (car sig) (list '... (cdr sig)))
          (cons (car sig) (to-jil-sig (cdr sig))))))
    (if (isa sig Symbol)
      (list 'def sig form)
      (list 'def (to-jil-sig sig)
            (if (isempty extra_forms)
              form
            (cons 'begin (cons form extra_forms))))))))

julia> (define my-name "António Menezes Leitão")
"António Menezes Leitão"

julia> (define (quux a b) (list a b))
quux (generic function with 1 method)

julia> (define (quux a . b) (list a b))
quux (generic function with 2 methods)

julia> (quux 1 2)
(1 2)

julia> (quux 1 2 3)
(1 (2, 3))

julia> (quux 1)
(1 ())
```