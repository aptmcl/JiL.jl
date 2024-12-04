```
JiL

```scheme

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
```

You can use Julia syntax

```jl
julia> 1 + 2 + 3
6

julia> 1 + 2*3^4
163
```
But you can also use Lisp syntax

```scheme
julia> (+ 1 2 3)
6

julia> (+ 1 (* 2 (^ 3 4)))
163

```
You can define functions using Julia syntax

```scheme
julia> foo(x, y, z) = x + y + z
foo (generic function with 1 method)
```
But you can also define functions using Lisp syntax

```scheme
julia> (def (foo x y z) (+ x y z))
foo (generic function with 1 method)
```
You can call it using Julia syntax
```scheme
julia> foo(1, 2, 3)
6
```
But you can also call it using Lisp syntax
```scheme
julia> (foo 1 2 3)
6
```
Function definition leverage all Julia capabilities, e.g.,
Optional parameters
```scheme
julia> (def (bar x (kw y 1)) (list x y))
bar (generic function with 2 methods)

julia> (bar 2 3)
(2 3)

julia> (bar 2)
(2 1)
```
Vargars
```scheme
julia> (def (mytuple (... args)) args)
mytuple (generic function with 1 method)
```
Note: the arguments are collected in a tuple
```scheme
julia> (mytuple 1 2 3)
(1, 2, 3)
```
Mandatory and varargs
```scheme
julia> (def (baz x (... others)) (tuple x others))
baz (generic function with 1 method)

julia> (baz 1 2 3 4)
(1, (2, 3, 4))
```
Splatting
```scheme
julia> (def (quux x (... args)) (tuple x (... args)))
quux (generic function with 1 method)

julia> (quux 1 2 3 4)
(1, 2, 3, 4)
```
Keyword parameters must come before mandatory
```scheme
julia> (def (foobar (parameters (kw y 2) z (... others)) x) (tuple x y z others))
foobar (generic function with 1 method)
```
The same applies to calls with keyword arguments
```scheme
julia> (foobar (kw z 3) (kw a 1) (kw b 2) 1)
(1, 2, 3, Base.Pairs(:a => 1, :b => 2))
```
Being a Lisp, lists are available
```scheme
julia> (def a-list (list 1 2 3))
(1 2 3)

julia> (car a-list)
1

julia> (cadr a-list)
2

julia> (null? a-list)
false

julia> (null? (cdddr a-list))
true
```
Non-traditionalists might prefer modern equivalents to car, cdr, and null?, e.g.,
head, tail, empty?
```scheme
julia> (def (append l1 l2)
         (if (empty? l1)
           l2
           (cons (head l1)
                 (append (tail l1) l2))))
append (generic function with 1 method)

julia> (append (list 1 2 3) (list 4 5 6))
(1 2 3 4 5 6)
```
BTW, Julia macros are available
```scheme
julia> (@time (append (list 1 2 3) (list 4 5 6)))
  0.000008 seconds (12 allocations: 384 bytes)
(1 2 3 4 5 6)
```
Being a Lisp, pairs are also available
```scheme
julia> (def a-pair (cons 1 2))
(1 . 2)

julia> (car a-pair)
1

julia> (cdr a-pair)
2
```
Differently from traditional Lisps, lists are not made of pairs.
```scheme
julia> (cons 1 (cons 2 (cons 3 nil)))
(1 2 3)

julia> (cons 1 (cons 2 3))
(1 . (2 . 3))
```
However, list can symbolically represent non-proper lists through the use of the splat symbol ...
```scheme
julia> (as-list (cons 1 (cons 2 (cons 3 (list)))))
(1 2 3)

julia> (as-list (cons 1 (cons 2 3)))
(1 2 (... 3))
```
BTW, tuples can also be converted to lists
```scheme
julia> (as-list (tuple 1 2 3))
(1 2 3)
```
We can also define macros.
Note that these are not Julia macros, they are JiL macros. 
As an example, consider the form (when <condition> <action>)
```scheme
julia> (def when
         (macro (condition action)
           (list (quote if) condition action (quote nothing))))
(macro (condition action) (list (quote if) condition action (quote nothing)))
```
Let's test it
```scheme
julia> (when (= (+ 1 1) 3)
         (println "Good math"))

julia> (when (= (+ 1 1) 2)
         (println "Good math"))
Good math
```
Let's define a more sophisticated macro: cond
cond takes an arbitrary number of clauses, so we need varargs.
Here, it is important to remember that varargs are tuples.
If we prefer to process them as lists, we need to convert them first.
```scheme
julia> (def cond
         (macro ((... clauses))
           (let ((clauses (as-list clauses)))
             (if (null? clauses)
               'false
               (list 'if (if (eq? (caar clauses) 'else) 'true (caar clauses))
                     (cons 'begin (cdar clauses))
                           (cons 'cond (cdr clauses)))))))
```
Here is an example:
```scheme
julia> (def (sign x)
         (cond ((> x 0) +1)
               ((< x 0) -1)
               (else 0)))
sign (generic function with 1 method)

julia> (sign (sin (/ pi 2)))
1

julia> (sign (sin (* 3 (/ pi 2))))
-1

julia> (sign (sin pi))
0
```
BTW, Julia types can be referenced
```scheme
julia> (def (explain arg)
         (cond ((isa arg String) "a string")
               ((isa arg Int) "an integer")
               (else "something else")))
explain (generic function with 1 method)

julia> (explain "Hi")
"a string"

julia> (explain 123)
"an integer"

julia> (explain 1.23)
"something else"
```
Macro entusiasts might like to know that quasiquotation
is available through the traditional characters ` and , and ,@.
This simplifies the generalization of the when macro to 
accept any number of actions.
```scheme
julia> (def when
         (macro (condition (... actions))
           (let ((actions (as-list actions)))
             `(if ,condition 
                (begin ,@actions)
                nothing))))

julia> (when (= (+ 1 1) 3)
         (println "Good math")
         (println "You deserve a prize"))

julia> (when (= (+ 1 1) 2)
         (println "Good math")
         (println "You deserve a prize"))
Good math
You deserve a prize
```
Just like any other Lisp, JiL allows manual macroexpanding for
debugging purposes
```scheme
julia> (macroexpand (when (= (+ 1 1) 3)
                      (println "Good math")
                      (println "You deserve a prize")))
(if (= (+ 1 1) 3)
  (begin
    (println "Good math")
    (println "You deserve a prize"))
  nothing)
```
Scheme tradionalists might prefer to specify varargs 
using the dot notation. Clearly, we can convert from dot notation
to varargs using the as-list function but, as before, the actual 
varargs will be collected in a tuple which we will have to convert
into a list. We can abstract that process with a macro-defining 
macro for Scheme's define.
We start by defining two functions, one to maybe wrap multiple
forms in a begin expression.
```scheme
(def (maybe-wrap-in-begin form extra-forms)
  (if (isempty extra-forms)
    form
    `(begin ,form ,@extra-forms)))
```
Next, we define a function to possibly convert the last vararg
parameter into a list and wrap the body with a new scope for
the converted parameter.
```scheme

(def (maybe-convert-last-to-list params body)
  (if (null? params)
    body
    (let ((param (last params)))
      (if (and (pair? param) (eq? (car param) '...))
        (let ((param-name (cadr param)))
          `(let ((,param-name (as-list ,param-name)))
             ,body))
           body))))
```
Now, we can define the define macro.
```scheme

(def define 
  (macro (sig form (... extra-forms))
    (let ((body (maybe-wrap-in-begin form (as-list extra-forms))))
      (if (symbol? sig)
        `(def ,sig ,body)
        (let ((name (car sig))
              (params (as-list (cdr sig))))
          `(def (,name ,@params)
             ,(maybe-convert-last-to-list params body)))))))
```
Here are a few examples of use.
```scheme
julia> (define answer 42)
42

julia> (define (mystery a b) (list a b))
mystery (generic function with 1 method)

julia> (define (mystery a . b) (list a b))
mystery (generic function with 2 methods)

julia> (mystery 1 2)
(1 2)

julia> (mystery 1 2 3)
(1 (2 3))

julia> (mystery 1)
(1 ())
```
We can apply the same principles to provide a simplified 
form of macro definition
```scheme

(def defmacro 
  (macro (sig form (... extra-forms))
    (let ((name (car sig))
          (params (as-list (cdr sig)))
          (body (maybe-wrap-in-begin form (as-list extra-forms))))
      `(def ,name 
         (macro ,params
           ,(maybe-convert-last-to-list params body))))))
```
Using this form we can further simplify the definition of
the when macro.
```scheme

julia> (defmacro (when condition . actions)
         `(if ,condition 
            (begin ,@actions)
            nothing))```