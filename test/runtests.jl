using JiL
using Test
using JiL: truncate, round, floor

@testset "JiL.jl" begin
    @test jilEval"(eqv? 'a 'a)" == true
    @test jilEval"(eqv? 'a 'b)" == false
    @test jilEval"(eqv? 2 2)" == true
    @test jilEval"(eqv? 2 2.0)" == false
    @test jilEval"(eqv? '() '())" == true
    @test jilEval"(eqv? 100000000 100000000)" == true
    @test jilEval"(eqv? 0.0 +nan.0)" == false
    #@test jilEval"(eqv? (cons 1 2) (cons 1 2))" == false
    @test jilEval"(eqv? (lambda () 1) (lambda () 2))" == false
    @test jilEval"(let ((p (lambda (x) x))) (eqv? p p))" == true
    @test jilEval"(eqv? #f 'nil)" == false
    @test jil"(def foo 1)" == julia"foo = 1"
    @test jil"(def (foo x) 1)" == julia"foo(x) = begin 1 end"
    @test jil"(def (foo x (... y)) 1)" == julia"foo(x, y...) = begin 1 end"
    @test jil"(def (foo x (kw y 2)) 1)" == julia"foo(x, y=2) = begin 1 end"
    @test jil"(define foo 1)" == julia"foo = 1"
    @test jil"(define (foo x) 1)" == julia"foo(x) = begin 1 end"
    @test jil"(define (foo x . y) 1)".args[1] == julia"foo(x, y...) = begin 1 end".args[1]
    @test jil"(define (fact n)
                (if (= n 0)
                  1 
                  (* n (fact (- n 1)))))" ==
        julia"fact(n) = begin
                          if n == 0
                              1
                          else
                              n * fact(n - 1)
                          end
                        end"
    @test jil"(def (quux x (... args)) (tuple x (... args)))" ==
        julia"quux(x, args...) = begin
                tuple(x, args...)
             end"

    @test jilEval"(define x 28)" == 28
    @test jilEval"(+ 3 4)" == 7
    @test jilEval"((if false + *) 3 4)" == 12
    @test jilEval"((lambda (x) (+ x x)) 4)" == 8
    @test jilEval"(begin
                    (define reverse-subtract
                      (lambda (x y) (- y x)))
                    (reverse-subtract 7 10))" == 3
    @test jilEval"(begin
                    (define add4
                      (let ((x 4))
                         (lambda (y) (+ x y))))
                    (add4 6))" == 10
    @test jilEval"((lambda x x) 3 4 5 6)" == list(3, 4, 5, 6)
    @test jilEval"((lambda (x y . z) z) 3 4 5 6)" == list(5, 6)
    @test jilEval"(if (> 3 2) 'yes 'no)" == :yes
    @test jilEval"(if (> 2 3) 'yes 'no)" == :no
    @test jilEval"(if (> 3 2)
                     (- 3 2)
                     (+ 3 2))" == 1
    @test jilEval"(begin (define x 2) (+ x 1))" == 3
    @test jilEval"(set! x 4)" == 4
    @test jilEval"(+ x 1)" == 5
    @test jilEval"(cond ((> 3 2) 'greater) ((< 3 2) 'less))" == :greater
    @test jilEval"(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))" == :equal
    @test jilEval"(cond ((assv 'b '((a 1) (b 2))) => cadr) (else false))" == 2
    @test jilEval"(case (* 2 3)
                    ((2 3 5 7) 'prime)
                    ((1 4 6 8 9) 'composite))" == :composite
    @test jilEval"(case (car '(c d))
                    ((a) 'a)
                    ((b) 'b))" == false
    @test jilEval"(case (car '(c d))
                    ((a e i o u) 'vowel)
                    ((w y) 'semivowel)
                    (else => (lambda (x) x)))" == :c
    @test jilEval"(or (= 2 2) (> 2 1))" == true
    @test jilEval"(or (= 2 2) (< 2 1))" == true
    @test jilEval"(or false false false)" == false
    #@test jilEval"(or (memq 'b '(a b c)) (/ 3 0))" == list(:b, :c) TypeError: non-boolean (Cons{Symbol}) used in boolean context
    @test jilEval"(let ((x 2) (y 3)) (* x y))" == 6
    @test jilEval"(let ((x 2) (y 3))
                    (let ((x 7)
                          (z (+ x y)))
                      (* z x)))" == 35
    @test jilEval"(let ((x 2) (y 3))
                    (let* ((x 7)
                           (z (+ x y)))
                      (* z x)))" == 70
    @test jilEval"(letrec ((even?
                             (lambda (n)
                               (if (zero? n)
                                 true
                                 (odd? (- n 1)))))
                           (odd?
                             (lambda (n)
                               (if (zero? n)
                                 false
                                 (even? (- n 1))))))
                    (even? 88))" == true
    @test jilEval"(letrec* ((p
                              (lambda (x)
                                (+ 1 (q (- x 1)))))
                            (q
                              (lambda (y)
                                (if (zero? y)
                                  0
                                  (+ 1 (p (- y 1))))))
                            (x (p 5))
                            (y x))
                    y)" == 5
    @test jilEval"(letrec* ((a 42)
                            (b (+ a 10)))
                    (* a b))" == 2184
    @test jilEval"(let-values (((root rem) (exact-integer-sqrt 32)))
                    (* root rem))" == 35

    @test jilEval"(let ((a 'a) (b 'b) (x 'x) (y 'y))
                    (let*-values (((a b) (values x y))
                                  ((x y) (values a b)))
                      (list a b x y)))" == jilEval"'(x y x y)"              
    jilEval"(define x 0)"
    @test jilEval"(and (= x 0)
                       (begin (set! x 5)
                              (+ x 1)))" == 6
    @test jilEval"(do ((vec (make-vector 5))
                       (i 0 (+ i 1)))
                       ((= i 5) vec)
                       (vector-set! vec i i))" == jilEval"#(0 1 2 3 4)"
    @test jilEval"(let ((x '(1 3 5 7 9)))
                    (do ((x x (cdr x))
                         (sum 0 (+ sum (car x))))
                      ((null? x) sum)))" == 25
    @test jilEval"(let loop ((numbers '(3 -2 1 6 -5))
                             (nonneg '())
                             (neg '()))
                    (cond ((null? numbers) (list nonneg neg))
                          ((>= (car numbers) 0)
                           (loop (cdr numbers)
                                 (cons (car numbers) nonneg)
                                 neg))
                          ((< (car numbers) 0)
                           (loop (cdr numbers)
                                 nonneg
                                 (cons (car numbers) neg)))))" == jilEval"'((6 1 3) (-5 -2))"

    @test jilEval"(force (delay (+ 1 2)))" == 3
    @test jilEval"(let ((p (delay (+ 1 2))))
                    (list (force p) (force p)))" == jilEval"'(3 3)"

    @test jilEval"(begin
                    (define integers
                      (letrec ((next
                                 (lambda (n)
                                   (delay (cons n (next (+ n 1)))))))
                        (next 0)))
                    (define lhead
                      (lambda (stream) (car (force stream))))
                    (define ltail
                      (lambda (stream) (cdr (force stream))))
                    (lhead (ltail (ltail integers))))" == 2
    @test jilEval"(begin
                    (define (stream-filter p? s)
                      (delay-force
                        (if (null? (force s))
                          (delay '())
                          (let ((h (car (force s)))
                                (t (cdr (force s))))
                            (if (p? h)
                              (delay (cons h (stream-filter p? t)))
                              (stream-filter p? t))))))
                    (lhead (ltail (ltail (stream-filter odd? integers)))))" == 5

    jilEval"(begin
              (define count 0)
              (define p
                (delay 
                  (begin
                    (set! count (+ count 1))
                    (if (> count x)
                      count
                      (force p)))))
              (define x 5)
              p)"
    @test jilEval"(force p)" == 6
    @test jilEval"(isa p promise)"
    @test jilEval"(begin
                    (set! x 10)
                    (force p))" == 6
# (define radix
# (make-parameter
# 10
# (lambda (x)
# (if (and (exact-integer? x) (<= 2 x 16))
# x
# (error "invalid radix")))))
# (define (f n) (number->string n (radix)))
# (f 12)" == "12"
# (parameterize ((radix 2))
# (f 12))" == "1100"
# (f 12)" == "12"
# (radix 16)" == unspecified
# 
# 
# (parameterize ((radix 0))
# (f 12)) 
# 
#     @test jilEval"(guard (condition
# ((assq 'a condition) => cdr)
# ((assq 'b condition)))
# (raise (list (cons 'a 42))))" == 42
#     @test jilEval"(guard (condition
# ((assq 'a condition) => cdr)
# ((assq 'b condition)))
# (raise (list (cons 'b 23))))" == (b . 23)
    @test jilEval"`(list ,(+ 1 2) 4)" == jilEval"'(list 3 4)"
    @test jilEval"(let ((name 'a)) `(list ,name ',name))" == jilEval"'(list a (quote a))"
    @test jilEval"`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))" == jilEval"'(a 3 4 5 6 b)"
    @test jilEval"`((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))" == jilEval"'((foo 7) . cons)"
    #@test jilEval"`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)" == jilEval"'#(10 5 2 4 3 8)" Backquote needs to handle vectors
    @test jilEval"(let ((foo '(foo bar)) (@baz 'baz))
                    `(list ,@foo , @baz))" == jilEval"'(list foo bar baz)"
    #@test jilEval"`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)" == jilEval"'(a `(b ,(+ 1 2) ,(foo 4 d) e) f)" Backquote needs to be improved to handle double backquote
    #@test jilEval"(let ((name1 'x)  Backquote needs to be improved to handle double backquote
    #                    (name2 'y))
    #                `(a `(b ,,name1 ,',name2 d) e))" == jilEval"'(a `(b ,x ,'y d) e)"
    @test jilEval"(quasiquote (list (unquote (+ 1 2)) 4))" == jilEval"'(list 3 4)"
    @test jilEval"'(quasiquote (list (unquote (+ 1 2)) 4))" == jilEval"'`(list ,(+ 1 2) 4)"
    @test jilEval"(let ((x 5))
                    (define foo (lambda (y) (bar x y)))
                    (define bar (lambda (a b) (+ (* a b) a)))
                    (foo (+ x 3)))" == 45

    @test jilEval"(begin (define-values (x y) (exact-integer-sqrt 17))
                         (list x y))" == jilEval"'(4 1)"
    @test jilEval"(let ()
                    (define-values (x y) (values 1 2))
                    (+ x y))" == 3
    @test jilEval"(begin
                    (define gen-counter
                      (lambda ()
                        (let ((n 0))
                          (lambda () (set! n (+ n 1)) n))))
                  (let ((g (gen-counter)))
                    (eqv? g g)))" == true
    @test jilEval"(eqv? (gen-counter) (gen-counter))" == false
    @test jilEval"(begin
                    (define gen-loser
                      (lambda ()
                        (let ((n 0))
                          (lambda () (set! n (+ n 1)) 27))))
                    (let ((g (gen-loser)))
                      (eqv? g g)))" == true
    @test jilEval"(eqv? (gen-loser) (gen-loser))" == false # unspecified
    @test jilEval"(letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
                           (g (lambda () (if (eqv? f g) 'both 'g))))
                    (eqv? f g))" == false # unspecified
    @test jilEval"(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
                           (g (lambda () (if (eqv? f g) 'g 'both))))
                    (eqv? f g))" == false
    @test jilEval"(eqv? '(a) '(a))" == true # unspecified
    @test jilEval"""(eqv? "a" "a")""" == true # unspecified
    @test jilEval"(eqv? '(b) (cdr '(a b)))" == true # unspecified
    @test jilEval"(let ((x '(a))) (eqv? x x))" == true
    @test jilEval"(eq? 'a 'a)" == true
    @test jilEval"(eq? '(a) '(a))" == true # unspecified
    @test jilEval"(eq? (list 'a) (list 'a))" == true # === on immutables work by comparing bit representations, not memory addresses
    @test jilEval"""(eq? "a" "a")""" == true # unspecified
    @test jilEval"""(eq? "" "")""" == true # unspecified
    @test jilEval"(eq? '() '())" == true
    @test jilEval"(eq? 2 2)" == true # unspecified
    @test jilEval"(eq? #\A #\A)" == true # unspecified
    @test jilEval"(eq? car car)" == true
    @test jilEval"(let ((n (+ 2 3)))
                    (eq? n n))" == true # unspecified
    @test jilEval"(let ((x '(a)))
                    (eq? x x))" == true
    @test jilEval"(let ((x '#()))
                    (eq? x x))" == true
    @test jilEval"(let ((p (lambda (x) x)))
                    (eq? p p))" == true
    @test jilEval"(equal? 'a 'a)" == true
    @test jilEval"(equal? '(a) '(a))" == true
    @test jilEval"(equal? '(a (b) c) '(a (b) c))" == true
    @test jilEval"""(equal? "abc" "abc")""" == true
    @test jilEval"(equal? 2 2)" == true
    @test jilEval"(equal? (make-vector 5 'a) (make-vector 5 'a))" == true
    #@test jilEval"(equal? '#1=(a b . #1#) '#2=(a b a b . #2#))" == true JiL does not support circular lists.
    @test jilEval"(equal? (lambda (x) x) (lambda (y) y))" == false # unspecified
    #@test jilEval"(complex? 3+4i)" == true
    @test jilEval"(complex? 3)" == true
    @test jilEval"(real? 3)" == true
    #@test jilEval"(real? -2.5+0i)" == true
    #@test jilEval"(real? -2.5+0.0i)" == false
    @test jilEval"(real? #e1e10)" == true
    @test jilEval"(real? +inf.0)" == true
    @test jilEval"(real? +nan.0)" == true
    @test jilEval"(rational? -inf.0)" == false
    #@test jilEval"(rational? 3.5)" == true 
    @test jilEval"(rational? 6/10)" == true
    @test jilEval"(rational? 6/3)" == true
    #@test jilEval"(integer? 3+0i)" == true
    @test jilEval"(integer? 3.0)" == true
    @test jilEval"(integer? 8/4)" == true
    @test jilEval"(exact? 3.0)" == false
    @test jilEval"(exact? #e3.0)" == true
    @test jilEval"(inexact? 3.)" == true
    @test jilEval"(exact-integer? 32)" == true
    @test jilEval"(exact-integer? 32.0)" == false
    @test jilEval"(exact-integer? 32/5)" == false
    @test jilEval"(finite? 3)" == true
    @test jilEval"(finite? +inf.0)" == false
    #@test jilEval"(finite? 3.0+inf.0i)" == false
    @test jilEval"(infinite? 3)" == false
    @test jilEval"(infinite? +inf.0)" == true
    @test jilEval"(infinite? +nan.0)" == false
    #@test jilEval"(infinite? 3.0+inf.0i)" == true
    @test jilEval"(nan? +nan.0)" == true
    @test jilEval"(nan? 32)" == false
    #@test jilEval"(nan? +nan.0+5.0i)" == true
    #@test jilEval"(nan? 1+2i)" == false
    @test jilEval"(max 3 4)" == 4 # exact
    @test jilEval"(max 3.9 4)" == 4.0 # inexact
    @test jilEval"(+ 3 4)" == 7
    @test jilEval"(+ 3)" == 3
    # @test jilEval"(+)" == 0 Not possible, as the return type depends on the type of argument
    @test jilEval"(* 4)" == 4
    # @test jilEval"(*)" == 1 Not possible, as the return type depends on the type of argument
    @test jilEval"(- 3 4)" == -1
    #@test jilEval"(- 3 4 5)" == -6 Not generalized
    @test jilEval"(- 3)" == -3
    #@test jilEval"(/ 3 4 5)" == 3/20 Not generalized neither computes fractions from integers
    #@test jilEval"(/ 3)" == 1/3
    @test jilEval"(floor/ 5 2)" == (2, 1)
    @test jilEval"(floor/ -5 2)" == (-3, 1)
    @test jilEval"(floor/ 5 -2)" == (-3, -1)
    @test jilEval"(floor/ -5 -2)" == (2, -1)
    @test jilEval"(truncate/ 5 2)" == (2, 1)
    @test jilEval"(truncate/ -5 2)" == (-2, -1)
    @test jilEval"(truncate/ 5 -2)" == (-2, 1)
    @test jilEval"(truncate/ -5 -2)" == (2, -1)
    @test jilEval"(truncate/ -5.0 -2)" == (2.0, -1.0)
    @test jilEval"(gcd 32 -36)" == 4
    #@test jilEval"(gcd)" == 0 Not generalized
    @test jilEval"(lcm 32 -36)" == 288
    #@test jilEval"(lcm 32.0 -36)" == 288.0 # inexact
    #@test jilEval"(lcm)" == 1 Not generalized
    #@test jilEval"(numerator (/ 6 4))" == 3 Only applicable to Rational
    #@test jilEval"(denominator (/ 6 4))" == 2 Only applicable to Rational
    #@test jilEval"(denominator (inexact (/ 6 4)))" == 2.0 Only applicable to Rational
    @test jilEval"(floor -4.3)" == -5.0
    @test jilEval"(ceiling -4.3)" == -4.0
    @test jilEval"(truncate -4.3)" == -4.0
    @test jilEval"(round -4.3)" == -4.0
    @test jilEval"(floor 3.5)" == 3.0
    @test jilEval"(ceiling 3.5)" == 4.0
    @test jilEval"(truncate 3.5)" == 3.0
    @test jilEval"(round 3.5)" == 4.0 # inexact
    @test jilEval"(round 7/2)" == 4 # exact
    @test jilEval"(round 7)" == 7
    #@test jilEval"(rationalize (exact .3) 1/10)" == 1/3 # exact
    #@test jilEval"(rationalize .3 1/10)" == #i1/3 # inexact
    @test jilEval"(square 42)" == 1764
    @test jilEval"(square 2.0)" == 4.0
    @test jilEval"(sqrt 9)" == 3
    #@test jilEval"(sqrt -1)" == +i
    @test jilEval"(exact-integer-sqrt 4)" == (2, 0)
    @test jilEval"(exact-integer-sqrt 5)" == (2, 1)
    @test jilEval"""(string->number "100")""" == 100
    @test jilEval"""(string->number "100" 16)""" == 256
    @test jilEval"""(string->number "1e2")""" == 100.0
    @test jilEval"true" == true
    @test jilEval"false" == false
    @test jilEval"'false" == false
    @test jilEval"(not true)" == false
    #@test jilEval"(not 3)" == false
    #@test jilEval"(not (list 3))" == false
    @test jilEval"(not false)" == true
    #@test jilEval"(not '())" == false
    #@test jilEval"(not (list))" == false
    #@test jilEval"(not 'nil)" == false
    @test jilEval"(boolean? false)" == true
    @test jilEval"(boolean? 0)" == false
    @test jilEval"(boolean? '())" == false
    @test jilEval"(begin (define x (list 'a 'b 'c)) (define y x) y)" == jilEval"'(a b c)"
    @test jilEval"(list? y)" == true
    #@test jilEval"(set-cdr! x 4)" == unspecified

    @test jilEval"(pair? '(a . b))" == true
    @test jilEval"(pair? '(a b c))" == true
    @test jilEval"(pair? '())" == false
    @test jilEval"(pair? '#(a b))" == false

    @test jilEval"(cons 'a '())" == jilEval"'(a)"
    @test jilEval"(cons '(a) '(b c d))" == jilEval"'((a) b c d)"
    @test jilEval"""(cons "a" '(b c))""" == jilEval"""'("a" b c)"""
    @test jilEval"(cons 'a 3)" == jilEval"'(a . 3)"
    @test jilEval"(cons '(a b) 'c)" == jilEval"'((a b) . c)"

    @test jilEval"(car '(a b c))" == jilEval"'a"
    @test jilEval"(car '((a) b c d))" == jilEval"'(a)"
    @test jilEval"(car '(1 . 2))" == 1
    @test_throws "applied to an empty list" jilEval"(car '())"

    @test jilEval"(cdr '((a) b c d))" == jilEval"'(b c d)"
    @test jilEval"(cdr '(1 . 2))" == 2
    @test_throws "applied to an empty list" jilEval"(cdr '())"

    @test jilEval"(list? '(a b c))" == true
    @test jilEval"(list? '())" == true
    @test jilEval"(list? '(a . b))" == false
    #@test jilEval"(let ((x (list 'a))) (set-cdr! x x) (list? x))" == false

    @test jilEval"(list 'a (+ 3 4) 'c)" == jilEval"'(a 7 c)"
    @test jilEval"(list)" == jilEval"'()"

    @test jilEval"(length '(a b c))" == 3
    @test jilEval"(length '(a (b) (c d e)))" == 3
    @test jilEval"(length '())" == 0

    @test jilEval"(append '(x) '(y))" == jilEval"'(x y)"
    @test jilEval"(append '(a) '(b c d))" == jilEval"'(a b c d)"
    @test jilEval"(append '(a (b)) '((c)))" == jilEval"'(a (b) (c))"
    @test jilEval"(append '(a b) '(c . d))" == jilEval"'(a b c . d)"
    @test jilEval"(append '() 'a)" == jilEval"'a"

    @test jilEval"(reverse '(a b c))" == jilEval"'(c b a)"
    @test jilEval"(reverse '(a (b c) d (e (f))))" == jilEval"'((e (f)) d (b c) a)"
    @test jilEval"(list-ref '(a b c d) 2)" == jilEval"'c"
    @test jilEval"(list-ref '(a b c d) (exact (round 1.8)))" == jilEval"'c"
    #@test jilEval"(let ((ls (list 'one 'two 'five!))) No mutable lists
    #                (list-set! ls 2 'three)
    #                ls)" == jilEval"'(one two three)"

    @test jilEval"(memq 'a '(a b c))" == jilEval"'(a b c)"
    @test jilEval"(memq 'b '(a b c))" == jilEval"'(b c)"
    @test jilEval"(memq 'a '(b c d))" == false
    # @test jilEval"(memq (list 'a) '(b (a) c))" == false
    @test jilEval"(member (list 'a) '(b (a) c))" == jilEval"'((a) c)"
    @test jilEval"""(member "B" '("a" "b" "c") string-ci=?)""" == jilEval"""'("b" "c")"""
    @test jilEval"(memq 101 '(100 101 102))" == jilEval"'(101 102)"
    @test jilEval"(memv 101 '(100 101 102))" == jilEval"'(101 102)"

    jilEval"(define e '((a 1) (b 2) (c 3)))"
    @test jilEval"(assq 'a e)" == jilEval"'(a 1)"
    @test jilEval"(assq 'b e)" == jilEval"'(b 2)"
    @test jilEval"(assq 'd e)" == false
    #@test jilEval"(assq (list 'a) '(((a)) ((b)) ((c))))" == false
    @test jilEval"(assoc (list 'a) '(((a)) ((b)) ((c))))" == jilEval"'((a))"
    @test jilEval"(assoc 2.0 '((1 1) (2 4) (3 9)) =)" == jilEval"'(2 4)"
    @test jilEval"(assq 5 '((2 3) (5 7) (11 13)))" == jilEval"'(5 7)"
    @test jilEval"(assv 5 '((2 3) (5 7) (11 13)))" == jilEval"'(5 7)"

    @test jilEval"(symbol? 'foo)" == true
    @test jilEval"(symbol? (car '(a b)))" == true
    @test jilEval"""(symbol? "bar")""" == false
    @test jilEval"(symbol? 'nil)" == true
    @test jilEval"(symbol? '())" == false
    @test jilEval"(symbol? false)" == false
    @test jilEval"""(string->symbol "mISSISSIppi")""" == jilEval"'mISSISSIppi"
    @test jilEval"""(eqv? 'bitBlt (string->symbol "bitBlt"))""" == true
    @test jilEval"(eqv? 'LollyPop (string->symbol (symbol->string 'LollyPop)))" == true
    @test jilEval"""(string=? "K. Harper, M.D." (symbol->string (string->symbol "K. Harper, M.D.")))""" == true
    # TO BE DONE
    #@test jilEval"(digit-value #\3)" == 3
    #@test jilEval"(digit-value #\x0664)" == 4
    #@test jilEval"(digit-value #\x0AE6)" == 0
    #@test jilEval"(digit-value #\x0EA6)" == false

    @test jilEval"(apply + (list 3 4))" == 7
    jilEval"(define compose (lambda (f g) (lambda args (f (apply g args)))))"
    @test jilEval"((compose sqrt *) 12 75)" == 30
    @test jilEval"(map cadr '((a b) (d e) (g h)))" == jilEval"'(b e h)"
    @test jilEval"(map (lambda (n) (expt n n)) '(1 2 3 4 5))" == jilEval"'(1 4 27 256 3125)"
    @test jilEval"(map + '(1 2 3) '(4 5 6 7))" == jilEval"'(5 7 9)"
    @test jilEval"(let ((count 0))
                    (map (lambda (ignored)
                           (set! count (+ count 1))
                    count)
                  '(a b)))" == jilEval"'(1 2)"

    @test jilEval"(vector 'a 'b 'c)" == jil"#(a b c)"
    @test jilEval"(vector-ref '#(1 1 2 3 5 8 13 21) 5)" == 8
    @test jilEval"(vector-ref '#(1 1 2 3 5 8 13 21) (exact (round (* 2 (acos -1)))))" == 13
    @test jilEval"""(let ((vec (vector 0 '(2 2 2 2) "Anna")))
                    (vector-set! vec 1 '("Sue" "Sue"))
                    vec)""" == jilEval"""#(0 ("Sue" "Sue") "Anna")"""
    #@test jilEval"""(vector-set! '#(0 1 2) 1 "doe")""" == error # Not because of the immutability but type-checking, instead.
    @test jilEval"(vector->list '#(dah dah didah))" == jilEval"'(dah dah didah)"
    @test jilEval"(vector->list '#(dah dah didah) 1 2)" == jilEval"'(dah)"
    @test jilEval"(list->vector '(dididit dah))" == jilEval"'#(dididit dah)"

    @test jilEval"""(string->vector "ABC")""" == jilEval"'#(#\A #\B #\C)"
    @test jilEval"(vector->string #(#\1 #\2 #\3))" == "123"

end
#=



=#
#=
#=
(define range
(case-lambda
((e) (range 0 e))
((b e) (do ((r '() (cons e r))
(e (- e 1) (- e 1)))
((< e b) r)))))
(range 3)" == (0 1 2)
(range 3 5)" == (3 4)

(define add3
(lambda (x) (+ x 3)))
(add3 3)" == 6
(define first car)
(first '(1 2))" == 1
=#
#=
(define-record-type <pare>
(kons x y)
pare?
(x kar set-kar!)
(y kdr))

    @test jilEval"(pare? (kons 1 2))" == true
    @test jilEval"(pare? (cons 1 2))" == false
    @test jilEval"(kar (kons 1 2))" == 1
    @test jilEval"(kdr (kons 1 2))" == 2
    @test jilEval"(let ((k (kons 1 2)))
(set-kar! k 3)
(kar k))" == 3
=#

#=
x" == (a . 4)
(eqv? x y)" == true
y" == (a . 4)
(list? y)" == false
(set-cdr! x x)" == unspecified
(list? x)" == false
=#
#=
(list-set! '(0 1 2) 1 "oops")" == error ; constant list
=#
#=

(define a '(1 8 2 8)) ; a may be immutable
(define b (list-copy a))
(set-car! b 3) ; b is mutable
b" == (3 8 2 8)
a" == (1 8 2 8)
=#
#=
    @test jilEval"(define (f) (make-string 3 #\*))
    @test jilEval"(define (g) "***")
    @test jilEval"(string-set! (f) 0 #\?)" == unspecified
    @test jilEval"(string-set! (g) 0 #\?)" == error
    @test jilEval"(string-set! (symbol->string 'immutable)
0
#\?)" == error

(define a "12345")
(define b (string-copy "abcde"))
(string-copy! b 1 a 0 2)
b" == "a12de"
=#
#=
(define a #(1 8 2 8)) ; a may be immutable
(define b (vector-copy a))
(vector-set! b 0 3) ; b is mutable
b" == #(3 8 2 8)
(define c (vector-copy b 1 3))
c" == #(8 2)
=#
#=
=#
end
=#
