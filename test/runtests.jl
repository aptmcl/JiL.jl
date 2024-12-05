using JiL
using Test

@testset "JiL.jl" begin
    @test jil"(def foo 1)" == julia"foo = 1"
    @test jil"(def (foo x) 1)" == julia"foo(x) = begin 1 end"
    @test jil"(def (foo x (... y)) 1)" == julia"foo(x, y...) = begin 1 end"
    @test jil"(def (foo x (kw y 2)) 1)" == julia"foo(x, y=2) = begin 1 end"
    @test jilm"(define foo 1)" == julia"foo = 1"
    @test jilm"(define (foo x) 1)" == julia"foo(x) = begin 1 end"
    @test jilm"(define (foo x . y) 1)".args[1] == julia"foo(x, y...) = begin 1 end".args[1]
    @test jilm"(define (fact n)
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
end
 