using JiL
using Test


@testset "JiL.jl" begin
    @test jil"(def foo 1)" == julia"foo = 1"
    @test jil"(def (foo x) 1)" == julia"foo(x) = begin 1 end"
    @test jil"(def (foo x (... y)) 1)" == julia"foo(x, y...) = begin 1 end"
    @test jil"(def (foo x (kw y 2)) 1)" == julia"foo(x, y=2) = begin 1 end"
    @test jil"(define foo 1)" == julia"foo = 1"
    @test jil"(define (foo x) 1)" == julia"foo(x) = begin 1 end"
    @test jil"(define (foo x . y) 1)" == julia"foo(x, y...) = begin 1 end"
end
