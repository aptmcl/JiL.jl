using JiL
using Test


@testset "JiL.jl" begin
    @test jil"(def foo 1)" == julia"foo = 1"
    @test jil"(def (bar x) 1)" == julia"bar(x) = begin 1 end"
    @test jil"(def (baz x (... y)) 1)" == julia"baz(x, y...) = begin 1 end"
    @test jil"(define foo 1)" == julia"foo = 1"
    @test jil"(define (bar x) 1)" == julia"bar(x) = begin 1 end"
    @test jil"(define (baz x . y) 1)" == julia"baz(x, y...) = begin 1 end"
end
