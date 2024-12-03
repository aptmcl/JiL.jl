using JiL
using Test



@testset "JiL.jl" begin
    @test jil"(define foo 1)" == julia"foo = 1"
    @test jil"(define (bar x) 1)" == julia"bar(x) = begin 1 end"
end
