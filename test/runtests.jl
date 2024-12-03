using JiL
using Test

@testset "JiL.jl" begin
    @test jil"(define foo 1)" == :(foo = 1)
    @test jil"(define (bar x) 1)" == :(bar(x) = begin 1 end)
end
