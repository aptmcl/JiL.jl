using JiL
using Test

@testset "JiL.jl" begin
    @test jil"(define foo 1)" == :(foo = 1)
end
