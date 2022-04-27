module StructSizes
using Test

abstract type AbstractSeg end

struct SegCom
    pt::Float64
    weight::Float64
end

struct PricingParams
    styl::Int
    strik::Float64
    toExpY::Float64
    vty::Float64
    under::Float64
end

struct SegPricing <: AbstractSeg
    params::PricingParams
    com::SegCom
end

@testset "Test struct sizes" begin
    pp1 = PricingParams(1, 1.0, 1.0, 2.4, 2.3)
    sc1 = SegCom(1.0, 1.0)
    sp1 = SegPricing(pp1, sc1)

    @test isbits(pp1)
    @test isbits(sc1)
    @test isbits(sp1)
    @test sizeof(pp1) == sizeof(1.0)*4 + sizeof(1)
    @test sizeof(sc1) == sizeof(1.0)*2
    @test sizeof(sp1) == sizeof(pp1) + sizeof(sc1)
end

end