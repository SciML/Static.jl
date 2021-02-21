
@testset "Static Numbers" begin
    for i ∈ -10:10
        for j ∈ -10:10
            @test i+j == @inferred(Static.StaticInt(i) + Static.StaticFloat(j)) == @inferred(i + Static.StaticFloat(j)) == @inferred(Static.StaticFloat(i) + j) == @inferred(Static.StaticFloat(i) + Static.StaticInt(j)) == @inferred(Static.StaticFloat(i) + Static.StaticFloat(j))
            @test i-j == @inferred(Static.StaticInt(i) - Static.StaticFloat(j)) == @inferred(i - Static.StaticFloat(j)) == @inferred(Static.StaticFloat(i) - Static.StaticInt(j)) == @inferred(Static.StaticFloat(i) - j) == @inferred(Static.StaticFloat(i) - Static.StaticFloat(j))
            @test i*j == @inferred(Static.StaticInt(i) * Static.StaticFloat(j)) == @inferred(i * Static.StaticFloat(j)) == @inferred(Static.StaticFloat(i) * Static.StaticInt(j)) == @inferred(Static.StaticFloat(i) * j) == @inferred(Static.StaticFloat(i) * Static.StaticFloat(j))
            i == j == 0 && continue
            @test i/j == @inferred(Static.StaticInt(i) / Static.StaticFloat(j)) == @inferred(i / Static.StaticFloat(j)) == @inferred(Static.StaticFloat(i) / Static.StaticInt(j)) == @inferred(Static.StaticFloat(i) / j) == @inferred(Static.StaticFloat(i) / Static.StaticFloat(j))
        end
        if i ≥ 0
            @test sqrt(i) == @inferred(sqrt(Static.StaticInt(i))) == @inferred(sqrt(Static.StaticFloat(i))) == @inferred(sqrt(Static.StaticFloat(Float64(i))))
        end
    end
    @test Static.floortostaticint(1.0) === 1
    @test Static.floortostaticint(prevfloat(2.0)) === 1
    @test @inferred(Static.floortostaticint(Static.StaticFloat(1.0))) === Static.StaticInt(1)
    @test @inferred(Static.floortostaticint(Static.StaticFloat(prevfloat(2.0)))) === Static.StaticInt(1)

    @test Static.roundtostaticint(1.0) === 1
    @test Static.roundtostaticint(prevfloat(2.0)) === 2
    @test @inferred(Static.roundtostaticint(Static.StaticFloat(1.0))) === Static.StaticInt(1)
    @test @inferred(Static.roundtostaticint(Static.StaticFloat(prevfloat(2.0)))) === Static.StaticInt(2)
    @test @inferred(round(Static.StaticFloat(1.0))) === Static.StaticFloat(1)
    @test @inferred(round(Static.StaticFloat(prevfloat(2.0)))) === Static.StaticFloat(2)
end
