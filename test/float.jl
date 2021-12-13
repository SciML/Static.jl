
@testset "StaticFloat64" begin
    for i ∈ -10:10
        for j ∈ -10:10
            @test i+j == @inferred(Static.StaticInt(i) + Static.StaticFloat64(j)) == @inferred(i + Static.StaticFloat64(j)) == @inferred(Static.StaticFloat64(i) + j) == @inferred(Static.StaticFloat64(i) + Static.StaticInt(j)) == @inferred(Static.StaticFloat64(i) + Static.StaticFloat64(j))
            @test i-j == @inferred(Static.StaticInt(i) - Static.StaticFloat64(j)) == @inferred(i - Static.StaticFloat64(j)) == @inferred(Static.StaticFloat64(i) - Static.StaticInt(j)) == @inferred(Static.StaticFloat64(i) - j) == @inferred(Static.StaticFloat64(i) - Static.StaticFloat64(j))
            @test i*j == @inferred(Static.StaticInt(i) * Static.StaticFloat64(j)) == @inferred(i * Static.StaticFloat64(j)) == @inferred(Static.StaticFloat64(i) * Static.StaticInt(j)) == @inferred(Static.StaticFloat64(i) * j) == @inferred(Static.StaticFloat64(i) * Static.StaticFloat64(j))
            i == j == 0 && continue
            @test i/j == @inferred(Static.StaticInt(i) / Static.StaticFloat64(j)) == @inferred(i / Static.StaticFloat64(j)) == @inferred(Static.StaticFloat64(i) / Static.StaticInt(j)) == @inferred(Static.StaticFloat64(i) / j) == @inferred(Static.StaticFloat64(i) / Static.StaticFloat64(j))
        end
        if i ≥ 0
            @test sqrt(i) == @inferred(sqrt(Static.StaticInt(i))) == @inferred(sqrt(Static.StaticFloat64(i))) == @inferred(sqrt(Static.StaticFloat64(Float64(i))))
        end
    end
    @test Static.floortostaticint(1.0) === 1
    @test Static.floortostaticint(prevfloat(2.0)) === 1
    @test @inferred(Static.floortostaticint(Static.StaticFloat64(1.0))) === Static.StaticInt(1)
    @test @inferred(Static.floortostaticint(Static.StaticFloat64(prevfloat(2.0)))) === Static.StaticInt(1)

    @test Static.roundtostaticint(1.0) === 1
    @test Static.roundtostaticint(prevfloat(2.0)) === 2
    @test @inferred(Static.roundtostaticint(Static.StaticFloat64(1.0))) === Static.StaticInt(1)
    @test @inferred(Static.roundtostaticint(Static.StaticFloat64(prevfloat(2.0)))) === Static.StaticInt(2)
    @test @inferred(round(Static.StaticFloat64(1.0))) === Static.StaticFloat64(1)
    @test @inferred(round(Static.StaticFloat64(prevfloat(2.0)))) === Static.StaticFloat64(2)

    fone = static(1.0)
    fzero = static(0.0)
    @test @inferred(isone(fone))
    @test @inferred(isone(one(fzero)))
    @test @inferred(isone(fzero)) === false

    @test @inferred(iszero(fone)) === false
    @test @inferred(iszero(fzero))
    @test @inferred(iszero(zero(typeof(fzero))))

    @test typeof(fone)(1) isa Static.StaticFloat64
    @test typeof(fone)(1.0) isa Static.StaticFloat64

    @test @inferred(eltype(Static.StaticFloat64(static(1)))) <: Float64
    @test @inferred(promote_rule(typeof(fone), Int)) <: promote_type(Float64, Int)
    @test @inferred(promote_rule(typeof(fone), Float64)) <: Float64
    @test @inferred(promote_rule(typeof(fone), Float32)) <: Float32
    @test @inferred(promote_rule(typeof(fone), Float16)) <: Float16

    @test @inferred(inv(static(2.0))) === static(inv(2.0))

    @test @inferred(static(2.0)^2.0) === 2.0^2.0

    for f in (sin, cos, tan, asin, atan, acos,
        sinh, cosh, tanh, asinh, atanh,
        exp, exp2, exp10, expm1, log, log2, log10, log1p,
        exponent, sqrt, cbrt)
        @info "Testing $f(0.5)"
        @inferred f(static(.5))
    end

    @info "Testing acosh(1.5)"
    @inferred acosh(static(1.5))
end

