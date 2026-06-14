using AllocCheck
using Static
using Test

# Helper functions to test allocation-free operations
# We use wrapper functions because @check_allocs requires function definitions

# Note: Type creation functions like StaticInt(n) involve constructing new types at
# compile time, which AllocCheck may flag. We focus on operations that should
# definitely be allocation-free at runtime.

# Arithmetic operations on StaticInt (these should definitely be allocation-free)
@check_allocs static_add(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = x + y
@check_allocs static_sub(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = x - y
@check_allocs static_mul(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = x * y
@check_allocs static_div(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = x / y
@check_allocs static_neg(x::StaticInt{X}) where {X} = -x

# Bitwise operations
@check_allocs static_lshift(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = x << y
@check_allocs static_rshift(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = x >> y
@check_allocs static_and(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = x & y
@check_allocs static_or(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = x | y
@check_allocs static_xor(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = xor(x, y)

# Boolean operations
@check_allocs static_not(x::StaticBool{B}) where {B} = !x
@check_allocs static_bool_and(x::StaticBool{X}, y::StaticBool{Y}) where {X, Y} = x & y
@check_allocs static_bool_or(x::StaticBool{X}, y::StaticBool{Y}) where {X, Y} = x | y

# Core interface functions
@check_allocs get_known(x::StaticInt{N}) where {N} = known(x)
@check_allocs get_dynamic(x::StaticInt{N}) where {N} = dynamic(x)
@check_allocs check_is_static(x::StaticInt{N}) where {N} = is_static(x)

# Comparison operators
@check_allocs static_eq(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = Static.eq(x, y)
@check_allocs static_ne(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = Static.ne(x, y)
@check_allocs static_lt(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = Static.lt(x, y)
@check_allocs static_gt(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = Static.gt(x, y)
@check_allocs static_le(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = Static.le(x, y)
@check_allocs static_ge(x::StaticInt{X}, y::StaticInt{Y}) where {X, Y} = Static.ge(x, y)

# Tuple operations
@check_allocs static_reduce_tup(t::Tuple{StaticInt{A}, StaticInt{B}, StaticInt{C}}) where {A, B, C} = Static.reduce_tup(+, t)

# NDIndex operations
@check_allocs ndindex_getindex(idx::NDIndex{3}, i::StaticInt{I}) where {I} = idx[i]
@check_allocs ndindex_tuple(idx::NDIndex{N}) where {N} = Tuple(idx)

# Range operations
@check_allocs range_length(r::Static.OptionallyStaticUnitRange{StaticInt{S}, StaticInt{E}}) where {S, E} = length(r)
@check_allocs range_first(r::Static.OptionallyStaticUnitRange{StaticInt{S}, StaticInt{E}}) where {S, E} = first(r)
@check_allocs range_iterate(r::Static.OptionallyStaticUnitRange{StaticInt{S}, StaticInt{E}}) where {S, E} = iterate(r)

# Float operations
@check_allocs static_sin(x::StaticFloat64{F}) where {F} = sin(x)
@check_allocs static_cos(x::StaticFloat64{F}) where {F} = cos(x)
@check_allocs static_exp(x::StaticFloat64{F}) where {F} = exp(x)
@check_allocs static_sqrt(x::StaticFloat64{F}) where {F} = sqrt(x)

# static_promote
@check_allocs test_static_promote(x::StaticInt{N}, y::StaticInt{N}) where {N} = static_promote(x, y)

@testset "AllocCheck - Zero Allocations" begin
    @testset "StaticInt Arithmetic" begin
        x = static(10)
        y = static(3)
        @test static_add(x, y) === static(13)
        @test static_sub(x, y) === static(7)
        @test static_mul(x, y) === static(30)
        @test static_div(x, y) === static(10.0 / 3.0)
        @test static_neg(x) === static(-10)
    end

    @testset "Bitwise Operations" begin
        x = static(10)
        y = static(2)
        @test static_lshift(x, y) === static(40)
        @test static_rshift(x, y) === static(2)
        @test static_and(x, y) === static(2)
        @test static_or(x, y) === static(10)
        @test static_xor(x, y) === static(8)
    end

    @testset "Boolean Operations" begin
        t = static(true)
        f = static(false)
        @test static_not(t) === False()
        @test static_not(f) === True()
        @test static_bool_and(t, f) === False()
        @test static_bool_or(t, f) === True()
    end

    @testset "Core Interface" begin
        x = static(5)
        @test get_known(x) === 5
        @test get_dynamic(x) === 5
        @test check_is_static(x) === True()
    end

    @testset "Comparison Operators" begin
        x = static(1)
        y = static(2)
        @test static_eq(x, x) === True()
        @test static_ne(x, y) === True()
        @test static_lt(x, y) === True()
        @test static_gt(y, x) === True()
        @test static_le(x, y) === True()
        @test static_ge(y, x) === True()
    end

    @testset "Tuple Operations" begin
        tup = (static(1), static(2), static(3))
        @test static_reduce_tup(tup) === static(6)
    end

    @testset "NDIndex Operations" begin
        x, y, z = static(1), static(2), static(3)
        idx = NDIndex(x, y, z)
        @test ndindex_getindex(idx, static(1)) === static(1)
        @test ndindex_tuple(idx) === (static(1), static(2), static(3))
    end

    @testset "Range Operations" begin
        r = static(1):static(10)
        @test range_length(r) === 10
        @test range_first(r) === 1
        @test range_iterate(r) === (1, 1)
    end

    @testset "Float Operations" begin
        x = static(0.5)
        @test static_sin(x) === static(sin(0.5))
        @test static_cos(x) === static(cos(0.5))
        @test static_exp(x) === static(exp(0.5))
        @test static_sqrt(x) === static(sqrt(0.5))
    end

    @testset "static_promote" begin
        x = static(5)
        @test test_static_promote(x, x) === x
    end
end
