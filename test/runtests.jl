using Static, Aqua
using Test

@testset "Static.jl" begin

    Aqua.test_all(Static)

    @testset "StaticInt" begin
        @test StaticInt(UInt(8)) === StaticInt{8}()
        @test iszero(StaticInt(0))
        @test !iszero(StaticInt(1))
        @test !isone(StaticInt(0))
        @test isone(StaticInt(1))
        @test @inferred(one(StaticInt(1))) === StaticInt(1)
        @test @inferred(zero(StaticInt(1))) === StaticInt(0)
        @test @inferred(one(StaticInt)) === StaticInt(1)
        @test @inferred(zero(StaticInt)) === StaticInt(0) === StaticInt(StaticInt(Val(0)))
        @test eltype(one(StaticInt)) <: Int

        x = StaticInt(1)
        @test @inferred(Bool(x)) isa Bool
        @test @inferred(BigInt(x)) isa BigInt
        @test @inferred(Integer(x)) === x
        @test @inferred(%(x, Integer)) === 1
        # test for ambiguities and correctness
        for i ∈ Any[StaticInt(0), StaticInt(1), StaticInt(2), 3]
            for j ∈ Any[StaticInt(0), StaticInt(1), StaticInt(2), 3]
                i === j === 3 && continue
                for f ∈ [+, -, *, ÷, %, <<, >>, >>>, &, |, ⊻, ==, ≤, ≥]
                    (iszero(j) && ((f === ÷) || (f === %))) && continue # integer division error
                    @test convert(Int, @inferred(f(i,j))) == f(convert(Int, i), convert(Int, j))
                end
            end
            i == 3 && break
            for f ∈ [+, -, *, /, ÷, %, ==, ≤, ≥]
                w = f(convert(Int, i), 1.4)
                x = f(1.4, convert(Int, i))
                @test convert(typeof(w), @inferred(f(i, 1.4))) === w
                @test convert(typeof(x), @inferred(f(1.4, i))) === x # if f is division and i === StaticInt(0), returns `NaN`; hence use of ==== in check.
                (((f === ÷) || (f === %)) && (i === StaticInt(0))) && continue
                y = f(convert(Int, i), 2 // 7)
                z = f(2 // 7, convert(Int, i))
                @test convert(typeof(y), @inferred(f(i, 2 // 7))) === y
                @test convert(typeof(z), @inferred(f(2 // 7, i))) === z 
            end
        end

        @test UnitRange{Int16}(StaticInt(-9), 17) === Int16(-9):Int16(17)
        @test UnitRange{Int16}(-7, StaticInt(19)) === Int16(-7):Int16(19)
        @test UnitRange(-11, StaticInt(15)) === -11:15
        @test UnitRange(StaticInt(-11), 15) === -11:15
        @test UnitRange{Int}(StaticInt(-11), StaticInt(15)) === -11:15
        @test UnitRange(StaticInt(-11), StaticInt(15)) === -11:15
        @test float(StaticInt(8)) === static(8.0)

        # test specific promote rules to ensure we don't cause ambiguities
        SI = StaticInt{1}
        IR = typeof(1//1)
        PI = typeof(pi)
        @test @inferred(convert(SI, SI())) === SI()
        @test @inferred(promote_rule(SI, PI)) <: promote_type(Int, PI)
        @test @inferred(promote_rule(SI, IR)) <: promote_type(Int, IR)
        @test @inferred(promote_rule(SI, SI)) <: Int
        @test @inferred(promote_rule(Missing, SI)) <: promote_type(Missing, Int)
        @test @inferred(promote_rule(Nothing, SI)) <: promote_type(Nothing, Int)
        @test @inferred(promote_rule(SI, Missing)) <: promote_type(Int, Missing)
        @test @inferred(promote_rule(SI, Nothing)) <: promote_type(Int, Nothing)
        @test @inferred(promote_rule(Union{Missing,Int}, SI)) <: promote_type(Union{Missing,Int}, Int)
        @test @inferred(promote_rule(Union{Nothing,Int}, SI)) <: promote_type(Union{Nothing,Int}, Int)
        @test @inferred(promote_rule(Union{Nothing,Missing,Int}, SI)) <: Union{Nothing,Missing,Int}
        @test @inferred(promote_rule(Union{Nothing,Missing}, SI)) <: promote_type(Union{Nothing,Missing}, Int)
        @test @inferred(promote_rule(SI, Missing)) <: promote_type(Int, Missing)
        @test @inferred(promote_rule(Base.TwicePrecision{Int}, StaticInt{1})) <: Base.TwicePrecision{Int}

        @test static(Int8(-18)) === static(-18)
        @test static(0xef) === static(239)
        @test static(Int16(-18)) === static(-18)
        @test static(0xffef) === static(65519)
        if sizeof(Int) == 8
            @test static(Int32(-18)) === static(-18)
            @test static(0xffffffef) === static(4294967279)
        end
    end

    @testset "StaticBool" begin
        t = static(static(true))
        f = StaticBool(static(false))

        @test @inferred(StaticInt(t)) === StaticInt(1)
        @test @inferred(StaticInt(f)) === StaticInt(0)

        @test @inferred(~t) === f
        @test @inferred(~f) === t
        @test @inferred(!t) === f
        @test @inferred(!f) === t
        @test @inferred(+t) === StaticInt(1)
        @test @inferred(+f) === StaticInt(0)
        @test @inferred(-t) === StaticInt(-1)
        @test @inferred(-f) === StaticInt(0)
        @test @inferred(sign(t)) === t
        @test @inferred(abs(t)) === t
        @test @inferred(abs2(t)) === t
        @test @inferred(iszero(t)) === f
        @test @inferred(isone(t)) === t
        @test @inferred(iszero(f)) === t
        @test @inferred(isone(f)) === f

        @test @inferred(xor(true, f))
        @test @inferred(xor(f, true))
        @test @inferred(xor(f, f)) === f
        @test @inferred(xor(f, t)) === t
        @test @inferred(xor(t, f)) === t
        @test @inferred(xor(t, t)) === f

        @test @inferred(|(true, f))
        @test @inferred(|(true, t)) === t
        @test @inferred(|(f, true))
        @test @inferred(|(t, true)) === t
        @test @inferred(|(f, f)) === f
        @test @inferred(|(f, t)) === t
        @test @inferred(|(t, f)) === t
        @test @inferred(|(t, t)) === t

        @test @inferred(Base.:(&)(true, f)) === f
        @test @inferred(Base.:(&)(true, t))
        @test @inferred(Base.:(&)(f, true)) === f
        @test @inferred(Base.:(&)(t, true))
        @test @inferred(Base.:(&)(f, f)) === f
        @test @inferred(Base.:(&)(f, t)) === f
        @test @inferred(Base.:(&)(t, f)) === f
        @test @inferred(Base.:(&)(t, t)) === t

        @test @inferred(<(f, f)) === f
        @test @inferred(<(f, t)) === t
        @test @inferred(<(t, f)) === f
        @test @inferred(<(t, t)) === f

        @test @inferred(<=(f, f)) === t
        @test @inferred(<=(f, t)) === t
        @test @inferred(<=(t, f)) === f
        @test @inferred(<=(t, t)) === t

        @test @inferred(==(f, f)) === t
        @test @inferred(==(f, t)) === f
        @test @inferred(==(t, f)) === f
        @test @inferred(==(t, t)) === t

        @test @inferred(*(f, t)) === t & f
        @test @inferred(-(f, t)) === StaticInt(f) - StaticInt(t)
        @test @inferred(+(f, t)) === StaticInt(f) + StaticInt(t)

        @test @inferred(^(t, f)) == ^(true, false)
        @test @inferred(^(t, t)) == ^(true, true)

        @test @inferred(^(2, f)) == 1
        @test @inferred(^(2, t)) == 2

        @test @inferred(^(BigInt(2), f)) == 1
        @test @inferred(^(BigInt(2), t)) == 2

        @test @inferred(div(t, t)) === t
        @test_throws DivideError div(t, f)

        @test @inferred(rem(t, t)) === f
        @test_throws DivideError rem(t, f)
        @test @inferred(mod(t, t)) === f

        @test @inferred(all((t, t, t)))
        @test !@inferred(all((t, f, t)))
        @test !@inferred(all((f, f, f)))

        @test @inferred(any((t, t, t)))
        @test @inferred(any((t, f, t)))
        @test !@inferred(any((f, f, f)))

        x = StaticInt(1)
        y = StaticInt(0)
        z = StaticInt(-1)
        @test @inferred(Static.eq(y)(x)) === f
        @test @inferred(Static.eq(x, x)) === t

        @test @inferred(Static.ne(y)(x)) === t
        @test @inferred(Static.ne(x, x)) === f

        @test @inferred(Static.gt(y)(x)) === t
        @test @inferred(Static.gt(y, x)) === f

        @test @inferred(Static.ge(y)(x)) === t
        @test @inferred(Static.ge(y, x)) === f

        @test @inferred(Static.lt(x)(y)) === t
        @test @inferred(Static.lt(x, y)) === f

        @test @inferred(Static.le(x)(y)) === t
        @test @inferred(Static.le(x, y)) === f

        @test @inferred(Static.ifelse(t, x, y)) === x
        @test @inferred(Static.ifelse(f, x, y)) === y

        @test @inferred(promote_rule(True, True)) <: StaticBool
        @test @inferred(promote_rule(True, Bool)) <: Bool
        @test @inferred(promote_rule(Bool, True)) <: Bool
    end

    @testset "StaticSymbol" begin
        x = StaticSymbol(:x)
        y = StaticSymbol("y")
        z = StaticSymbol(1)
        @test y === StaticSymbol(:y)
        @test z === StaticSymbol(Symbol(1))
        @test @inferred(StaticSymbol(x)) === x
        @test @inferred(StaticSymbol(x, y)) === StaticSymbol(:x, :y)
        @test @inferred(StaticSymbol(x, y, z)) === static(:xy1)
    end

    @testset "static interface" begin
        v = Val((:a, 1, true))

        @test static(1) === StaticInt(1)
        @test static(true) === True()
        @test static(:a) === StaticSymbol{:a}()
        @test Symbol(static(:a)) === :a
        @test static((:a, 1, true)) === (static(:a), static(1), static(true))
        @test @inferred(static(v)) === (static(:a), static(1), static(true))
        @test_throws ErrorException static("a")

        @test @inferred(Static.is_static(v)) === True()
        @test @inferred(Static.is_static(typeof(v))) === True()
        @test @inferred(Static.is_static(typeof(static(true)))) === True()
        @test @inferred(Static.is_static(typeof(static(1)))) === True()
        @test @inferred(Static.is_static(typeof(static(:x)))) === True()
        @test @inferred(Static.is_static(typeof(1))) === False()
        @test @inferred(Static.is_static(typeof((static(:x),static(:x))))) === True()
        @test @inferred(Static.is_static(typeof((static(:x),:x)))) === False()
        @test @inferred(Static.is_static(typeof(static(1.0)))) === True()

        @test @inferred(Static.known(v)) === (:a, 1, true)
        @test @inferred(Static.known(typeof(v))) === (:a, 1, true)
        @test @inferred(Static.known(typeof(static(true))))
        @test @inferred(Static.known(typeof(static(false)))) === false
        @test @inferred(Static.known(typeof(static(1.0)))) === 1.0
        @test @inferred(Static.known(typeof(static(1)))) === 1
        @test @inferred(Static.known(typeof(static(:x)))) === :x
        @test @inferred(Static.known(typeof(1))) === nothing
        @test @inferred(Static.known(typeof((static(:x),static(:x))))) === (:x, :x)
        @test @inferred(Static.known(typeof((static(:x),:x)))) === (:x, nothing)

        @test @inferred(Static.dynamic((static(:a), static(1), true))) === (:a, 1, true)
    end

    @testset "tuple utilities" begin
        x = (static(1), static(2), static(3))
        y = (static(3), static(2), static(1))
        z = (static(1), static(2), static(3), static(4))
        T = Tuple{Int,Float64,String}
        @test @inferred(Static.invariant_permutation(x, x)) === True()
        @test @inferred(Static.invariant_permutation(x, y)) === False()
        @test @inferred(Static.invariant_permutation(x, z)) === False()

        @test @inferred(Static.permute(x, Val(x))) === x
        @test @inferred(Static.permute(x, (static(1), static(2)))) === (static(1), static(2))
        @test @inferred(Static.permute(x, x)) === x
        @test @inferred(Static.permute(x, y)) === y
        @test @inferred(Static.eachop(getindex, x)) === x

        get_tuple_add(::Type{T}, ::Type{X}, dim::StaticInt) where {T,X} = Tuple{Static._get_tuple(T, dim),X}
        @test @inferred(Static.eachop_tuple(Static._get_tuple, y, T)) === Tuple{String,Float64,Int}
        @test @inferred(Static.eachop_tuple(get_tuple_add, y, T, String)) === Tuple{Tuple{String,String},Tuple{Float64,String},Tuple{Int,String}}
        @test @inferred(Static.find_first_eq(static(1), y)) === static(3)
        # inferred is Union{Int,Nothing}
        @test Static.find_first_eq(1, map(Int, y)) === 3
    end

    @test repr(static(float(1))) == "static($(float(1)))"
    @test repr(static(1)) == "static(1)"
    @test repr(static(:x)) == "static(:x)"
end

# for some reason this can't be inferred when in the "Static.jl" test set
known_length(x) = known_length(typeof(x))
known_length(::Type{T}) where {N,T<:Tuple{Vararg{Any,N}}} = N
known_length(::Type{T}) where {T} = nothing
maybe_static_length(x) = Static.maybe_static(known_length, length, x)
x = ntuple(+, 10)
y = 1:10
@test @inferred(maybe_static_length(x)) === StaticInt(10)
@test @inferred(maybe_static_length(y)) === 10

include("float.jl")


#=
A = rand(3,4);

offset1(x::Base.OneTo) = static(1)
offset1(x::AbstractUnitRange) = first(x)

offsets(x) = Static._eachop(offset1, (axes(x),), Static.nstatic(Val(ndims(x))))

=#
