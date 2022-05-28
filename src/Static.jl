module Static

import IfElse: ifelse
using Base: @propagate_inbounds, Slice, AbstractCartesianIndex, Fix2, BitIntegerType

export StaticInt, StaticFloat64, StaticSymbol, True, False, StaticBool, NDIndex
export dynamic, is_static, known, static 

"""
    StaticSymbol

A statically typed `Symbol`.
"""
struct StaticSymbol{s}
    StaticSymbol{s}() where {s} = new{s::Symbol}()
    StaticSymbol(s::Symbol) = new{s}()
    StaticSymbol(@nospecialize x::StaticSymbol) = x
    StaticSymbol(x) = StaticSymbol(Symbol(x))
    StaticSymbol(x, y) = StaticSymbol(Symbol(x, y))
    @generated function StaticSymbol(::StaticSymbol{X}, ::StaticSymbol{Y}) where {X,Y}
        StaticSymbol(Symbol(X, Y))
    end
    StaticSymbol(x, y, z...) = StaticSymbol(StaticSymbol(x, y), z...)
end

Base.Symbol(@nospecialize(s::StaticSymbol)) = known(s)

"""
    StaticInt(N::Int) -> StaticInt{N}()

A statically sized `Int`.
Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticInt{N} <: Real
    StaticInt{N}() where {N} = new{N::Int}()
    StaticInt(N::Int) = new{N}()
    StaticInt(@nospecialize N::StaticInt) = N
    StaticInt(::Val{N}) where {N} = StaticInt(N)
    StaticInt(@nospecialize x::Integer) = StaticInt(convert(Int, x)::Int)
end

"""
    StaticFloat64{N}

A statically sized `Float64`.
Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticFloat64{N} <: Real
    StaticFloat64{N}() where {N} = new{N::Float64}()
    StaticFloat64(x::Float64) = new{x}()
    StaticFloat64(x::Int) = new{Base.sitofp(Float64, x)::Float64}()
    StaticFloat64(@nospecialize(x::StaticInt)) = float(x)
    StaticFloat64(x::Complex) = StaticFloat64(convert(Float64, x))
    StaticFloat64(x::Real) = StaticFloat64(convert(Float64, x))
end

"""
    StaticBool(x::Bool) -> True/False

A statically typed `Bool`.
"""
abstract type StaticBool{bool} <: Real end

struct True <: StaticBool{true} end

struct False <: StaticBool{false} end

StaticBool{true}() = True()
StaticBool{false}() = False()
StaticBool(x::StaticBool) = x
function StaticBool(x::Bool)
    if x
        return True()
    else
        return False()
    end
end

StaticInt(x::False) = Zero()
StaticInt(x::True) = One()
Base.Bool(::True) = true
Base.Bool(::False) = false

const StaticInteger{N} = Union{StaticInt{N},StaticBool{N}}
const StaticNumber{N} = Union{StaticFloat64{N},StaticInteger{N}}

const Zero = StaticInt{0}
const One = StaticInt{1}
const FloatOne = StaticFloat64{one(Float64)}
const FloatZero = StaticFloat64{zero(Float64)}

Base.eltype(@nospecialize(T::Type{<:StaticFloat64})) = Float64
Base.eltype(@nospecialize(T::Type{<:StaticInt})) = Int
Base.eltype(@nospecialize(T::Type{<:StaticBool})) = Bool

Base.:(~)(::StaticNumber{N}) where {N} = static(~N)

Base.inv(x::StaticNumber{N}) where {N} = one(x) / x

Base.sqrt(::StaticNumber{N}) where {N} = static(sqrt(N))

@inline Base.one(@nospecialize T::Type{<:StaticNumber}) = static(one(eltype(T)))
@inline Base.zero(@nospecialize T::Type{<:StaticNumber}) = static(one(eltype(T)))
@inline Base.iszero(::Union{Zero,FloatZero,False}) = true
@inline Base.iszero(@nospecialize x::StaticNumber) = false
@inline Base.isone(::Union{One,FloatOne,True}) = true
@inline Base.isone(@nospecialize x::StaticNumber) = false

Base.AbstractFloat(x::StaticNumber) = StaticFloat64(x)

Base.widen(@nospecialize(x::StaticNumber)) = widen(known(x))

Base.convert(::Type{T}, @nospecialize(N::StaticInt)) where {T<:Number} = convert(T, Int(N))

Base.Bool(::StaticInt{N}) where {N} = Bool(N)

Base.BigInt(@nospecialize(x::StaticInt)) = BigInt(Int(x))
Base.Integer(@nospecialize(x::StaticInt)) = x
(::Type{T})(@nospecialize(x::StaticInt)) where {T<:Integer} = T(known(x))
(::Type{T})(x::Int) where {T<:StaticInt} = StaticInt{x}()
Base.convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()

Base.:(%)(@nospecialize(n::StaticInt), ::Type{Integer}) = Int(n)

Base.:(*)(@nospecialize(x::StaticInt), ::Zero) = Zero()
Base.:(*)(::Zero, @nospecialize(y::StaticInt)) = Zero()
Base.:(*)(::Zero, ::Zero) = Zero()

@inline Base.:(-)(::StaticNumber{N}) where {N} = static(-N)

for f in [:(%), :(<<), :(>>), :(>>>), :(&), :(|), :(⊻)]
    @eval begin
        Base.$f(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = static($f(X,Y))
        Base.$f(x::StaticInteger, y::Integer) = $f(known(x), y)
        Base.$f(x::Integer, y::StaticInteger) = $f(x, known(y))
    end
end

Base.minmax(x::StaticNumber, y::StaticNumber) = y < x ? (y, x) : (x, y)

for f in [:(==), :(!=), :(<), :(≤), :(≥)]
    @eval begin
        Base.$f(::StaticNumber{M}, ::StaticNumber{N}) where {M,N} = $f(M, N)
        Base.$f(@nospecialize(x::StaticNumber), y::Real) = $f(known(x), y)
        Base.$f(x::Real, @nospecialize(y::StaticNumber)) = $f(x, known(y))
    end
end

Base.:(==)(@nospecialize(x::StaticNumber), y::AbstractIrrational) = known(x) == y
Base.:(==)(x::AbstractIrrational, @nospecialize(y::StaticNumber)) = x == known(y)

for f in [:(+), :(-), :(*), :div, :min, :max, :rem]
    @eval begin
        Base.$f(::StaticNumber{M}, ::StaticNumber{N}) where {M,N} = static($f(M, N))
        Base.$f(@nospecialize(x::StaticNumber), y::Real) = $f(known(x), y)
        Base.$f(x::Real, @nospecialize(y::StaticNumber)) = $f(x, known(y))
    end
end

#(::Type{T})(x::Integer) where {T<:StaticFloat64} = StaticFloat64(x)
#(::Type{T})(x::AbstractFloat) where {T<:StaticFloat64} = StaticFloat64(x)

Base.convert(::Type{T}, @nospecialize(x::StaticFloat64)) where {T<:AbstractFloat} = T(known(x))

@generated Base.round(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat64, round(M)))
@generated roundtostaticint(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, round(Int, M)))
roundtostaticint(x::AbstractFloat) = round(Int, x)
@generated floortostaticint(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, floor(Int, M)))
floortostaticint(x::AbstractFloat) = Base.fptosi(Int, x)

Base.:(^)(::StaticFloat64{x}, y::Float64) where {x} = exp2(log2(x) * y)

@inline Base.exponent(::StaticFloat64{M}) where {M} = static(exponent(M))

for f in (:rad2deg, :deg2rad, :cbrt,
          :mod2pi, :rem2pi, :sinpi, :cospi,
          :exp, :exp2, :exp10, :expm1,
          :log, :log2, :log10, :log1p,
          :sin, :cos, :tan, :sec, :csc, :cot,
          :asin, :acos, :atan, :asec, :acsc, :acot,
          :sind, :cosd, :tand, :secd, :cscd, :cotd,
          :asind, :acosd, :atand, :asecd, :acscd, :acotd,
          :sinh, :cosh, :tanh, :sech, :csch, :coth,
          :asinh, :acosh, :atanh, :asech, :acsch, :acoth,
         )
    @eval @generated function (Base.$f)(::StaticFloat64{M}) where {M}
        Expr(:call, Expr(:curly, :StaticFloat64, $f(M)))
    end
end

Base.:(!)(::True) = False()
Base.:(!)(::False) = True()

Base.:(|)(x::Bool, y::True) = y
Base.:(|)(x::Bool, y::False) = x
Base.:(|)(x::True, y::Bool) = x
Base.:(|)(x::False, y::Bool) = y

Base.:(&)(x::Bool, y::True) = x
Base.:(&)(x::Bool, y::False) = y
Base.:(&)(x::True, y::Bool) = y
Base.:(&)(x::False, y::Bool) = x

Base.xor(y::StaticBool, x::StaticBool) = _xor(x, y)
_xor(::True, ::True) = False()
_xor(::True, ::False) = True()
_xor(::False, ::True) = True()
_xor(::False, ::False) = False()
Base.xor(x::Bool, y::StaticBool) = xor(x, Bool(y))
Base.xor(x::StaticBool, y::Bool) = xor(Bool(x), y)

Base.sign(x::StaticBool) = x
Base.abs(x::StaticBool) = x
Base.abs2(x::StaticBool) = x

Base.:(+)(x::True) = One()
Base.:(+)(x::False) = Zero()

# from `^(x::Bool, y::Bool) = x | !y`
Base.:(^)(x::StaticBool, y::False) = True()
Base.:(^)(x::StaticBool, y::True) = x
Base.:(^)(x::Integer, y::False) = one(x)
Base.:(^)(x::Integer, y::True) = x
Base.:(^)(x::BigInt, y::False) = one(x)
Base.:(^)(x::BigInt, y::True) = x

#Base.rem(x::StaticBool, y::False) = throw(DivideError())
#Base.rem(x::StaticBool, y::True) = False()
#Base.mod(x::StaticBool, y::StaticBool) = rem(x, y)

Base.all(::Tuple{Vararg{True}}) = true
Base.all(::Tuple{Vararg{Union{True,False}}}) = false
Base.all(::Tuple{Vararg{False}}) = false

Base.any(::Tuple{Vararg{True}}) = true
Base.any(::Tuple{Vararg{Union{True,False}}}) = true
Base.any(::Tuple{Vararg{False}}) = false

ifelse(::True, x, y) = x
ifelse(::False, x, y) = y

Base.UnitRange{T}(@nospecialize(start::StaticNumber), stop) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange{T}(start, @nospecialize(stop::StaticNumber)) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange{T}(@nospecialize(start::StaticNumber), @nospecialize(stop::StaticNumber)) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange(@nospecialize(start::StaticNumber), stop) = UnitRange(known(start), stop)
Base.UnitRange(start, @nospecialize(stop::StaticNumber)) = UnitRange(start, known(stop))
function Base.UnitRange(@nospecialize(start::StaticNumber), @nospecialize(stop::StaticNumber))
    UnitRange(known(start), known(stop))
end

"""
    field_type(::Type{T}, f)

Functionally equivalent to `fieldtype(T, f)` except `f` may be a static type.
"""
@inline field_type(::Type{T}, f::Union{Int,Symbol}) where {T} = fieldtype(T, f)
@generated field_type(::Type{T}, ::StaticInt{N}) where {T,N} = fieldtype(T, N)
@generated field_type(::Type{T}, ::StaticSymbol{S}) where {T,S} = fieldtype(T, S)

@inline nstatic(::Val{N}) where {N} = ntuple(StaticInt, Val(N))

@inline function invariant_permutation(@nospecialize(x::Tuple), @nospecialize(y::Tuple))
    if y === x === nstatic(Val(nfields(x)))
        return True()
    else
        return False()
    end
end

permute(@nospecialize(x::Tuple), @nospecialize(perm::Val)) = permute(x, static(perm))
@inline function permute(@nospecialize(x::Tuple), @nospecialize(perm::Tuple))
    if invariant_permutation(nstatic(Val(nfields(x))), perm) === False()
        return eachop(getindex, perm, x)
    else
        return x
    end
end

"""
    eachop(op, args...; iterator::Tuple{Vararg{StaticInt}}) -> Tuple

Produces a tuple of `(op(args..., iterator[1]), op(args..., iterator[2]),...)`.
"""
@inline function eachop(op::F, itr::Tuple{T,Vararg{Any}}, args::Vararg{Any}) where {F,T}
    (op(args..., first(itr)), eachop(op, Base.tail(itr), args...)...)
end
eachop(::F, ::Tuple{}, args::Vararg{Any}) where {F} = ()

"""
    eachop_tuple(op, arg, args...; iterator::Tuple{Vararg{StaticInt}}) -> Type{Tuple}

Produces a tuple type of `Tuple{op(arg, args..., iterator[1]), op(arg, args..., iterator[2]),...}`.
Note that if one of the arguments passed to `op` is a `Tuple` type then it should be the first argument
instead of one of the trailing arguments, ensuring type inference of each element of the tuple.
"""
eachop_tuple(op, itr, arg, args...) = _eachop_tuple(op, itr, arg, args)
@generated function _eachop_tuple(op, ::I, arg, args::A) where {A,I}
    t = Expr(:curly, Tuple)
    narg = length(A.parameters)
    for p in I.parameters
        call_expr = Expr(:call, :op, :arg)
        if narg > 0
            for i in 1:narg
                push!(call_expr.args, :(getfield(args, $i)))
            end
        end
        push!(call_expr.args, :(StaticInt{$(p.parameters[1])}()))
        push!(t.args, call_expr)
    end
    Expr(:block, Expr(:meta, :inline), t)
end

#=
    find_first_eq(x, collection::Tuple)

Finds the position in the tuple `collection` that is exactly equal (i.e. `===`) to `x`.
If `x` and `collection` are static (`is_static`) and `x` is in `collection` then the return
value is a `StaticInt`.
=#
@generated function find_first_eq(x::X, itr::I) where {X,N,I<:Tuple{Vararg{Any,N}}}
    # we avoid incidental code gen when evaluated a tuple of known values by iterating
    #  through `I.parameters` instead of `known(I)`.
    index = ifelse(known(X) === nothing, nothing, findfirst(==(X), I.parameters))
    if index === nothing
        :(Base.Cartesian.@nif $(N + 1) d->(x == getfield(itr, d)) d->(d) d->(nothing))
    else
        :($(static(index)))
    end
end

"""
  reduce_tup(f::F, inds::Tuple{Vararg{Any,N}}) where {F,N}

An optimized `reduce` for tuples. `Base.reduce`'s `afoldl` will often not inline.
Additionally, `reduce_tup` attempts to order the reduction in an optimal manner.

```julia
julia> using StaticArrays, ArrayInterface, BenchmarkTools

julia> rsum(v::SVector) = ArrayInterface.reduce_tup(+, v.data)
rsum (generic function with 2 methods)

julia> for n ∈ 2:16
           @show n
           v = @SVector rand(n)
           s1 = @btime  sum(\$(Ref(v))[])
           s2 = @btime rsum(\$(Ref(v))[])
       end
n = 2
  0.863 ns (0 allocations: 0 bytes)
  0.863 ns (0 allocations: 0 bytes)
n = 3
  0.862 ns (0 allocations: 0 bytes)
  0.863 ns (0 allocations: 0 bytes)
n = 4
  0.862 ns (0 allocations: 0 bytes)
  0.862 ns (0 allocations: 0 bytes)
n = 5
  1.074 ns (0 allocations: 0 bytes)
  0.864 ns (0 allocations: 0 bytes)
n = 6
  0.864 ns (0 allocations: 0 bytes)
  0.862 ns (0 allocations: 0 bytes)
n = 7
  1.075 ns (0 allocations: 0 bytes)
  0.864 ns (0 allocations: 0 bytes)
n = 8
  1.077 ns (0 allocations: 0 bytes)
  0.865 ns (0 allocations: 0 bytes)
n = 9
  1.081 ns (0 allocations: 0 bytes)
  0.865 ns (0 allocations: 0 bytes)
n = 10
  1.195 ns (0 allocations: 0 bytes)
  0.867 ns (0 allocations: 0 bytes)
n = 11
  1.357 ns (0 allocations: 0 bytes)
  1.400 ns (0 allocations: 0 bytes)
n = 12
  1.543 ns (0 allocations: 0 bytes)
  1.074 ns (0 allocations: 0 bytes)
n = 13
  1.702 ns (0 allocations: 0 bytes)
  1.077 ns (0 allocations: 0 bytes)
n = 14
  1.913 ns (0 allocations: 0 bytes)
  0.867 ns (0 allocations: 0 bytes)
n = 15
  2.076 ns (0 allocations: 0 bytes)
  1.077 ns (0 allocations: 0 bytes)
n = 16
  2.273 ns (0 allocations: 0 bytes)
  1.078 ns (0 allocations: 0 bytes)
```

More importantly, `reduce_tup(_pick_range, inds)` often performs better than `reduce(_pick_range, inds)`.
```julia
julia> using ArrayInterface, BenchmarkTools, Static

julia> inds = (Base.OneTo(100), 1:100, 1:static(100))
(Base.OneTo(100), 1:100, 1:static(100))

julia> @btime reduce(ArrayInterface._pick_range, \$(Ref(inds))[])
  6.405 ns (0 allocations: 0 bytes)
Base.Slice(static(1):static(100))

julia> @btime ArrayInterface.reduce_tup(ArrayInterface._pick_range, \$(Ref(inds))[])
  2.570 ns (0 allocations: 0 bytes)
Base.Slice(static(1):static(100))

julia> inds = (Base.OneTo(100), 1:100, 1:UInt(100))
(Base.OneTo(100), 1:100, 0x0000000000000001:0x0000000000000064)

julia> @btime reduce(ArrayInterface._pick_range, \$(Ref(inds))[])
  6.411 ns (0 allocations: 0 bytes)
Base.Slice(static(1):100)

julia> @btime ArrayInterface.reduce_tup(ArrayInterface._pick_range, \$(Ref(inds))[])
  2.592 ns (0 allocations: 0 bytes)
Base.Slice(static(1):100)

julia> inds = (Base.OneTo(100), 1:100, 1:UInt(100), Int32(1):Int32(100))
(Base.OneTo(100), 1:100, 0x0000000000000001:0x0000000000000064, 1:100)

julia> @btime reduce(ArrayInterface._pick_range, \$(Ref(inds))[])
  9.048 ns (0 allocations: 0 bytes)
Base.Slice(static(1):100)

julia> @btime ArrayInterface.reduce_tup(ArrayInterface._pick_range, \$(Ref(inds))[])
  2.569 ns (0 allocations: 0 bytes)
Base.Slice(static(1):100)
```
"""
@generated function reduce_tup(f::F, inds::Tuple{Vararg{Any,N}}) where {F,N}
    q = Expr(:block, Expr(:meta, :inline, :propagate_inbounds))
    if N == 1
        push!(q.args, :(inds[1]))
        return q
    end
    syms = Vector{Symbol}(undef, N)
    i = 0
    for n ∈ 1:N
        syms[n] = iₙ = Symbol(:i_, (i += 1))
        push!(q.args, Expr(:(=), iₙ, Expr(:ref, :inds, n)))
    end
    W =  1 << (8sizeof(N) - 2 - leading_zeros(N))
    while W > 0
        _N = length(syms)
        for _ ∈ 2W:W:_N
            for w ∈ 1:W
                new_sym = Symbol(:i_, (i += 1))
                push!(q.args, Expr(:(=), new_sym, Expr(:call, :f, syms[w], syms[w+W])))
                syms[w] = new_sym
            end
            deleteat!(syms, 1+W:2W)
        end
        W >>>= 1
    end
    q
end

"""
    NDIndex(i, j, k...)   -> I
    NDIndex((i, j, k...)) -> I

A multidimensional index that refers to a single element. Each dimension is represented by
a single `Int` or `StaticInt`.

```julia
julia> using Static

julia> i = NDIndex(static(1), 2, static(3))
NDIndex(static(1), 2, static(3))

julia> i[static(1)]
static(1)

julia> i[1]
1

```
"""
struct NDIndex{N,I<:Tuple{Vararg{Union{StaticInt,Int},N}}} <: AbstractCartesianIndex{N}
    index::I

    NDIndex{N}(i::Tuple{Vararg{Union{StaticInt,Int},N}}) where {N} = new{N,typeof(i)}(i)
    NDIndex{N}(index::Tuple) where {N} = _ndindex(static(N), _flatten(index...))
    NDIndex{N}(index...) where {N} = NDIndex{N}(index)

    NDIndex{0}(::Tuple{}) = new{0,Tuple{}}(())
    NDIndex{0}() = NDIndex{0}(())

    NDIndex(i::Tuple{Vararg{Union{StaticInt,Int},N}}) where {N} = new{N,typeof(i)}(i)
    NDIndex(i::Vararg{Union{StaticInt,Int},N}) where {N} = NDIndex(i)

    NDIndex(index::Tuple) = NDIndex(_flatten(index...))
    NDIndex(index...) = NDIndex(index)
end

_ndindex(n::StaticInt{N}, i::Tuple{Vararg{Union{Int,StaticInt},N}}) where {N} = NDIndex(i)
function _ndindex(n::StaticInt{N}, i::Tuple{Vararg{Any,M}}) where {N,M}
    M > N && throw(ArgumentError("input tuple of length $M, requested $N"))
    return NDIndex(_fill_to_length(i, n))
end
_fill_to_length(x::Tuple{Vararg{Any,N}}, n::StaticInt{N}) where {N} = x
@inline function _fill_to_length(x::Tuple{Vararg{Any,M}}, n::StaticInt{N}) where {M,N}
    return _fill_to_length((x..., static(1)), n)
end

_flatten(i::StaticInt{N}) where {N} = (i,)
_flatten(i::Integer) = (Int(i),)
_flatten(i::Base.AbstractCartesianIndex) = _flatten(Tuple(i)...)
@inline _flatten(i::StaticInt, I...) = (i, _flatten(I...)...)
@inline _flatten(i::Integer, I...) = (Int(i), _flatten(I...)...)
@inline function _flatten(i::Base.AbstractCartesianIndex, I...)
    return (_flatten(Tuple(i)...)..., _flatten(I...)...)
 end
Base.Tuple(@nospecialize(x::NDIndex)) = getfield(x, :index)

Base.show(io::IO, @nospecialize(x::NDIndex)) = show(io, MIME"text/plain"(), x)
function Base.show(io::IO, m::MIME"text/plain", @nospecialize(x::NDIndex))
    print(io, "NDIndex")
    show(io, m, Tuple(x))
end

# length
Base.length(@nospecialize(x::NDIndex))::Int = length(Tuple(x))
Base.length(::Type{<:NDIndex{N}}) where {N} = N

# indexing
@propagate_inbounds function Base.getindex(x::NDIndex{N,T}, i::Int)::Int where {N,T}
    return Int(getfield(Tuple(x), i))
end
@propagate_inbounds function Base.getindex(x::NDIndex{N,T}, i::StaticInt{I}) where {N,T,I}
    return getfield(Tuple(x), I)
end

# Base.get(A::AbstractArray, I::CartesianIndex, default) = get(A, I.I, default)
# eltype(::Type{T}) where {T<:CartesianIndex} = eltype(fieldtype(T, :I))

Base.setindex(x::NDIndex, i, j) = NDIndex(Base.setindex(Tuple(x), i, j))

# equality
Base.:(==)(@nospecialize(x::NDIndex), @nospecialize(y::NDIndex)) = ==(Tuple(x), Tuple(y))

# zeros and ones
Base.zero(@nospecialize(x::NDIndex)) = zero(typeof(x))
Base.zero(@nospecialize(T::Type{<:NDIndex})) = NDIndex(ntuple(_ -> static(0), Val(length(T))))
Base.oneunit(@nospecialize(x::NDIndex)) = oneunit(typeof(x))
Base.oneunit(@nospecialize(T::Type{<:NDIndex})) = NDIndex(ntuple(_ -> static(1), Val(length(T))))

@inline function Base.IteratorsMD.split(i::NDIndex, V::Val)
    i, j = Base.IteratorsMD.split(Tuple(i), V)
    return NDIndex(i), NDIndex(j)
end

# arithmetic, min/max
@inline Base.:(-)(@nospecialize(i::NDIndex)) = NDIndex(map(-, Tuple(i)))
@inline function Base.:(+)(@nospecialize(i1::NDIndex), @nospecialize(i2::NDIndex))
    NDIndex(map(+, Tuple(i1), Tuple(i2)))
end
@inline function Base.:(-)(@nospecialize(i1::NDIndex), @nospecialize(i2::NDIndex))
    NDIndex(map(-, Tuple(i1), Tuple(i2)))
end
@inline function Base.min(@nospecialize(i1::NDIndex), @nospecialize(i2::NDIndex))
    NDIndex(map(min, Tuple(i1), Tuple(i2)))
end
@inline function Base.max(@nospecialize(i1::NDIndex), @nospecialize(i2::NDIndex))
    NDIndex(map(max, Tuple(i1), Tuple(i2)))
end
@inline Base.:(*)(a::Integer, @nospecialize(i::NDIndex)) = NDIndex(map(x->a*x, Tuple(i)))
@inline Base.:(*)(@nospecialize(i::NDIndex), a::Integer) = *(a, i)

Base.CartesianIndex(@nospecialize(x::NDIndex)) = dynamic(x)

# comparison
@inline function Base.isless(@nospecialize(x::NDIndex), @nospecialize(y::NDIndex))
    Bool(_isless(static(0), Tuple(x), Tuple(y)))
end

lt(@nospecialize(x::NDIndex), @nospecialize(y::NDIndex)) = _isless(static(0), Tuple(x), Tuple(y))

_final_isless(c::Int) = c === 1
_final_isless(::StaticInt{N}) where {N} = static(false)
_final_isless(::StaticInt{1}) = static(true)
_isless(c::C, x::Tuple{}, y::Tuple{}) where {C} = _final_isless(c)
function _isless(c::C, x::Tuple, y::Tuple) where {C}
    _isless(icmp(c, x, y), Base.front(x), Base.front(y))
end
icmp(::StaticInt{0}, x::Tuple, y::Tuple) = icmp(last(x), last(y))
icmp(::StaticInt{N}, x::Tuple, y::Tuple) where {N} = static(N)
function icmp(cmp::Int, x::Tuple, y::Tuple)
    if cmp === 0
        return icmp(Int(last(x)), Int(last(y)))
    else
        return cmp
    end
end
icmp(a, b) = _icmp(lt(a, b), a, b)
_icmp(x::StaticBool, a, b) = ifelse(x, static(1), __icmp(eq(a, b)))
_icmp(x::Bool, a, b) = ifelse(x, 1, __icmp(a == b))
__icmp(x::StaticBool) = ifelse(x, static(0), static(-1))
__icmp(x::Bool) = ifelse(x, 0, -1)

#  Necessary for compatibility with Base
# In simple cases, we know that we don't need to use axes(A). Optimize those
# until Julia gets smart enough to elide the call on its own:
@inline function Base.to_indices(A, inds, I::Tuple{NDIndex, Vararg{Any}})
    to_indices(A, inds, (Tuple(I[1])..., Base.tail(I)...))
end
# But for arrays of CartesianIndex, we just skip the appropriate number of inds
@inline function Base.to_indices(A, inds, I::Tuple{AbstractArray{NDIndex{N,J}}, Vararg{Any}}) where {N,J}
    _, indstail = Base.IteratorsMD.split(inds, Val(N))
    return (Base.to_index(A, I[1]), to_indices(A, indstail, Base.tail(I))...)
end

"""
    known(::Type{T})

Returns the known value corresponding to a static type `T`. If `T` is not a static type then
`nothing` is returned.

See also: [`static`](@ref), [`is_static`](@ref)
"""
known(::Type{<:StaticNumber{N}}) where {N} = N
known(@nospecialize(T::Type{<:StaticSymbol}))::Symbol = T.parameters[1]
known(::Type{Val{V}}) where {V} = V
known(@nospecialize(T::Type{<:NDIndex})) = known(T.parameters[2])
_get_known(::Type{T}, dim::StaticInt{D}) where {T,D} = known(field_type(T, dim))
known(@nospecialize(T::Type{<:Tuple})) = eachop(_get_known, nstatic(Val(fieldcount(T))), T)
known(T::DataType) = nothing
known(@nospecialize(x)) = known(typeof(x))

"""
    static(x)

Returns a static form of `x`. If `x` is already in a static form then `x` is returned. If
there is no static alternative for `x` then an error is thrown.

See also: [`is_static`](@ref), [`known`](@ref)

```julia
julia> using Static

julia> static(1)
static(1)

julia> static(true)
True()

julia> static(:x)
static(:x)

```
"""
static(@nospecialize(x::Union{StaticInt,StaticSymbol,StaticFloat64,True,False})) = x
static(x::Integer) = StaticInt(x)
static(x::Union{AbstractFloat,Complex,Rational,AbstractIrrational}) = StaticFloat64(Float64(x))
static(x::Bool) = StaticBool(x)
static(x::Union{Symbol,AbstractChar,AbstractString}) = StaticSymbol(x)
static(x::Tuple{Vararg{Any}}) = map(static, x)
static(::Val{V}) where {V} = static(V)
static(@nospecialize(x::CartesianIndex)) = NDIndex(static(Tuple(x)))
@noinline static(x) = error("There is no static alternative for type $(typeof(x)).")

"""
    is_static(::Type{T}) -> StaticBool

Returns `True` if `T` is a static type.

See also: [`static`](@ref), [`known`](@ref)
"""
is_static(@nospecialize(x)) = is_static(typeof(x))
is_static(@nospecialize(x::Type{<:Union{StaticInt,StaticSymbol,StaticFloat64,True,False}})) = True()
is_static(@nospecialize(x::Type{<:Val})) = True()
_tuple_static(::Type{T}, i) where {T} = is_static(field_type(T, i))
@inline function is_static(@nospecialize(T::Type{<:Tuple}))
    if all(eachop(_tuple_static, nstatic(Val(fieldcount(T))), T))
        return True()
    else
        return False()
    end
end
is_static(T::DataType) = False()

"""
    dynamic(x)

Returns the "dynamic" or non-static form of `x`.
"""
@inline dynamic(@nospecialize(x)) = ifelse(is_static(typeof(x)), known, identity)(x)
dynamic(@nospecialize(x::Tuple)) = map(dynamic, x)
dynamic(@nospecialize(x::NDIndex)) = CartesianIndex(dynamic(Tuple(x)))

"""
    eq(x, y)

Equivalent to `!=` but if `x` and `y` are both static returns a `StaticBool.
"""
eq(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x == y)
eq(x) = Fix2(eq, x)

"""
    ne(x, y)

Equivalent to `!=` but if `x` and `y` are both static returns a `StaticBool.
"""
ne(x::X, y::Y) where {X,Y} = !eq(x, y)
ne(x) = Fix2(ne, x)

"""
    gt(x, y)

Equivalent to `>` but if `x` and `y` are both static returns a `StaticBool.
"""
gt(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x > y)
gt(x) = Fix2(gt, x)

"""
    ge(x, y)

Equivalent to `>=` but if `x` and `y` are both static returns a `StaticBool.
"""
ge(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x >= y)
ge(x) = Fix2(ge, x)

"""
    le(x, y)

Equivalent to `<=` but if `x` and `y` are both static returns a `StaticBool.
"""
le(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x <= y)
le(x) = Fix2(le, x)

"""
    lt(x, y)

Equivalent to `<` but if `x` and `y` are both static returns a `StaticBool.
"""
lt(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x < y)
lt(x) = Fix2(lt, x)

"""
    mul(x) -> Base.Fix2(*, x)
    mul(x, y) -> 

Equivalent to `*` but allows for lazy multiplication when passing functions.
"""
mul(x) = Fix2(*, x)
const Mul{X} = Fix2{typeof(*),X}

"""
    add(x) -> Base.Fix2(+, x)
    add(x, y) -> 

Equivalent to `+` but allows for lazy addition when passing functions.
"""
add(x) = Fix2(+, x)
const Add{X} = Fix2{typeof(+),X}

import Base: ∘
const compose = ∘
compose(::Add{StaticInt{X}}, ::Add{StaticInt{Y}}) where {X,Y} = Fix2(+, static(X + Y))
compose(x::Mul{Int}, ::Add{StaticInt{0}}) = x
compose(x::Mul{StaticInt{X}}, ::Add{StaticInt{0}}) where {X} = x
compose(x::Mul{StaticInt{0}}, ::Add{StaticInt{0}}) = x
compose(x::Mul{StaticInt{1}}, ::Add{StaticInt{0}}) = x
compose(x::Mul{StaticInt{0}}, y::Add{StaticInt{Y}}) where {Y} = x
compose(::Mul{StaticInt{1}}, y::Add{StaticInt{Y}}) where {Y} = y
compose(x::Mul{StaticInt{0}}, y::Add{Int}) = x
compose(::Mul{StaticInt{1}}, y::Add{Int}) = y
compose(::Mul{StaticInt{X}}, ::Mul{StaticInt{Y}}) where {X,Y} = Fix2(*, static(X * Y))
compose(x::Mul{StaticInt{0}}, y::Mul{Int}) = x
compose(::Mul{Int}, y::Mul{StaticInt{0}}) = y
compose(::Mul{StaticInt{1}}, y::Mul{Int}) = y
compose(x::Mul{Int}, ::Mul{StaticInt{1}}) = x


Base.show(io::IO, @nospecialize(x::Union{StaticInt,StaticSymbol,StaticFloat64,True,False})) = show(io, MIME"text/plain"(), x)
function Base.show(io::IO, ::MIME"text/plain", @nospecialize(x::Union{StaticInt,StaticSymbol,StaticFloat64,True,False}))
    print(io, "static(" * repr(known(typeof(x))) * ")")
end

# This method assumes that `f` uetrieves compile time information and `g` is the fall back
# for the corresponding dynamic method. If the `f(x)` doesn't return `nothing` that means
# the value is known and compile time and returns `static(f(x))`.
@inline function maybe_static(f::F, g::G, x) where {F,G}
    L = f(x)
    if L === nothing
        return g(x)
    else
        return static(L)
    end
end

#=
Base.:(==)(@nospecialize(x::StaticSymbol), @nospecialize(y::StaticSymbol)) = x === y
Base.:(==)(@nospecialize(x::StaticSymbol), y::Symbol) = known(typeof(x)) === y
Base.:(==)(x::Symbol, @nospecialize(y::StaticSymbol)) = x === known(typeof(y))
=#

end
