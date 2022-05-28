module Static

import IfElse: ifelse

export StaticInt, StaticFloat64, StaticSymbol, True, False, StaticBool
export dynamic, is_static, known, static

#const BaseRealTypes = Union{AbstractFloat,AbstractIrrational,Integer,Rational}

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

abstract type StaticNumber{N} <: Number end

abstract type StaticInteger{N} <: StaticNumber{N} end

"""
    StaticInt(N::Int) -> StaticInt{N}()

A statically sized `Int`.
Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticInt{N} <: StaticInteger{N}
    StaticInt{N}() where {N} = new{N::Int}()
    StaticInt(N::Int) = new{N}()
    StaticInt(@nospecialize N::StaticInt) = N
    StaticInt(::Val{N}) where {N} = StaticInt(N)
end

Base.getindex(x::Tuple, ::StaticInt{N}) where {N} = getfield(x, N)

"""
    StaticFloat64{N}

A statically sized `Float64`.
Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticFloat64{N} <: StaticNumber{N}
    StaticFloat64{N}() where {N} = new{N::Float64}()
    StaticFloat64(x::Float64) = new{x}()
    StaticFloat64(x::Int) = new{Base.sitofp(Float64, x)::Float64}()
    StaticFloat64(x::StaticInt{N}) where {N} = StaticFloat64(convert(Float64, N))
    StaticFloat64(x::Complex) = StaticFloat64(convert(Float64, x))
end

"""
    StaticBool(x::Bool) -> True/False

A statically typed `Bool`.
"""
abstract type StaticBool{bool} <: StaticInteger{bool} end

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

ifelse(::True, x, y) = x
ifelse(::False, x, y) = y

StaticInt(x::False) = Zero()
StaticInt(x::True) = One()
Base.Bool(::True) = true
Base.Bool(::False) = false

const Zero = StaticInt{0}
const One = StaticInt{1}
const FloatOne = StaticFloat64{one(Float64)}
const FloatZero = StaticFloat64{zero(Float64)}

Base.eltype(@nospecialize(T::Type{<:StaticFloat64})) = Float64
Base.eltype(@nospecialize(T::Type{<:StaticInt})) = Int
Base.eltype(@nospecialize(T::Type{<:StaticBool})) = Bool

"""
    known(::Type{T})

Returns the known value corresponding to a static type `T`. If `T` is not a static type then
`nothing` is returned.

See also: [`static`](@ref), [`is_static`](@ref)
"""
known(::Type{<:StaticNumber{N}}) where {N} = N
known(@nospecialize(T::Type{<:StaticSymbol}))::Symbol = T.parameters[1]
known(::Type{Val{V}}) where {V} = V
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
static(@nospecialize(x::Union{StaticSymbol,StaticNumber})) = x
static(x::Integer) = StaticInt(x)
static(x::Union{AbstractFloat,Complex,Rational,AbstractIrrational}) = StaticFloat64(Float64(x))
static(x::Bool) = StaticBool(x)
static(x::Union{Symbol,AbstractChar,AbstractString}) = StaticSymbol(x)
static(x::Tuple{Vararg{Any}}) = map(static, x)
static(::Val{V}) where {V} = static(V)
@noinline static(x) = error("There is no static alternative for type $(typeof(x)).")

"""
    is_static(::Type{T}) -> StaticBool

Returns `True` if `T` is a static type.

See also: [`static`](@ref), [`known`](@ref)
"""
is_static(@nospecialize(x)) = is_static(typeof(x))
is_static(@nospecialize(x::Type{<:Union{StaticSymbol,StaticNumber}})) = True()
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

function Base.promote_rule(::Type{<:Base.TwicePrecision{R}}, @nospecialize(T::Type{<:StaticNumber})) where {R<:Number}
    promote_rule(Base.TwicePrecision{R}, eltype(T))
end

Base.:(~)(::StaticNumber{N}) where {N} = static(~N)

Base.inv(x::StaticNumber{N}) where {N} = one(x) / x

@inline Base.one(@nospecialize T::Type{<:StaticNumber}) = static(one(eltype(T)))
@inline Base.zero(@nospecialize T::Type{<:StaticNumber}) = static(zero(eltype(T)))
@inline Base.iszero(::Union{StaticInt{0},StaticFloat64{0.0},False}) = true
@inline Base.iszero(@nospecialize x::StaticNumber) = false
@inline Base.isone(::Union{One,FloatOne,True}) = true
@inline Base.isone(@nospecialize x::StaticNumber) = false

Base.AbstractFloat(x::StaticNumber) = StaticFloat64(x)

Base.abs(::StaticNumber{N}) where {N} = static(abs(N))
Base.abs2(::StaticNumber{N}) where {N} = static(abs2(N))
Base.sign(::StaticNumber{N}) where {N} = static(sign(N))

Base.widen(@nospecialize(x::StaticNumber)) = widen(known(x))

function Base.convert(::Type{T}, @nospecialize(N::StaticNumber)) where {T<:Number}
    convert(T, known(N))
end

#Base.Bool(::StaticInt{N}) where {N} = Bool(N)

Base.Integer(@nospecialize(x::StaticInt)) = x
(::Type{T})(x::StaticNumber) where {T<:Real} = T(known(x))
function (@nospecialize(T::Type{<:StaticNumber}))(x::Union{AbstractFloat,AbstractIrrational,Integer,Rational})
    static(convert(eltype(T), x))
end
#=
(@nospecialize(T::Type{<:StaticFloat64}))(x) = StaticFloat64(convert(Float64, x)::Float64)
(@nospecialize(T::Type{<:StaticBool}))(x) = StaticFloat64(convert(Bool, x)::Bool)
Base.convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()
=#


@inline Base.:(-)(::StaticNumber{N}) where {N} = static(-N)
Base.:(*)(::Union{AbstractFloat,AbstractIrrational,Integer,Rational}, y::Zero) = y
Base.:(*)(x::Zero, ::Union{AbstractFloat,AbstractIrrational,Integer,Rational}) = x
Base.:(*)(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(X * Y)
Base.:(/)(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(X / Y)
Base.:(-)(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(X - Y)
Base.:(+)(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(X + Y)

@generated Base.sqrt(::StaticNumber{N}) where {N} = :($(static(sqrt(N))))

Base.div(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(div(X, Y))
Base.div(x::Real, ::StaticNumber{Y}) where {Y} = div(x, Y)
Base.div(::StaticNumber{X}, y::Real) where {X} = div(X, y)
Base.div(x::StaticBool, y::False) = throw(DivideError())
Base.div(x::StaticBool, y::True) = x

Base.rem(@nospecialize(x::StaticNumber), T::Type{<:Integer}) = rem(known(x), T)
Base.rem(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(rem(X, Y))
Base.rem(x::Real, ::StaticNumber{Y}) where {Y} = rem(x, Y)
Base.rem(::StaticNumber{X}, y::Real) where {X} = rem(X, y)

Base.mod(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(mod(X, Y))

Base.round(::StaticFloat64{M}) where {M} = StaticFloat64(round(M))
roundtostaticint(::StaticFloat64{M}) where {M} = StaticInt(round(Int, M))
roundtostaticint(x::AbstractFloat) = round(Int, x)
floortostaticint(::StaticFloat64{M}) where {M} = StaticInt(Base.fptosi(Int, M))
floortostaticint(x::AbstractFloat) = Base.fptosi(Int, x)

Base.:(==)(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = ==(X, Y)

Base.:(<)(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = <(X, Y)

Base.isless(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = isless(X,Y)
Base.isless(::StaticNumber{X}, y::Real) where {X} = isless(X, y)
Base.isless(x::Real, ::StaticInteger{Y}) where {Y} = isless(x, Y)

Base.min(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(min(X, Y))
Base.min(::StaticNumber{X}, y::Number) where {X} = min(X, y)
Base.min(x::Number, ::StaticNumber{Y}) where {Y} = min(x, Y)

Base.max(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(max(X, Y))
Base.max(::StaticNumber{X}, y::Number) where {X} = max(X, y)
Base.max(x::Number, ::StaticNumber{Y}) where {Y} = max(x, Y)

Base.minmax(::StaticNumber{X}, ::StaticNumber{Y}) where {X,Y} = static(minmax(X, Y))

Base.:(<<)(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = static(<<(X,Y))
Base.:(<<)(::StaticInteger{X}, n::Integer) where {X} = <<(X,n)
Base.:(<<)(x::Integer, ::StaticInteger{N}) where {N} = <<(x,N)


Base.:(>>)(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = static(>>(X,Y))
Base.:(>>)(::StaticInteger{X}, n::Integer) where {X} = >>(X,n)
Base.:(>>)(x::Integer, ::StaticInteger{N}) where {N} = >>(x,N)

Base.:(>>>)(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = static(>>>(X,Y))
Base.:(>>>)(::StaticInteger{X}, n::Integer) where {X} = >>>(X, n)
Base.:(>>>)(x::Integer, ::StaticInteger{N}) where {N} = >>>(x,N)

Base.:(&)(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = static(X & Y)
Base.:(&)(::StaticInteger{X}, y::Union{Integer,Missing}) where {X} = X & y
Base.:(&)(x::Union{Integer,Missing}, ::StaticInteger{Y}) where {Y} = x & Y
Base.:(&)(x::Bool, y::True) = x
Base.:(&)(x::Bool, y::False) = y
Base.:(&)(x::True, y::Bool) = y
Base.:(&)(x::False, y::Bool) = x

Base.:(|)(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = static(|(X,Y))
Base.:(|)(::StaticInteger{X}, y::Union{Integer,Missing}) where {X} = X | y
Base.:(|)(x::Union{Integer,Missing}, ::StaticInteger{Y}) where {Y} = x | Y
Base.:(|)(x::Bool, y::True) = y
Base.:(|)(x::Bool, y::False) = x
Base.:(|)(x::True, y::Bool) = x
Base.:(|)(x::False, y::Bool) = y

Base.xor(::StaticInteger{X}, ::StaticInteger{Y}) where {X,Y} = static(xor(X,Y))
Base.xor(::StaticInteger{X}, y::Union{Integer,Missing}) where {X} = xor(X, y)
Base.xor(x::Union{Integer,Missing}, ::StaticInteger{Y}) where {Y} = xor(x, Y)

Base.:(!)(::True) = False()
Base.:(!)(::False) = True()

Base.UnitRange{T}(@nospecialize(start::StaticNumber), stop) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange{T}(start, @nospecialize(stop::StaticNumber)) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange{T}(@nospecialize(start::StaticNumber), @nospecialize(stop::StaticNumber)) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange(@nospecialize(start::StaticNumber), stop) = UnitRange(known(start), stop)
Base.UnitRange(start, @nospecialize(stop::StaticNumber)) = UnitRange(start, known(stop))
function Base.UnitRange(@nospecialize(start::StaticNumber), @nospecialize(stop::StaticNumber))
    UnitRange(known(start), known(stop))
end

Base.all(::Tuple{Vararg{True}}) = true
Base.all(::Tuple{Vararg{Union{True,False}}}) = false
Base.all(::Tuple{Vararg{False}}) = false

Base.any(::Tuple{Vararg{True}}) = true
Base.any(::Tuple{Vararg{Union{True,False}}}) = true
Base.any(::Tuple{Vararg{False}}) = false

"""
    field_type(::Type{T}, f)

Functionally equivalent to `fieldtype(T, f)` except `f` may be a static type.
"""
@inline field_type(T::Type, f::Union{Int,Symbol}) = fieldtype(T, f)
@inline field_type(::Type{T}, ::StaticInt{N}) where {T,N} = fieldtype(T, N)
@inline field_type(::Type{T}, ::StaticSymbol{S}) where {T,S} = fieldtype(T, S)

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


@inline Base.exponent(::StaticNumber{M}) where {M} = static(exponent(M))

Base.:(^)(::StaticFloat64{x}, y::Float64) where {x} = exp2(log2(x) * y)

Base.:(+)(x::True) = One()
Base.:(+)(x::False) = Zero()

# from `^(x::Bool, y::Bool) = x | !y`
Base.:(^)(x::StaticBool, y::False) = True()
Base.:(^)(x::StaticBool, y::True) = x
Base.:(^)(x::Integer, y::False) = one(x)
Base.:(^)(x::Integer, y::True) = x
Base.:(^)(x::BigInt, y::False) = one(x)
Base.:(^)(x::BigInt, y::True) = x


#=
Base.rem(x::StaticBool, y::False) = throw(DivideError())
Base.rem(x::StaticBool, y::True) = False()
Base.mod(x::StaticBool, y::StaticBool) = rem(x, y)

=#

@inline function invariant_permutation(@nospecialize(x::Tuple), @nospecialize(y::Tuple))
    if y === x === nstatic(Val(nfields(x)))
        return True()
    else
        return False()
    end
end

@inline nstatic(::Val{N}) where {N} = ntuple(StaticInt, Val(N))

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

"""
    eq(x, y)

Equivalent to `!=` but if `x` and `y` are both static returns a `StaticBool.
"""
eq(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x == y)
eq(x) = Base.Fix2(eq, x)

"""
    ne(x, y)

Equivalent to `!=` but if `x` and `y` are both static returns a `StaticBool.
"""
ne(x::X, y::Y) where {X,Y} = !eq(x, y)
ne(x) = Base.Fix2(ne, x)

"""
    gt(x, y)

Equivalent to `>` but if `x` and `y` are both static returns a `StaticBool.
"""
gt(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x > y)
gt(x) = Base.Fix2(gt, x)

"""
    ge(x, y)

Equivalent to `>=` but if `x` and `y` are both static returns a `StaticBool.
"""
ge(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x >= y)
ge(x) = Base.Fix2(ge, x)

"""
    le(x, y)

Equivalent to `<=` but if `x` and `y` are both static returns a `StaticBool.
"""
le(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x <= y)
le(x) = Base.Fix2(le, x)

"""
    lt(x, y)

Equivalent to `<` but if `x` and `y` are both static returns a `StaticBool.
"""
lt(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x < y)
lt(x) = Base.Fix2(lt, x)

"""
    mul(x) -> Base.Fix2(*, x)
    mul(x, y) ->

Equivalent to `*` but allows for lazy multiplication when passing functions.
"""
mul(x) = Base.Fix2(*, x)

"""
    add(x) -> Base.Fix2(+, x)
    add(x, y) ->

Equivalent to `+` but allows for lazy addition when passing functions.
"""
add(x) = Base.Fix2(+, x)

const Mul{X} = Base.Fix2{typeof(*),X}
const Add{X} = Base.Fix2{typeof(+),X}

Base.:∘(::Add{StaticInt{X}}, ::Add{StaticInt{Y}}) where {X,Y} = Base.Fix2(+, static(X + Y))
Base.:∘(x::Mul{Int}, ::Add{StaticInt{0}}) = x
Base.:∘(x::Mul{StaticInt{X}}, ::Add{StaticInt{0}}) where {X} = x
Base.:∘(x::Mul{StaticInt{0}}, ::Add{StaticInt{0}}) = x
Base.:∘(x::Mul{StaticInt{1}}, ::Add{StaticInt{0}}) = x
Base.:∘(x::Mul{StaticInt{0}}, y::Add{StaticInt{Y}}) where {Y} = x
Base.:∘(::Mul{StaticInt{1}}, y::Add{StaticInt{Y}}) where {Y} = y
Base.:∘(x::Mul{StaticInt{0}}, y::Add{Int}) = x
Base.:∘(::Mul{StaticInt{1}}, y::Add{Int}) = y
Base.:∘(::Mul{StaticInt{X}}, ::Mul{StaticInt{Y}}) where {X,Y} = Base.Fix2(*, static(X * Y))
Base.:∘(x::Mul{StaticInt{0}}, y::Mul{Int}) = x
Base.:∘(::Mul{Int}, y::Mul{StaticInt{0}}) = y
Base.:∘(::Mul{StaticInt{1}}, y::Mul{Int}) = y
Base.:∘(x::Mul{Int}, ::Mul{StaticInt{1}}) = x

Base.show(io::IO, @nospecialize(x::Union{StaticNumber,StaticSymbol})) = show(io, MIME"text/plain"(), x)
function Base.show(io::IO, ::MIME"text/plain", @nospecialize(x::Union{StaticNumber,StaticSymbol}))
    print(io, "static(" * repr(known(typeof(x))) * ")")
end

Base.to_index(x::StaticInt) = known(x)

end
