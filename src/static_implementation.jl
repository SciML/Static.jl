
"""
    StaticInt(N::Int) -> StaticInt{N}()

A statically sized `Int`.
Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticInt{N} <: Integer
    StaticInt{N}() where {N} = new{N::Int}()
end

const Zero = StaticInt{0}
const One = StaticInt{1}

Base.show(io::IO, ::StaticInt{N}) where {N} = print(io, "static($N)")

StaticInt(N::Int) = StaticInt{N}()
StaticInt(N::Integer) = StaticInt(convert(Int, N))
StaticInt(::StaticInt{N}) where {N} = StaticInt{N}()
StaticInt(::Val{N}) where {N} = StaticInt{N}()
# Base.Val(::StaticInt{N}) where {N} = Val{N}()
Base.convert(::Type{T}, ::StaticInt{N}) where {T<:Number,N} = convert(T, N)
Base.Bool(x::StaticInt{N}) where {N} = Bool(N)
Base.BigInt(x::StaticInt{N}) where {N} = BigInt(N)
Base.Integer(x::StaticInt{N}) where {N} = x
(::Type{T})(x::StaticInt{N}) where {T<:Integer,N} = T(N)
(::Type{T})(x::Int) where {T<:StaticInt} = StaticInt(x)
Base.convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()

Base.promote_rule(::Type{<:StaticInt}, ::Type{T}) where {T<:Number} = promote_type(Int, T)
function Base.promote_rule(::Type{<:StaticInt}, ::Type{T}) where {T<:AbstractIrrational}
    return promote_type(Int, T)
end
# Base.promote_rule(::Type{T}, ::Type{<:StaticInt}) where {T <: AbstractIrrational} = promote_rule(T, Int)
for (S, T) in [(:Complex, :Real), (:Rational, :Integer), (:(Base.TwicePrecision), :Any)]
    @eval function Base.promote_rule(::Type{$S{T}}, ::Type{<:StaticInt}) where {T<:$T}
        return promote_type($S{T}, Int)
    end
end
function Base.promote_rule(::Type{Union{Nothing,Missing}}, ::Type{<:StaticInt})
    return Union{Nothing,Missing,Int}
end
function Base.promote_rule(::Type{T}, ::Type{<:StaticInt}) where {T>:Union{Missing,Nothing}}
    return promote_type(T, Int)
end
Base.promote_rule(::Type{T}, ::Type{<:StaticInt}) where {T>:Nothing} = promote_type(T, Int)
Base.promote_rule(::Type{T}, ::Type{<:StaticInt}) where {T>:Missing} = promote_type(T, Int)
for T in [:Bool, :Missing, :BigFloat, :BigInt, :Nothing, :Any]
    # let S = :Any
    @eval begin
        function Base.promote_rule(::Type{S}, ::Type{$T}) where {S<:StaticInt}
            return promote_type(Int, $T)
        end
        function Base.promote_rule(::Type{$T}, ::Type{S}) where {S<:StaticInt}
            return promote_type($T, Int)
        end
    end
end
Base.promote_rule(::Type{<:StaticInt}, ::Type{<:StaticInt}) = Int
Base.:(%)(::StaticInt{N}, ::Type{Integer}) where {N} = N

Base.eltype(::Type{T}) where {T<:StaticInt} = Int
Base.iszero(::Zero) = true
Base.iszero(::StaticInt) = false
Base.isone(::One) = true
Base.isone(::StaticInt) = false
Base.zero(::Type{T}) where {T<:StaticInt} = Zero()
Base.one(::Type{T}) where {T<:StaticInt} = One()

for T in [:Real, :Rational, :Integer]
    @eval begin
        @inline Base.:(+)(i::$T, ::Zero) = i
        @inline Base.:(+)(i::$T, ::StaticInt{M}) where {M} = i + M
        @inline Base.:(+)(::Zero, i::$T) = i
        @inline Base.:(+)(::StaticInt{M}, i::$T) where {M} = M + i
        @inline Base.:(-)(i::$T, ::Zero) = i
        @inline Base.:(-)(i::$T, ::StaticInt{M}) where {M} = i - M
        @inline Base.:(*)(i::$T, ::Zero) = Zero()
        @inline Base.:(*)(i::$T, ::One) = i
        @inline Base.:(*)(i::$T, ::StaticInt{M}) where {M} = i * M
        @inline Base.:(*)(::Zero, i::$T) = Zero()
        @inline Base.:(*)(::One, i::$T) = i
        @inline Base.:(*)(::StaticInt{M}, i::$T) where {M} = M * i
    end
end
@inline Base.:(+)(::Zero, ::Zero) = Zero()
@inline Base.:(+)(::Zero, ::StaticInt{M}) where {M} = StaticInt{M}()
@inline Base.:(+)(::StaticInt{M}, ::Zero) where {M} = StaticInt{M}()


@inline Base.:(-)(::StaticInt{M}) where {M} = StaticInt{-M}()
@inline Base.:(-)(::StaticInt{M}, ::Zero) where {M} = StaticInt{M}()

@inline Base.:(*)(::Zero, ::Zero) = Zero()
@inline Base.:(*)(::One, ::Zero) = Zero()
@inline Base.:(*)(::Zero, ::One) = Zero()
@inline Base.:(*)(::One, ::One) = One()
@inline Base.:(*)(::StaticInt{M}, ::Zero) where {M} = Zero()
@inline Base.:(*)(::Zero, ::StaticInt{M}) where {M} = Zero()
@inline Base.:(*)(::StaticInt{M}, ::One) where {M} = StaticInt{M}()
@inline Base.:(*)(::One, ::StaticInt{M}) where {M} = StaticInt{M}()
for f in [:(+), :(-), :(*), :(/), :(÷), :(%), :(<<), :(>>), :(>>>), :(&), :(|), :(⊻)]
    @eval @generated function Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N}
        return Expr(:call, Expr(:curly, :StaticInt, $f(M, N)))
    end
end
for f in [:(<<), :(>>), :(>>>)]
    @eval begin
        @inline Base.$f(::StaticInt{M}, x::UInt) where {M} = $f(M, x)
        @inline Base.$f(x::Integer, ::StaticInt{M}) where {M} = $f(x, M)
    end
end
for f in [:(==), :(!=), :(<), :(≤), :(>), :(≥)]
    @eval begin
        @inline Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = $f(M, N)
        @inline Base.$f(::StaticInt{M}, x::Int) where {M} = $f(M, x)
        @inline Base.$f(x::Int, ::StaticInt{M}) where {M} = $f(x, M)
    end
end

@inline function maybe_static(f::F, g::G, x) where {F,G}
    L = f(x)
    if L === nothing
        return g(x)
    else
        return static(L)
    end
end

@inline Base.widen(::StaticInt{N}) where {N} = widen(N)

Base.UnitRange{T}(start::StaticInt, stop) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange{T}(start, stop::StaticInt) where {T<:Real} = UnitRange{T}(T(start), T(stop))
function Base.UnitRange{T}(start::StaticInt, stop::StaticInt) where {T<:Real}
    return UnitRange{T}(T(start), T(stop))
end

Base.UnitRange(start::StaticInt, stop) = UnitRange(Int(start), stop)
Base.UnitRange(start, stop::StaticInt) = UnitRange(start, Int(stop))
Base.UnitRange(start::StaticInt, stop::StaticInt) = UnitRange(Int(start), Int(stop))

"""
    StaticBool(x::Bool) -> True/False

A statically typed `Bool`.
"""
abstract type StaticBool <: Integer end

StaticBool(x::StaticBool) = x

struct True <: StaticBool end

struct False <: StaticBool end

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

Base.:(~)(::True) = False()
Base.:(~)(::False) = True()
Base.:(!)(::True) = False()
Base.:(!)(::False) = True()

Base.:(==)(::True, ::True) = True()
Base.:(==)(::True, ::False) = False()
Base.:(==)(::False, ::True) = False()
Base.:(==)(::False, ::False) = True()

Base.:(|)(x::StaticBool, y::StaticBool) = _or(x, y)
_or(::True, ::False) = True()
_or(::False, ::True) = True()
_or(::True, ::True) = True()
_or(::False, ::False) = False()
Base.:(|)(x::Bool, y::True) = y
Base.:(|)(x::Bool, y::False) = x
Base.:(|)(x::True, y::Bool) = x
Base.:(|)(x::False, y::Bool) = y

Base.:(&)(x::StaticBool, y::StaticBool) = _and(x, y)
_and(::True, ::False) = False()
_and(::False, ::True) = False()
_and(::True, ::True) = True()
_and(::False, ::False) = False()
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
Base.iszero(::True) = False()
Base.iszero(::False) = True()
Base.isone(::True) = True()
Base.isone(::False) = False()

Base.:(<)(x::StaticBool, y::StaticBool) = _lt(x, y)
_lt(::False, ::True) = True()
_lt(::True, ::True) = False()
_lt(::False, ::False) = False()
_lt(::True, ::False) = False()

Base.:(<=)(x::StaticBool, y::StaticBool) = _lteq(x, y)
_lteq(::False, ::True) = True()
_lteq(::True, ::True) = True()
_lteq(::False, ::False) = True()
_lteq(::True, ::False) = False()

Base.:(+)(x::True) = One()
Base.:(+)(x::False) = Zero()
Base.:(-)(x::True) = -One()
Base.:(-)(x::False) = Zero()

Base.:(+)(x::StaticBool, y::StaticBool) = StaticInt(x) + StaticInt(y)
Base.:(-)(x::StaticBool, y::StaticBool) = StaticInt(x) - StaticInt(y)
Base.:(*)(x::StaticBool, y::StaticBool) = x & y

# from `^(x::Bool, y::Bool) = x | !y`
Base.:(^)(x::StaticBool, y::False) = True()
Base.:(^)(x::StaticBool, y::True) = x
Base.:(^)(x::Integer, y::False) = one(x)
Base.:(^)(x::Integer, y::True) = x
Base.:(^)(x::BigInt, y::False) = one(x)
Base.:(^)(x::BigInt, y::True) = x

Base.div(x::StaticBool, y::False) = throw(DivideError())
Base.div(x::StaticBool, y::True) = x

Base.rem(x::StaticBool, y::False) = throw(DivideError())
Base.rem(x::StaticBool, y::True) = False()
Base.mod(x::StaticBool, y::StaticBool) = rem(x, y)

Base.promote_rule(::Type{<:StaticBool}, ::Type{<:StaticBool}) = StaticBool
Base.promote_rule(::Type{<:StaticBool}, ::Type{Bool}) = Bool
Base.promote_rule(::Type{Bool}, ::Type{<:StaticBool}) = Bool

@generated _get_tuple(::Type{T}, ::StaticInt{i}) where {T<:Tuple, i} = T.parameters[i]

Base.all(::Tuple{Vararg{True}}) = true
Base.all(::Tuple{Vararg{Union{True,False}}}) = false
Base.all(::Tuple{Vararg{False}}) = false

Base.any(::Tuple{Vararg{True}}) = true
Base.any(::Tuple{Vararg{Union{True,False}}}) = true
Base.any(::Tuple{Vararg{False}}) = false

ifelse(::True, x, y) = x

ifelse(::False, x, y) = y

"""
    eq(x, y)

Equivalent to `!=` but if `x` and `y` are both static returns a `StaticBool.
"""
eq(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x == y)
eq(x::X) where {X} = Base.Fix2(eq, x)

"""
    ne(x, y)

Equivalent to `!=` but if `x` and `y` are both static returns a `StaticBool.
"""
ne(x::X, y::Y) where {X,Y} = !eq(x, y)
ne(x::X) where {X} = Base.Fix2(ne, x)

"""
    gt(x, y)

Equivalent to `>` but if `x` and `y` are both static returns a `StaticBool.
"""
gt(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x > y)
gt(x::X) where {X} = Base.Fix2(gt, x)

"""
    ge(x, y)

Equivalent to `>=` but if `x` and `y` are both static returns a `StaticBool.
"""
ge(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x >= y)
ge(x::X) where {X} = Base.Fix2(ge, x)

"""
    le(x, y)

Equivalent to `<=` but if `x` and `y` are both static returns a `StaticBool.
"""
le(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x <= y)
le(x::X) where {X} = Base.Fix2(le, x)

"""
    lt(x, y)

Equivalent to `<` but if `x` and `y` are both static returns a `StaticBool.
"""
lt(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x < y)
lt(x::X) where {X} = Base.Fix2(lt, x)

"""
    StaticSymbol

A statically typed `Symbol`.
"""
struct StaticSymbol{s}
    StaticSymbol{s}() where {s} = new{s::Symbol}()
    StaticSymbol(s::Symbol) = new{s}()
    StaticSymbol(x::StaticSymbol) = x
    StaticSymbol(x) = StaticSymbol(Symbol(x))
end
StaticSymbol(x, y) = StaticSymbol(Symbol(x, y))
StaticSymbol(x::StaticSymbol, y::StaticSymbol) = _cat_syms(x, y)
@generated function _cat_syms(::StaticSymbol{x}, ::StaticSymbol{y}) where {x,y}
    return :(StaticSymbol{$(QuoteNode(Symbol(x, y)))}())
end
StaticSymbol(x, y, z...) = StaticSymbol(StaticSymbol(x, y), z...)

Base.Symbol(::StaticSymbol{s}) where {s} = s::Symbol

Base.show(io::IO, ::StaticSymbol{s}) where {s} = print(io, "static(:$s)")


