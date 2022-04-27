
"""
    StaticBool(x::Bool) -> True/False

A statically typed `Bool`.
"""
abstract type StaticBool{bool} <: Integer end

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

Base.:(~)(::True) = False()
Base.:(~)(::False) = True()
Base.:(!)(::True) = False()
Base.:(!)(::False) = True()

Base.:(==)(::True, ::True) = true
Base.:(==)(::True, ::False) = false
Base.:(==)(::False, ::True) = false
Base.:(==)(::False, ::False) = true

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
_lt(::False, ::True) = true
_lt(::True, ::True) = false
_lt(::False, ::False) = false
_lt(::True, ::False) = false

Base.:(<=)(x::StaticBool, y::StaticBool) = _lteq(x, y)
_lteq(::False, ::True) = true
_lteq(::True, ::True) = true
_lteq(::False, ::False) = true
_lteq(::True, ::False) = false

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

Base.all(::Tuple{Vararg{True}}) = true
Base.all(::Tuple{Vararg{Union{True,False}}}) = false
Base.all(::Tuple{Vararg{False}}) = false

Base.any(::Tuple{Vararg{True}}) = true
Base.any(::Tuple{Vararg{Union{True,False}}}) = true
Base.any(::Tuple{Vararg{False}}) = false

ifelse(::True, x, y) = x

ifelse(::False, x, y) = y

