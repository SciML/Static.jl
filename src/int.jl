
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

Base.show(io::IO, @nospecialize(x::StaticInt)) = print(io, "static($(dynamic(x)))")

StaticInt(N::Int) = StaticInt{N}()
StaticInt(N::Integer) = StaticInt(convert(Int, N))
StaticInt(::StaticInt{N}) where {N} = StaticInt{N}()
StaticInt(::Val{N}) where {N} = StaticInt{N}()
# Base.Val(::StaticInt{N}) where {N} = Val{N}()
Base.convert(::Type{T}, @nospecialize(x::StaticInt)) where {T<:Number} = convert(T, dynamic(x))
Base.Bool(@nospecialize(x::StaticInt)) = Bool(dynamic(x))
Base.BigInt(@nospecialize(x::StaticInt)) = BigInt(dynamic(x))
Base.Integer(x::StaticInt{N}) where {N} = x
(::Type{T})(@nospecialize(x::StaticInt)) where {T<:Integer} = T(dynamic(x))
(::Type{T})(x::Int) where {T<:StaticInt} = StaticInt(x)
Base.convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()

Base.promote_rule(@nospecialize(x::Type{<:StaticInt}), ::Type{T}) where {T<:Number} = promote_type(Int, T)
function Base.promote_rule(@nospecialize(x::Type{<:StaticInt}), ::Type{T}) where {T<:AbstractIrrational}
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
Base.promote_rule(::Type{T}, @nospecialize(x::Type{<:StaticInt})) where {T>:Nothing} = promote_type(T, Int)
Base.promote_rule(::Type{T}, @nospecialize(x::Type{<:StaticInt})) where {T>:Missing} = promote_type(T, Int)
for T in [:Bool, :Missing, :BigFloat, :BigInt, :Nothing, :Any]
    # let S = :Any
    @eval begin
        function Base.promote_rule(@nospecialize(x::Type{<:StaticInt}), ::Type{$T})
            return promote_type(Int, $T)
        end
        function Base.promote_rule(::Type{$T}, @nospecialize(x::Type{<:StaticInt}))
            return promote_type($T, Int)
        end
    end
end
Base.promote_rule(@nospecialize(x::Type{<:StaticInt}), @nospecialize(y::Type{<:StaticInt})) = Int
Base.:(%)(@nospecialize(x::StaticInt), ::Type{Integer}) = dynamic(x)

Base.eltype(@nospecialize(x::Type{<:StaticInt})) = Int
Base.iszero(::Zero) = true
Base.iszero(@nospecialize(x::StaticInt)) = false
Base.isone(::One) = true
Base.isone(@nospecialize(x::StaticInt)) = false
Base.zero(@nospecialize(x::Type{<:StaticInt})) = Zero()
Base.one(@nospecialize(x::Type{<:StaticInt})) = One()

for T in [:Real, :Rational, :Integer]
    @eval begin
        @inline Base.:(+)(i::$T, ::Zero) = i
        @inline Base.:(+)(i::$T, @nospecialize(m::StaticInt)) = i + dynamic(m)
        @inline Base.:(+)(::Zero, i::$T) = i
        @inline Base.:(+)(@nospecialize(m::StaticInt), i::$T) = dynamic(m) + i
        @inline Base.:(-)(i::$T, ::Zero) = i
        @inline Base.:(-)(i::$T, @nospecialize(m::StaticInt)) = i - dynamic(m)
        @inline Base.:(*)(i::$T, ::Zero) = Zero()
        @inline Base.:(*)(i::$T, ::One) = i
        @inline Base.:(*)(i::$T, @nospecialize(m::StaticInt)) = i * dynamic(m)
        @inline Base.:(*)(::Zero, i::$T) = Zero()
        @inline Base.:(*)(::One, i::$T) = i
        @inline Base.:(*)(@nospecialize(m::StaticInt), i::$T) = dynamic(m) * i
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
        @inline Base.$f(@nospecialize(m::StaticInt), x::Int) = $f(dynamic(m), x)
        @inline Base.$f(x::Int, @nospecialize(m::StaticInt))  = $f(x, dynamic(m))
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

@inline Base.widen(@nospecialize(x::StaticInt)) = widen(dynamic(x))

Base.UnitRange{T}(@nospecialize(start::StaticInt), stop) where {T<:Real} = UnitRange{T}(T(start), T(stop))
Base.UnitRange{T}(start, @nospecialize(stop::StaticInt)) where {T<:Real} = UnitRange{T}(T(start), T(stop))
function Base.UnitRange{T}(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt)) where {T<:Real}
    return UnitRange{T}(T(start), T(stop))
end

Base.UnitRange(@nospecialize(start::StaticInt), stop) = UnitRange(Int(start), stop)
Base.UnitRange(start, @nospecialize(stop::StaticInt)) = UnitRange(start, Int(stop))
Base.UnitRange(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt)) = UnitRange(Int(start), Int(stop))

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

