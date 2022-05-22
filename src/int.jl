
"""
    StaticInt(N::Int) -> StaticInt{N}()

A statically sized `Int`.
Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticInt{N} <: Integer
    StaticInt{N}() where {N} = new{N::Int}()
    StaticInt(N::Int) = new{N}()
    StaticInt(N::Integer) = new{convert(Int, N)}()
    StaticInt(::Val{N}) where {N} = StaticInt(N)
    StaticInt(@nospecialize(N::StaticInt)) = N
end

const Zero = StaticInt{0}
const One = StaticInt{1}

Base.convert(::Type{T}, @nospecialize(N::StaticInt)) where {T<:AbstractFloat} = T(known(N))
#Base.convert(::Type{T}, @nospecialize(N::StaticInt)) where {T<:Number} = convert(T, Int(N))
Base.Bool(x::StaticInt{N}) where {N} = Bool(N)

Base.Rational{T}(@nospecialize(x::StaticInt)) where {T<:Integer} = Rational{T}(known(x))

Base.BigInt(@nospecialize(x::StaticInt)) = BigInt(known(x))
Base.Integer(@nospecialize(x::StaticInt)) = x
#(::Type{T})(@nospecialize(x::StaticInt)) where {T<:Integer} = T(known(x))
(::Type{T})(x::Int) where {T<:StaticInt} = StaticInt{x}()
#Base.convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()

#=
Base.promote_rule(@nospecialize(T1::Type{<:StaticInt}), ::Type{T2}) where {T2<:Number} = promote_type(Int, T2)
Base.promote_rule(@nospecialize(T1::Type{<:StaticInt}), ::Type{T2}) where {T2<:AbstractIrrational} = promote_type(Int, T2)
for (S, T) in [(:Complex, :Real), (:Rational, :Integer), (:(Base.TwicePrecision), :Any)]
    @eval function Base.promote_rule(::Type{$S{T}}, @nospecialize(SI::Type{<:StaticInt})) where {T<:$T}
        promote_type($S{T}, Int)
    end
end

Base.promote_rule(::Type{Union{Nothing,Missing}}, @nospecialize(T::Type{<:StaticInt})) = Union{Nothing,Missing,Int}
Base.promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Union{Missing,Nothing}} = promote_type(T1, Int)
Base.promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Nothing} = promote_type(T1, Int)
Base.promote_rule(::Type{T1}, @nospecialize(T2::Type{<:StaticInt})) where {T1>:Missing} = promote_type(T1, Int)
for T in [:Bool, :Missing, :BigFloat, :BigInt, :Nothing, :Any]
    # let S = :Any
    @eval begin
        Base.promote_rule(@nospecialize(S::Type{<:StaticInt}), ::Type{$T}) = promote_type(Int, $T)
        Base.promote_rule(::Type{$T}, @nospecialize(S::Type{<:StaticInt})) = promote_type($T, Int)
    end
end
Base.promote_rule(@nospecialize(T1::Type{<:StaticInt}), @nospecialize(T2::Type{<:StaticInt})) = Int
=#

Base.promote(x, @nospecialize(y::StaticInt)) = promote(x, known(y))
Base.promote(@nospecialize(x::StaticInt), y) = promote(known(x), y)
Base.promote(@nospecialize(x::StaticInt), @nospecialize(y::StaticInt)) = x, y
#=
function Base.promote(x, y)
    @inline
    px, py = _promote(x, y)
    Base.not_sametype((x,y), (px,py))
    px, py
end
function _promote(x::T, y::S) where {T,S}
    @inline
    R = promote_type(T, S)
    return (convert(R, x), convert(R, y))
end
=#

Base.rem(@nospecialize(n::StaticInt), ::Type{Int}) = known(n)
Base.:(%)(@nospecialize(n::StaticInt), ::Type{Integer}) = known(n)

Base.eltype(@nospecialize(T::Type{<:StaticInt})) = Int
Base.iszero(::Zero) = true
Base.iszero(@nospecialize(x::StaticInt)) = false
Base.isone(::One) = true
Base.isone(@nospecialize(x::StaticInt)) = false
Base.zero(@nospecialize(x::Type{<:StaticInt})) = Zero()
Base.one(@nospecialize(x::Type{<:StaticInt})) = One()


for T in [:Real, :Rational, :Integer]
    for f in [:(-), :(+), :(*), :min, :max, :div, :rem]
        @eval begin
            Base.$(f)(x::$T, @nospecialize(y::StaticInt)) = $(f)(x, known(y))
            Base.$(f)(@nospecialize(x::StaticInt), y::$T) = $(f)(known(x), y)
        end
    end
    @eval begin
        Base.:(*)(::$T, ::Zero) = Zero()
        Base.:(*)(::Zero, ::$T) = Zero()
    end
end
Base.:(*)(@nospecialize(x::StaticInt), ::Zero) = Zero()
Base.:(*)(::Zero, @nospecialize(y::StaticInt)) = Zero()
Base.:(*)(::Zero, ::Zero) = Zero()

@inline Base.:(-)(::StaticInt{M}) where {M} = StaticInt{-M}()

for f in [:(+), :(-), :(*), :(÷), :(%), :(<<), :(>>), :(>>>), :(&), :(|), :(⊻), :min, :max]
    eval(:(Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = StaticInt{$f(M,N)}()))
end

Base.minmax(x::StaticInt, y::StaticInt) = y < x ? (y, x) : (x, y)

for f in [:(<<), :(>>), :(>>>)]
    @eval begin
        Base.$f(@nospecialize(x::StaticInt), y::UInt) = $f(known(x), y)
        Base.$f(x::Integer, @nospecialize(y::StaticInt)) = $f(x, known(y))
    end
end
for f in [:(==), :(!=), :(<), :(≤), :(>), :(≥)]
    @eval begin
        Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = $f(M, N)
        Base.$f(@nospecialize(x::StaticInt), y::Int) = $f(known(x), y)
        Base.$f(x::Int, @nospecialize(y::StaticInt)) = $f(x, known(y))
    end
end

Base.widen(@nospecialize(x::StaticInt)) = widen(known(x))

function Base.UnitRange{T}(@nospecialize(start::StaticInt), stop) where {T<:Real}
    UnitRange{T}(T(dynamic(start)), T(stop))
end
function Base.UnitRange{T}(start, @nospecialize(stop::StaticInt)) where {T<:Real}
    UnitRange{T}(T(start), T(dynamic(stop)))
end
function Base.UnitRange{T}(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt)) where {T<:Real}
    UnitRange{T}(T(dynamic(start)), T(dynamic(stop)))
end
Base.UnitRange(@nospecialize(start::StaticInt), stop) = UnitRange(known(start), stop)
Base.UnitRange(start, @nospecialize(stop::StaticInt)) = UnitRange(start, known(stop))
function Base.UnitRange(@nospecialize(start::StaticInt), @nospecialize(stop::StaticInt))
    UnitRange(known(start), known(stop))
end

Base.getindex(x::Tuple, ::StaticInt{N}) where {N} = getfield(x, N)

