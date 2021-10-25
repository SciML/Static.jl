
"""
    StaticInt(N::Int) -> StaticInt{N}()

A statically sized `Int`.
Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticInt{N}
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
# Base.convert(::Type{T}, ::StaticInt{N}) where {T<:Number,N} = convert(T, N)
Base.Bool(x::StaticInt{N}) where {N} = Bool(N)
Base.BigInt(x::StaticInt{N}) where {N} = BigInt(N)
Base.Integer(x::StaticInt{N}) where {N} = x

(::Type{T})(x::StaticInt{N}) where {T<:Integer,N} = T(N)
(::Type{T})(x::Int) where {T<:StaticInt} = StaticInt(x)
Base.convert(::Type{StaticInt{N}}, ::StaticInt{N}) where {N} = StaticInt{N}()
Base.convert(::Type{T}, ::StaticInt{N}) where {T<:Number, N} = T(N)

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
    @eval begin
        @generated function Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N}
              return Expr(:call, Expr(:curly, :StaticInt, $f(M, N)))
        end
        @inline Base.$f(::StaticInt{N}, x::Number) where {N} = $f(N, x)
        @inline Base.$f(x::Number, ::StaticInt{N}) where {N} = $f(x, N)
    end
end
for f in [:(<<), :(>>), :(>>>)]
    @eval begin
        @inline Base.$f(::StaticInt{M}, x::UInt) where {M} = $f(M, x)
        @inline Base.$f(x::Integer, ::StaticInt{M}) where {M} = $f(x, M)
    end
end
for f in [:(==), :(!=), :(<), :(≤), :(>), :(≥), :isless, :min, :max]
    @eval begin
        @inline Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = $f(M, N)
        @inline Base.$f(::StaticInt{N}, x::Number) where {N} = $f(N, x)
        @inline Base.$f(x::Number, ::StaticInt{N}) where {N} = $f(x, N)
    end
end

@inline Base.:(^)(::StaticInt{M}, ::StaticInt{N}) where {M,N} = M^N
@inline Base.:(^)(::StaticInt{N}, x::Number) where {N} = N^x
@inline Base.:(^)(x::Number, ::StaticInt{N}) where {N} = Base.literal_pow(^, x, Val{N}())

@inline function maybe_static(f::F, g::G, x) where {F,G}
    L = f(x)
    if L === nothing
        return g(x)
    else
        return static(L)
    end
end

Base.zero(::StaticInt) = Zero()
Base.one(::StaticInt) = One()
Base.getindex(::StaticInt{N}, args...) where {N} = StaticInt{N}()
Base.@propagate_inbounds Base.getindex(t::Tuple, ::StaticInt{N}) where {N} = t[N]
for T ∈ [AbstractArray, LinearAlgebra.AbstractQ]
  @eval begin
    Base.@propagate_inbounds Base.getindex(A::$T, ::StaticInt{N}, args::Vararg{Any,K}) where {N,K} = A[N, args...]
    Base.@propagate_inbounds Base.getindex(A::$T, i::Union{Colon,AbstractArray,Integer}, ::StaticInt{N}, args::Vararg{Any,K}) where {N,K} = A[i, N, args...]
    Base.@propagate_inbounds Base.getindex(A::$T, i::Union{Colon,AbstractArray,Integer}, j::Union{Colon,AbstractArray,Integer}, ::StaticInt{N}, args::Vararg{Any,K}) where {N,K} = A[i, j, N, args...]
    Base.@propagate_inbounds Base.getindex(A::$T, i::Union{Colon,AbstractArray,Integer}, j::Union{Colon,AbstractArray,Integer}, k::Union{Colon,AbstractArray,Integer}, ::StaticInt{N}, args::Vararg{Any,K}) where {N,K} = A[i, j, k, N, args...]
    Base.@propagate_inbounds Base.getindex(A::$T, i::Union{Colon,AbstractArray,Integer}, j::Union{Colon,AbstractArray,Integer}, k::Union{Colon,AbstractArray,Integer}, l::Union{Colon,AbstractArray,Integer}, ::StaticInt{N}, args::Vararg{Any,K}) where {N,K} = A[i, j, k, l, N, args...]
  end
end


Base.@propagate_inbounds Base.getindex(A::SparseArrays.AbstractSparseMatrixCSC, ::StaticInt{N}, ::Colon) where {N} = A[N, :]
Base.@propagate_inbounds Base.getindex(A::SparseArrays.AbstractSparseMatrixCSC, ::Colon, ::StaticInt{N}) where {N} = A[:, N]

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

