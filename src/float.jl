
const Float = Int === Int64 ? Float64 : Float32

struct StaticFloat{N} <: AbstractFloat
    StaticFloat{N}() where {N} = new{N::Float}()
    StaticFloat(x::Float) = new{x}()
    StaticFloat(x::Int) = new{Base.sitofp(Float, x)::Float}()
end

(::Type{T})(x::Integer) where {T<:StaticFloat} = StaticFloat(x)
(::Type{T})(x::AbstractFloat) where {T<:StaticFloat} = StaticFloat(x)
@generated function Base.float(::StaticInt{N}) where {N}
    Expr(:call, Expr(:curly, :StaticFloat, float(N)))
end
StaticFloat(x::StaticInt{N}) where {N} = float(x)

const FloatOne = StaticFloat{one(Float)}
const FloatZero = StaticFloat{zero(Float)}

Base.show(io::IO, ::StaticFloat{N}) where {N} = print(io, "static($N)")

Base.convert(::Type{T}, ::StaticFloat{N}) where {N,T<:AbstractFloat} = T(N)
Base.promote_rule(::Type{StaticFloat{N}}, ::Type{T}) where {N,T} = promote_type(T, Float)

Base.eltype(::Type{T}) where {T<:StaticFloat} = Float
Base.iszero(::FloatZero) = true
Base.iszero(::StaticFloat) = false
Base.isone(::FloatOne) = true
Base.isone(::StaticFloat) = false
Base.zero(::Type{T}) where {T<:StaticFloat} = FloatZero()
Base.one(::Type{T}) where {T<:StaticFloat} = FloatOne()

Base.@pure function fsub(::StaticFloat{X}, ::StaticFloat{Y}) where {X,Y}
    return StaticFloat{Base.sub_float(X, Y)::Float}()
end

Base.@pure function fadd(::StaticFloat{X}, ::StaticFloat{Y}) where {X,Y}
    return StaticFloat{Base.add_float(X, Y)::Float}()
end

Base.@pure function fdiv(::StaticFloat{X}, ::StaticFloat{Y}) where {X,Y}
    return StaticFloat{Base.div_float(X, Y)::Float}()
end

Base.@pure function fmul(::StaticFloat{X}, ::StaticFloat{Y}) where {X,Y}
    return StaticFloat{Base.mul_float(X, Y)::Float}()
end

Base.:+(x::StaticFloat{X}, y::StaticFloat{Y}) where {X,Y} = fadd(x, y)
Base.:+(x::StaticFloat{X}, y::StaticInt{Y}) where {X,Y} = +(x, float(y))
Base.:+(x::StaticInt{X}, y::StaticFloat{Y}) where {X,Y} = +(float(x), y)
Base.:+(x::FloatZero, ::FloatZero) = x
Base.:+(x::StaticFloat{X}, ::FloatZero) where {X} = x
Base.:+(::FloatZero, y::StaticFloat{Y}) where {Y} = y
Base.:+(x::StaticFloat{X}, ::Zero) where {X} = x
Base.:+(::Zero, y::StaticFloat{Y}) where {Y} = y

Base.:-(::StaticFloat{X}) where {X} = StaticFloat{-X}()
Base.:-(x::StaticFloat{X}, y::StaticFloat{Y}) where {X,Y} = fsub(x, y)
Base.:-(x::StaticFloat{X}, y::StaticInt{Y}) where {X,Y} = -(x, float(y))
Base.:-(x::StaticInt{X}, y::StaticFloat{Y}) where {X,Y} = -(float(x), y)
Base.:-(x::FloatZero, ::FloatZero) = x
Base.:-(x::StaticFloat{X}, ::FloatZero) where {X} = x
Base.:-(x::StaticFloat{X}, ::Zero) where {X} = x
Base.:-(::FloatZero, y::StaticFloat{Y}) where {Y} = -y
Base.:-(::Zero, y::StaticFloat{Y}) where {Y} = -y

Base.:*(x::StaticFloat{X}, y::StaticFloat{Y}) where {X,Y} = fmul(x, y)
Base.:*(x::StaticFloat{X}, y::StaticInt{Y}) where {X,Y} = *(x, float(y))
Base.:*(::StaticFloat{X}, ::Zero) where {X} = FloatZero()
Base.:*(::Zero, ::StaticFloat{Y}, ) where {Y} = FloatZero()
Base.:*(x::StaticFloat{X}, ::One) where {X} = x
Base.:*(x::StaticInt{X}, y::StaticFloat{Y}) where {X,Y} = *(float(x), y)
Base.:*(::One, y::StaticFloat{Y}) where {Y} = y
Base.:*(x::FloatZero, ::FloatZero) = x
Base.:*(::StaticFloat{X}, y::FloatZero) where {X} = y
Base.:*(x::FloatZero, ::StaticFloat{Y}) where {Y} = x
Base.:*(x::FloatZero, ::FloatOne) = x
Base.:*(x::FloatOne, ::FloatOne) = x
Base.:*(x::StaticFloat{X}, ::FloatOne) where {X} = x
Base.:*(::FloatOne, y::StaticFloat{Y}) where {Y} = y
Base.:*(::FloatOne, y::FloatZero) = y


Base.:/(x::StaticFloat{X}, y::StaticFloat{Y}) where {X,Y} = fdiv(x, y)
Base.:/(x::StaticFloat{X}, y::StaticInt{Y}) where {X,Y} = /(x, float(y))
Base.:/(x::StaticInt{X}, y::StaticFloat{Y}) where {X,Y} = /(float(x), y)

@generated Base.sqrt(::StaticInt{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat, sqrt(M)))
@generated Base.sqrt(::StaticFloat{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat, sqrt(M)))

@generated Base.round(::StaticFloat{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat, round(M)))
@generated roundtostaticint(::StaticFloat{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, round(Int, M)))
roundtostaticint(x::AbstractFloat) = round(Int, x)
@generated floortostaticint(::StaticFloat{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, floor(Int, M)))
floortostaticint(x::AbstractFloat) = Base.fptosi(Int, x)

Base.inv(x::StaticFloat{N}) where {N} = fdiv(one(x), x)

