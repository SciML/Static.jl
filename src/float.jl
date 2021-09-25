
"""
    StaticFloat64{N}

A statically sized `Float64`.
Use `StaticInt(N)` instead of `Val(N)` when you want it to behave like a number.
"""
struct StaticFloat64{N} <: AbstractFloat
    StaticFloat64{N}() where {N} = new{N::Float64}()
    StaticFloat64(x::Float64) = new{x}()
    StaticFloat64(x::Int) = new{Base.sitofp(Float64, x)::Float64}()
end

(::Type{T})(x::Integer) where {T<:StaticFloat64} = StaticFloat64(x)
(::Type{T})(x::AbstractFloat) where {T<:StaticFloat64} = StaticFloat64(x)
@generated function Base.AbstractFloat(::StaticInt{N}) where {N}
    Expr(:call, Expr(:curly, :StaticFloat64, Float64(N)))
end
StaticFloat64(x::StaticInt{N}) where {N} = float(x)

const FloatOne = StaticFloat64{one(Float64)}
const FloatZero = StaticFloat64{zero(Float64)}

Base.show(io::IO, @nospecialize(x::StaticFloat64)) = print(io, "static($(dynamic(x)))")

Base.convert(::Type{T}, @nospecialize(x::StaticFloat64)) where {T<:AbstractFloat} = T(dynamic(x))
Base.promote_rule(x::Type{<:StaticFloat64}, ::Type{T}) where {T} = promote_type(T, Float64)
Base.promote_rule(@nospecialize(x::Type{<:StaticFloat64}), ::Type{Float64})  = Float64
Base.promote_rule(@nospecialize(x::Type{<:StaticFloat64}), ::Type{Float32}) = Float32
Base.promote_rule(@nospecialize(x::Type{<:StaticFloat64}), ::Type{Float16}) = Float16

@static if VERSION == v"1.2"
    Base.promote_rule(::Type{StaticFloat64{N}}, ::Type{Any}) where {N} = Any
end

Base.eltype(@nospecialize(x::Type{<:StaticFloat64})) = Float64
Base.iszero(::FloatZero) = true
Base.iszero(@nospecialize(x::StaticFloat64)) = false
Base.isone(::FloatOne) = true
Base.isone(@nospecialize(x::StaticFloat64)) = false
Base.zero(@nospecialize(x::Type{<:StaticFloat64})) = FloatZero()
Base.one(@nospecialize(x::Type{<:StaticFloat64})) = FloatOne()

Base.@pure function fsub(::StaticFloat64{X}, ::StaticFloat64{Y}) where {X,Y}
    return StaticFloat64{Base.sub_float(X, Y)::Float64}()
end

Base.@pure function fadd(::StaticFloat64{X}, ::StaticFloat64{Y}) where {X,Y}
    return StaticFloat64{Base.add_float(X, Y)::Float64}()
end

Base.@pure function fdiv(::StaticFloat64{X}, ::StaticFloat64{Y}) where {X,Y}
    return StaticFloat64{Base.div_float(X, Y)::Float64}()
end

Base.@pure function fmul(::StaticFloat64{X}, ::StaticFloat64{Y}) where {X,Y}
    return StaticFloat64{Base.mul_float(X, Y)::Float64}()
end

Base.:+(x::StaticFloat64{X}, y::StaticFloat64{Y}) where {X,Y} = fadd(x, y)
Base.:+(x::StaticFloat64{X}, y::StaticInt{Y}) where {X,Y} = +(x, float(y))
Base.:+(x::StaticInt{X}, y::StaticFloat64{Y}) where {X,Y} = +(float(x), y)
Base.:+(x::FloatZero, ::FloatZero) = x
Base.:+(x::StaticFloat64{X}, ::FloatZero) where {X} = x
Base.:+(::FloatZero, y::StaticFloat64{Y}) where {Y} = y
Base.:+(x::StaticFloat64{X}, ::Zero) where {X} = x
Base.:+(::Zero, y::StaticFloat64{Y}) where {Y} = y

Base.:-(::StaticFloat64{X}) where {X} = StaticFloat64{-X}()
Base.:-(x::StaticFloat64{X}, y::StaticFloat64{Y}) where {X,Y} = fsub(x, y)
Base.:-(x::StaticFloat64{X}, y::StaticInt{Y}) where {X,Y} = -(x, float(y))
Base.:-(x::StaticInt{X}, y::StaticFloat64{Y}) where {X,Y} = -(float(x), y)
Base.:-(x::FloatZero, ::FloatZero) = x
Base.:-(x::StaticFloat64{X}, ::FloatZero) where {X} = x
Base.:-(x::StaticFloat64{X}, ::Zero) where {X} = x
Base.:-(::FloatZero, y::StaticFloat64{Y}) where {Y} = -y
Base.:-(::Zero, y::StaticFloat64{Y}) where {Y} = -y

Base.:*(x::StaticFloat64{X}, y::StaticFloat64{Y}) where {X,Y} = fmul(x, y)
Base.:*(x::StaticFloat64{X}, y::StaticInt{Y}) where {X,Y} = *(x, float(y))
Base.:*(::StaticFloat64{X}, ::Zero) where {X} = FloatZero()
Base.:*(::Zero, ::StaticFloat64{Y}, ) where {Y} = FloatZero()
Base.:*(x::StaticFloat64{X}, ::One) where {X} = x
Base.:*(x::StaticInt{X}, y::StaticFloat64{Y}) where {X,Y} = *(float(x), y)
Base.:*(::One, y::StaticFloat64{Y}) where {Y} = y
Base.:*(x::FloatZero, ::FloatZero) = x
Base.:*(::StaticFloat64{X}, y::FloatZero) where {X} = y
Base.:*(x::FloatZero, ::StaticFloat64{Y}) where {Y} = x
Base.:*(x::FloatZero, ::FloatOne) = x
Base.:*(x::FloatOne, ::FloatOne) = x
Base.:*(x::StaticFloat64{X}, ::FloatOne) where {X} = x
Base.:*(::FloatOne, y::StaticFloat64{Y}) where {Y} = y
Base.:*(::FloatOne, y::FloatZero) = y


Base.:/(x::StaticFloat64{X}, y::StaticFloat64{Y}) where {X,Y} = fdiv(x, y)
Base.:/(x::StaticFloat64{X}, y::StaticInt{Y}) where {X,Y} = /(x, float(y))
Base.:/(x::StaticInt{X}, y::StaticFloat64{Y}) where {X,Y} = /(float(x), y)

@generated Base.sqrt(::StaticInt{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat64, sqrt(M)))
@generated Base.sqrt(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat64, sqrt(M)))

@generated Base.round(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat64, round(M)))
@generated roundtostaticint(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, round(Int, M)))
roundtostaticint(x::AbstractFloat) = round(Int, x)
@generated floortostaticint(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, floor(Int, M)))
floortostaticint(x::AbstractFloat) = Base.fptosi(Int, x)

Base.:(^)(::StaticFloat64{x}, y::Float64) where {x} = exp2(log2(x) * y)

Base.inv(x::StaticFloat64{N}) where {N} = fdiv(one(x), x)

