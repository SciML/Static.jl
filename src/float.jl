
const Float = Int === Int64 ? Float64 : Float32

struct StaticFloat{N} <: AbstractFloat
    StaticFloat{N}() where {N} = new{N::Float}()
    StaticFloat(x::Float) = new{N}()
end

const FloatOne = StaticFloat{one(Float)}
const FloatZero = StaticFloat{zero(Float)}

Base.@pure StaticFloat(x::Float) = StaticFloat{x}()
Base.@pure StaticFloat(x::Int) = StaticFloat{Base.sitofp(Float, x)}()
@generated StatiFloat(::StaticInt{N}) where {N} = Expr(:call, Expr(:curly, :StaticFloat, Float(N)))

@aggressive_constprop static(x::Float) = StaticFloat(x)
is_static(::Type{T}) where {T<:StaticFloat} = True()
known(::Type{StaticFloat{N}}) where {N} = N::Float
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
Base.:+(x::StaticFloat{X}, y::StaticInt{Y}) where {X,Y} = fadd(x, StaticFloat(y))
Base.:+(x::StaticInt{X}, y::StaticFloat{Y}) where {X,Y} = fadd(StaticFloat(x), y)

Base.:-(x::StaticFloat{X}, y::StaticFloat{Y}) where {X,Y} = fsub(x, y)
Base.:-(x::StaticFloat{X}, y::StaticInt{Y}) where {X,Y} = fsub(x, StaticFloat(y))
Base.:-(x::StaticInt{X}, y::StaticFloat{Y}) where {X,Y} = fsub(StaticFloat(x), y)

Base.:*(x::StaticFloat{X}, y::StaticFloat{Y}) where {X,Y} = fmul(x, y)
Base.:*(x::StaticFloat{X}, y::StaticInt{Y}) where {X,Y} = fmul(x, StaticFloat(y))
Base.:*(x::StaticInt{X}, y::StaticFloat{Y}) where {X,Y} = fmul(StaticFloat(x), y)

Base.:/(x::StaticFloat{X}, y::StaticFloat{Y}) where {X,Y} = fdiv(x, y)
Base.:/(x::StaticFloat{X}, y::StaticInt{Y}) where {X,Y} = fdiv(x, StaticFloat(y))
Base.:/(x::StaticInt{X}, y::StaticFloat{Y}) where {X,Y} = fdiv(StaticFloat(x), y)

@generated Base.sqrt(::StaticInt{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat, sqrt(M)))
@generated Base.sqrt(::StaticFloat{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat, sqrt(M)))

Base.:+(x::StaticFloat{N}, y::StaticInt{0}) where {N} = x
Base.:+(x::StaticInt{0}, y::StaticFloat{N}) where {N} = y

Base.:-(x::StaticFloat{N}, y::StaticInt{0}) where {N} = x
Base.:-(x::StaticInt{0}, y::StaticFloat{N}) where {N} = -x

Base.:*(x::StaticFloat{N}, y::StaticInt{0}) where {N} = zero(x)
Base.:*(x::StaticInt{0}, y::StaticFloat{N}) where {N} = zero(y)
Base.:*(x::StaticFloat{N}, y::StaticInt{1}) where {N} = x
Base.:*(x::StaticInt{1}, y::StaticFloat{N}) where {N} = y

@generated Base.round(::StaticFloat{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat, round(M)))
@generated roundtostaticint(::StaticFloat{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, round(Int, M)))
roundtostaticint(x::AbstractFloat) = round(Int, x)
@generated floortostaticint(::StaticFloat{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, floor(Int, M)))
floortostaticint(x::AbstractFloat) = Base.fptosi(Int, x)

Base.inv(x::StaticFloat{N}) where {N} = fdiv(one(x), x)
