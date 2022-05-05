
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
Base.AbstractFloat(::StaticInt{N}) where {N} = StaticFloat64{Float64(N)}()
StaticFloat64(@nospecialize(x::StaticInt)) = float(x)

const FloatOne = StaticFloat64{one(Float64)}
const FloatZero = StaticFloat64{zero(Float64)}

Base.convert(::Type{T}, @nospecialize(x::StaticFloat64)) where {T<:AbstractFloat} = T(known(x))
Base.promote_rule(@nospecialize(T1::Type{<:StaticFloat64}), ::Type{T2}) where {T2} = promote_type(T2, Float64)
Base.promote_rule(@nospecialize(T1::Type{<:StaticFloat64}), ::Type{Float64}) = Float64
Base.promote_rule(@nospecialize(T1::Type{<:StaticFloat64}), ::Type{Float32}) = Float32
Base.promote_rule(@nospecialize(T1::Type{<:StaticFloat64}), ::Type{Float16}) = Float16

Base.eltype(@nospecialize(T::Type{<:StaticFloat64})) = Float64
Base.iszero(::FloatZero) = true
Base.iszero(@nospecialize(x::StaticFloat64)) = false
Base.isone(::FloatOne) = true
Base.isone(@nospecialize(x::StaticFloat64)) = false
Base.zero(@nospecialize(x::Type{<:StaticFloat64})) = FloatZero
Base.one(@nospecialize(x::Type{<:StaticFloat64})) = FloatOne()

function fsub(::StaticFloat64{X}, ::StaticFloat64{Y}) where {X,Y}
    return StaticFloat64{Base.sub_float(X, Y)::Float64}()
end

function fadd(::StaticFloat64{X}, ::StaticFloat64{Y}) where {X,Y}
    return StaticFloat64{Base.add_float(X, Y)::Float64}()
end

function fdiv(::StaticFloat64{X}, ::StaticFloat64{Y}) where {X,Y}
    return StaticFloat64{Base.div_float(X, Y)::Float64}()
end

function fmul(::StaticFloat64{X}, ::StaticFloat64{Y}) where {X,Y}
    return StaticFloat64{Base.mul_float(X, Y)::Float64}()
end

Base.:+(@nospecialize(x::StaticFloat64), @nospecialize(y::StaticFloat64)) = fadd(x, y)
@inline Base.:+(@nospecialize(x::StaticFloat64), @nospecialize(y::StaticInt)) = +(x, float(y))
@inline Base.:+(@nospecialize(x::StaticInt), @nospecialize(y::StaticFloat64)) = +(float(x), y)

Base.:-(::StaticFloat64{X}) where {X} = StaticFloat64{-X}()
Base.:-(@nospecialize(x::StaticFloat64), @nospecialize(y::StaticFloat64)) = fsub(x, y)
@inline Base.:-(@nospecialize(x::StaticFloat64), @nospecialize(y::StaticInt)) = -(x, float(y))
@inline Base.:-(@nospecialize(x::StaticInt), @nospecialize(y::StaticFloat64)) = -(float(x), y)

Base.:*(@nospecialize(x::StaticFloat64), @nospecialize(y::StaticFloat64)) = fmul(x, y)
@inline Base.:*(@nospecialize(x::StaticFloat64), @nospecialize(y::StaticInt)) = *(x, float(y))
@inline Base.:*(@nospecialize(x::StaticInt), @nospecialize(y::StaticFloat64)) = *(float(x), y)
@inline Base.:*(@nospecialize(x::StaticFloat64), ::Zero) = FloatZero()
@inline Base.:*(::Zero, @nospecialize(y::StaticFloat64)) = FloatZero()

Base.:/(@nospecialize(x::StaticFloat64), @nospecialize(y::StaticFloat64)) = fdiv(x, y)
Base.:/(@nospecialize(x::StaticFloat64), @nospecialize(y::StaticInt)) = /(x, float(y))
Base.:/(@nospecialize(x::StaticInt), @nospecialize(y::StaticFloat64)) = /(float(x), y)

Base.sqrt(@nospecialize(x::StaticInt)) = sqrt(float(x))
@generated Base.sqrt(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat64, sqrt(M)))

@generated Base.round(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticFloat64, round(M)))
@generated roundtostaticint(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, round(Int, M)))
roundtostaticint(x::AbstractFloat) = round(Int, x)
@generated floortostaticint(::StaticFloat64{M}) where {M} = Expr(:call, Expr(:curly, :StaticInt, floor(Int, M)))
floortostaticint(x::AbstractFloat) = Base.fptosi(Int, x)

Base.:(^)(::StaticFloat64{x}, y::Float64) where {x} = exp2(log2(x) * y)

Base.inv(x::StaticFloat64{N}) where {N} = fdiv(one(x), x)

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
