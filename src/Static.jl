module Static

import IfElse: ifelse
using Base: @propagate_inbounds, Slice, AbstractCartesianIndex, Fix2

export StaticInt, StaticFloat64, StaticSymbol, True, False, StaticBool, NDIndex
export dynamic, is_static, known, static 

@static if isdefined(Base, Symbol("@constprop"))
    using Base: @constprop
else
    macro constprop(_, ex)
        ex
    end
end

include("int.jl")
include("bool.jl")
include("float.jl")
include("symbol.jl")
include("operators.jl")
include("ndindex.jl")
include("tuples.jl")

"""
    known(::Type{T})

Returns the known value corresponding to a static type `T`. If `T` is not a static type then
`nothing` is returned.

See also: [`static`](@ref), [`is_static`](@ref)
"""
known
@constprop :aggressive known(x) = known(typeof(x))
known(::Type{T}) where {T} = nothing
known(::Type{StaticInt{N}}) where {N} = N::Int
known(::Type{StaticFloat64{N}}) where {N} = N::Float64
known(::Type{StaticSymbol{S}}) where {S} = S::Symbol
known(::Type{Val{V}}) where {V} = V
known(::Type{True}) = true
known(::Type{False}) = false
known(::Type{NDIndex{N,I}}) where {N,I} = known(I)
_get_known(::Type{T}, dim::StaticInt{D}) where {T,D} = known(field_type(T, dim))
function known(::Type{T}) where {N,T<:Tuple{Vararg{Any,N}}}
    return eachop(_get_known, nstatic(Val(N)), T)
end

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
static
@constprop :aggressive static(x::X) where {X} = ifelse(is_static(X), identity, _no_static_type)(x)
@constprop :aggressive static(x::Int) = StaticInt(x)
@constprop :aggressive static(x::Union{Int8,UInt8,Int16,UInt16}) = StaticInt(x % Int)
@static if sizeof(Int) == 8
    @constprop :aggressive static(x::Union{Int32,UInt32}) = StaticInt(x % Int)
    @constprop :aggressive static(x::UInt64) = StaticInt(Int(x))
else
    @constprop :aggressive static(x::UInt32) = StaticInt(Int(x))
end
@constprop :aggressive static(x::Float64) = StaticFloat64(x)
@constprop :aggressive static(x::Bool) = StaticBool(x)
@constprop :aggressive static(x::Symbol) = StaticSymbol(x)
@constprop :aggressive static(x::Tuple{Vararg{Any}}) = map(static, x)
@generated static(::Val{V}) where {V} = static(V)
function _no_static_type(@nospecialize(x))
    error("There is no static alternative for type $(typeof(x)).")
end
static(x::CartesianIndex) = NDIndex(static(Tuple(x)))


"""
    is_static(::Type{T}) -> StaticBool

Returns `True` if `T` is a static type.

See also: [`static`](@ref), [`known`](@ref)
"""
is_static(@nospecialize(x)) = is_static(typeof(x))
is_static(@nospecialize(x::Type{<:StaticInt})) = True()
is_static(@nospecialize(x::Type{<:StaticBool})) = True()
is_static(@nospecialize(x::Type{<:StaticSymbol})) = True()
is_static(@nospecialize(x::Type{<:Val})) = True()
is_static(@nospecialize(x::Type{<:StaticFloat64})) = True()
is_static(x::Type{T}) where {T} = False()

@constprop :aggressive _tuple_static(::Type{T}, i) where {T} = is_static(field_type(T, i))
function is_static(::Type{T}) where {N,T<:Tuple{Vararg{Any,N}}}
    if all(eachop(_tuple_static, nstatic(Val(N)), T))
        return True()
    else
        return False()
    end
end

"""
    dynamic(x)

Returns the "dynamic" or non-static form of `x`.
"""
dynamic(x::X) where {X} = _dynamic(is_static(X), x)
_dynamic(::True, x::X) where {X} = known(X)
_dynamic(::False, x::X) where {X} = x
@constprop :aggressive dynamic(x::Tuple) = map(dynamic, x)
dynamic(x::NDIndex) = CartesianIndex(dynamic(Tuple(x)))

end
