module Static

import IfElse: ifelse
using Base: @propagate_inbounds, Slice

export dynamic, is_static, known, static, StaticInt, StaticFloat64, StaticSymbol, True, False, StaticBool

@static if VERSION >= v"1.7.0-DEV.421"
    using Base: @aggressive_constprop
else
    macro aggressive_constprop(ex)
        ex
    end
end


include("int.jl")
include("bool.jl")
include("float.jl")
include("symbol.jl")
include("tuples.jl")

"""
    known(::Type{T})

Returns the known value corresponding to a static type `T`. If `T` is not a static type then
`nothing` is returned.

See also: [`static`](@ref), [`is_static`](@ref)
"""
known
@aggressive_constprop known(x) = known(typeof(x))
known(::Type{T}) where {T} = nothing
known(::Type{StaticInt{N}}) where {N} = N::Int
known(::Type{StaticFloat64{N}}) where {N} = N::Float64
known(::Type{StaticSymbol{S}}) where {S} = S::Symbol
known(::Type{Val{V}}) where {V} = V
known(::Type{True}) = true
known(::Type{False}) = false
_get_known(::Type{T}, dim::StaticInt{D}) where {T,D} = known(_get_tuple(T, dim))
function known(::Type{T}) where {N,T<:Tuple{Vararg{Any,N}}}
    return eachop(_get_known, nstatic(Val(N)), T)
end

"""
    static(x)

Returns a static form of `x`. If `x` is already in a static form then `x` is returned. If
there is no static alternative for `x` then an error is thrown.

See also: [`is_static`](@ref), [`known`](@ref)

```julia
julia> using ArrayInterface: static

julia> static(1)
static(1)

julia> static(true)
ArrayInterface.True()

julia> static(:x)
static(:x)

```
"""
static
@aggressive_constprop static(x::X) where {X} = ifelse(is_static(X), identity, _no_static_type)(x)
@aggressive_constprop static(x::Int) = StaticInt(x)
@aggressive_constprop static(x::Union{Int8,UInt8,Int16,UInt16}) = StaticInt(x % Int)
if sizeof(Int) == 8
    @aggressive_constprop static(x::Union{Int32,UInt32}) = StaticInt(x % Int)
end
@aggressive_constprop static(x::Float64) = StaticFloat64(x)
@aggressive_constprop static(x::Bool) = StaticBool(x)
@aggressive_constprop static(x::Symbol) = StaticSymbol(x)
@aggressive_constprop static(x::Tuple{Vararg{Any}}) = map(static, x)
@generated static(::Val{V}) where {V} = static(V)
function _no_static_type(@nospecialize(x))
    error("There is no static alternative for type $(typeof(x)).")
end

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

@aggressive_constprop _tuple_static(::Type{T}, i) where {T} = is_static(_get_tuple(T, i))
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
@aggressive_constprop dynamic(x::Tuple) = map(dynamic, x)

end
