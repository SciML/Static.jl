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
known(@nospecialize(T::Type{<:StaticInt}))::Int = T.parameters[1]
known(@nospecialize(T::Type{<:StaticFloat64}))::Float64 = T.parameters[1]
known(@nospecialize(T::Type{<:StaticSymbol}))::Symbol = T.parameters[1]
known(::Type{Val{V}}) where {V} = V
known(::Type{True}) = true
known(::Type{False}) = false
known(@nospecialize(T::Type{<:NDIndex})) = known(T.parameters[2])
_get_known(::Type{T}, dim::StaticInt{D}) where {T,D} = known(field_type(T, dim))
known(@nospecialize(T::Type{<:Tuple})) = eachop(_get_known, nstatic(Val(fieldcount(T))), T)
known(T::DataType) = nothing
known(@nospecialize(x)) = known(typeof(x))

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
static(@nospecialize(x::Union{StaticInt,StaticSymbol,StaticFloat64,True,False})) = x
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
static(::Val{V}) where {V} = static(V)
static(@nospecialize(x::CartesianIndex)) = NDIndex(static(Tuple(x)))
static(x) = error("There is no static alternative for type $(typeof(x)).")

"""
    is_static(::Type{T}) -> StaticBool

Returns `True` if `T` is a static type.

See also: [`static`](@ref), [`known`](@ref)
"""
is_static(@nospecialize(x)) = is_static(typeof(x))
is_static(@nospecialize(x::Type{<:Union{StaticInt,StaticSymbol,StaticFloat64,True,False}})) = True()
is_static(@nospecialize(x::Type{<:Val})) = True()
@constprop :aggressive _tuple_static(::Type{T}, i) where {T} = is_static(field_type(T, i))
@inline function is_static(@nospecialize(T::Type{<:Tuple}))
    if all(eachop(_tuple_static, nstatic(Val(fieldcount(T))), T))
        return True()
    else
        return False()
    end
end
is_static(T::DataType) = False()

"""
    dynamic(x)

Returns the "dynamic" or non-static form of `x`.
"""
@inline dynamic(@nospecialize(x)) = ifelse(is_static(typeof(x)), known, identity)(x)
dynamic(@nospecialize(x::Tuple)) = map(dynamic, x)
dynamic(@nospecialize(x::NDIndex)) = CartesianIndex(dynamic(Tuple(x)))

function Base.string(@nospecialize(x::Union{StaticInt,StaticSymbol,StaticFloat64,True,False}); kwargs...)
    string("static(" * repr(known(typeof(x))) * ")"; kwargs...)
end
Base.show(io::IO, @nospecialize(x::Union{StaticInt,StaticSymbol,StaticFloat64,True,False})) = show(io, MIME"text/plain"(), x)
Base.show(io::IO, ::MIME"text/plain", @nospecialize(x::Union{StaticInt,StaticSymbol,StaticFloat64,True,False})) = print(io, string(x))

# This method assumes that `f` uetrieves compile time information and `g` is the fall back
# for the corresponding dynamic method. If the `f(x)` doesn't return `nothing` that means
# the value is known and compile time and returns `static(f(x))`.
@inline function maybe_static(f::F, g::G, x) where {F,G}
    L = f(x)
    if L === nothing
        return g(x)
    else
        return static(L)
    end
end

end
