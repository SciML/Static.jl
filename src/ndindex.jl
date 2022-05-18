
"""
    NDIndex(i, j, k...)   -> I
    NDIndex((i, j, k...)) -> I

A multidimensional index that refers to a single element. Each dimension is represented by
a single `Int` or `StaticInt`.

```julia
julia> using Static

julia> i = NDIndex(static(1), 2, static(3))
NDIndex(static(1), 2, static(3))

julia> i[static(1)]
static(1)

julia> i[1]
1

```
"""
struct NDIndex{N,I<:Tuple{Vararg{Union{StaticInt,Int},N}}} <: AbstractCartesianIndex{N}
    index::I

    NDIndex{N}(i::Tuple{Vararg{Union{StaticInt,Int},N}}) where {N} = new{N,typeof(i)}(i)
    NDIndex{N}(index::Tuple) where {N} = _ndindex(static(N), _flatten(index...))
    NDIndex{N}(index...) where {N} = NDIndex{N}(index)

    NDIndex{0}(::Tuple{}) = new{0,Tuple{}}(())
    NDIndex{0}() = NDIndex{0}(())

    NDIndex(i::Tuple{Vararg{Union{StaticInt,Int},N}}) where {N} = new{N,typeof(i)}(i)
    NDIndex(i::Vararg{Union{StaticInt,Int},N}) where {N} = NDIndex(i)

    NDIndex(index::Tuple) = NDIndex(_flatten(index...))
    NDIndex(index...) = NDIndex(index)
end

_ndindex(n::StaticInt{N}, i::Tuple{Vararg{Union{Int,StaticInt},N}}) where {N} = NDIndex(i)
function _ndindex(n::StaticInt{N}, i::Tuple{Vararg{Any,M}}) where {N,M}
    M > N && throw(ArgumentError("input tuple of length $M, requested $N"))
    return NDIndex(_fill_to_length(i, n))
end
_fill_to_length(x::Tuple{Vararg{Any,N}}, n::StaticInt{N}) where {N} = x
@inline function _fill_to_length(x::Tuple{Vararg{Any,M}}, n::StaticInt{N}) where {M,N}
    return _fill_to_length((x..., static(1)), n)
end

_flatten(i::StaticInt{N}) where {N} = (i,)
_flatten(i::Integer) = (Int(i),)
_flatten(i::Base.AbstractCartesianIndex) = _flatten(Tuple(i)...)
@inline _flatten(i::StaticInt, I...) = (i, _flatten(I...)...)
@inline _flatten(i::Integer, I...) = (Int(i), _flatten(I...)...)
@inline function _flatten(i::Base.AbstractCartesianIndex, I...)
    return (_flatten(Tuple(i)...)..., _flatten(I...)...)
 end
Base.Tuple(@nospecialize(x::NDIndex)) = getfield(x, :index)

Base.show(io::IO, @nospecialize(x::NDIndex)) = show(io, MIME"text/plain"(), x)
function Base.show(io::IO, m::MIME"text/plain", @nospecialize(x::NDIndex))
    print(io, "NDIndex")
    show(io, m, Tuple(x))
end

# length
Base.length(@nospecialize(x::NDIndex))::Int = length(Tuple(x))
Base.length(::Type{<:NDIndex{N}}) where {N} = N

# indexing
@propagate_inbounds function Base.getindex(x::NDIndex{N,T}, i::Int)::Int where {N,T}
    return Int(getfield(Tuple(x), i))
end
@propagate_inbounds function Base.getindex(x::NDIndex{N,T}, i::StaticInt{I}) where {N,T,I}
    return getfield(Tuple(x), I)
end

# Base.get(A::AbstractArray, I::CartesianIndex, default) = get(A, I.I, default)
# eltype(::Type{T}) where {T<:CartesianIndex} = eltype(fieldtype(T, :I))

Base.setindex(x::NDIndex, i, j) = NDIndex(Base.setindex(Tuple(x), i, j))

# equality
Base.:(==)(@nospecialize(x::NDIndex), @nospecialize(y::NDIndex)) = ==(Tuple(x), Tuple(y))

# zeros and ones
Base.zero(@nospecialize(x::NDIndex)) = zero(typeof(x))
Base.zero(@nospecialize(T::Type{<:NDIndex})) = NDIndex(ntuple(_ -> static(0), Val(length(T))))
Base.oneunit(@nospecialize(x::NDIndex)) = oneunit(typeof(x))
Base.oneunit(@nospecialize(T::Type{<:NDIndex})) = NDIndex(ntuple(_ -> static(1), Val(length(T))))

@inline function Base.IteratorsMD.split(i::NDIndex, V::Val)
    i, j = Base.IteratorsMD.split(Tuple(i), V)
    return NDIndex(i), NDIndex(j)
end

# arithmetic, min/max
@inline Base.:(-)(@nospecialize(i::NDIndex)) = NDIndex(map(-, Tuple(i)))
@inline function Base.:(+)(@nospecialize(i1::NDIndex), @nospecialize(i2::NDIndex))
    NDIndex(map(+, Tuple(i1), Tuple(i2)))
end
@inline function Base.:(-)(@nospecialize(i1::NDIndex), @nospecialize(i2::NDIndex))
    NDIndex(map(-, Tuple(i1), Tuple(i2)))
end
@inline function Base.min(@nospecialize(i1::NDIndex), @nospecialize(i2::NDIndex))
    NDIndex(map(min, Tuple(i1), Tuple(i2)))
end
@inline function Base.max(@nospecialize(i1::NDIndex), @nospecialize(i2::NDIndex))
    NDIndex(map(max, Tuple(i1), Tuple(i2)))
end
@inline Base.:(*)(a::Integer, @nospecialize(i::NDIndex)) = NDIndex(map(x->a*x, Tuple(i)))
@inline Base.:(*)(@nospecialize(i::NDIndex), a::Integer) = *(a, i)

Base.CartesianIndex(@nospecialize(x::NDIndex)) = dynamic(x)

# comparison
@inline function Base.isless(@nospecialize(x::NDIndex), @nospecialize(y::NDIndex))
    Bool(_isless(static(0), Tuple(x), Tuple(y)))
end

lt(@nospecialize(x::NDIndex), @nospecialize(y::NDIndex)) = _isless(static(0), Tuple(x), Tuple(y))

_final_isless(c::Int) = c === 1
_final_isless(::StaticInt{N}) where {N} = static(false)
_final_isless(::StaticInt{1}) = static(true)
_isless(c::C, x::Tuple{}, y::Tuple{}) where {C} = _final_isless(c)
function _isless(c::C, x::Tuple, y::Tuple) where {C}
    _isless(icmp(c, x, y), Base.front(x), Base.front(y))
end
icmp(::StaticInt{0}, x::Tuple, y::Tuple) = icmp(last(x), last(y))
icmp(::StaticInt{N}, x::Tuple, y::Tuple) where {N} = static(N)
function icmp(cmp::Int, x::Tuple, y::Tuple)
    if cmp === 0
        return icmp(Int(last(x)), Int(last(y)))
    else
        return cmp
    end
end
icmp(a, b) = _icmp(lt(a, b), a, b)
_icmp(x::StaticBool, a, b) = ifelse(x, static(1), __icmp(eq(a, b)))
_icmp(x::Bool, a, b) = ifelse(x, 1, __icmp(a == b))
__icmp(x::StaticBool) = ifelse(x, static(0), static(-1))
__icmp(x::Bool) = ifelse(x, 0, -1)

#  Necessary for compatibility with Base
# In simple cases, we know that we don't need to use axes(A). Optimize those
# until Julia gets smart enough to elide the call on its own:
@inline function Base.to_indices(A, inds, I::Tuple{NDIndex, Vararg{Any}})
    to_indices(A, inds, (Tuple(I[1])..., Base.tail(I)...))
end
# But for arrays of CartesianIndex, we just skip the appropriate number of inds
@inline function Base.to_indices(A, inds, I::Tuple{AbstractArray{NDIndex{N,J}}, Vararg{Any}}) where {N,J}
    _, indstail = Base.IteratorsMD.split(inds, Val(N))
    return (Base.to_index(A, I[1]), to_indices(A, indstail, Base.tail(I))...)
end

