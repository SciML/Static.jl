
"""
    OptionallyStaticUnitRange(start, stop) <: AbstractUnitRange{Int}

Similar to `UnitRange` except each field may be an `Int` or `StaticInt`. An
`OptionallyStaticUnitRange` is intended to be constructed internally from other valid
indices. Therefore, users should not expect the same checks are used to ensure construction
of a valid `OptionallyStaticUnitRange` as a `UnitRange`.
"""
struct OptionallyStaticUnitRange{F <: IntType, L <: IntType} <:
       AbstractUnitRange{Int}
    start::F
    stop::L

    global function _OptionallyStaticUnitRange(start::IntType, stop::IntType)
        new{typeof(start), typeof(stop)}(start, stop)
    end
end

"""
    OptionallyStaticStepRange(start, step, stop) <: OrdinalRange{Int,Int}

Similarly to [`OptionallyStaticUnitRange`](@ref), `OptionallyStaticStepRange` permits
a combination of static and standard primitive `Int`s to construct a range. It
specifically enables the use of ranges without a step size of 1. It may be constructed
through the use of `OptionallyStaticStepRange` directly or using static integers with
the range operator (i.e., `:`).

```julia
julia> using Static

julia> x = static(2);

julia> x:x:10
static(2):static(2):10

julia> Static.OptionallyStaticStepRange(x, x, 10)
static(2):static(2):10

```
"""
struct OptionallyStaticStepRange{F <: IntType, S <: IntType,
    L <: IntType} <: OrdinalRange{Int, Int}
    start::F
    step::S
    stop::L

    global function _OptionallyStaticStepRange(@nospecialize(start::IntType),
            @nospecialize(step::IntType),
            @nospecialize(stop::IntType))
        new{typeof(start), typeof(step), typeof(stop)}(start, step, stop)
    end
end
@noinline function OptionallyStaticStepRange(@nospecialize(start::IntType),
        ::StaticInt{0},
        @nospecialize(stop::IntType))
    throw(ArgumentError("step cannot be zero"))
end
# we don't need to check the `stop` if we know it acts like a unit range
function OptionallyStaticStepRange(@nospecialize(start::IntType),
        step::StaticInt{1},
        @nospecialize(stop::IntType))
    _OptionallyStaticStepRange(start, step, stop)
end
function OptionallyStaticStepRange(@nospecialize(start::IntType),
        @nospecialize(step::StaticInt),
        @nospecialize(stop::IntType))
    _OptionallyStaticStepRange(start, step, _steprange_last(start, step, stop))
end
function OptionallyStaticStepRange(start, step, stop)
    OptionallyStaticStepRange(IntType(start), IntType(step), IntType(stop))
end
function OptionallyStaticStepRange(@nospecialize(start::IntType),
        step::Int,
        @nospecialize(stop::IntType))
    if step === 0
        throw(ArgumentError("step cannot be zero"))
    else
        _OptionallyStaticStepRange(start, step, _steprange_last(start, step, stop))
    end
end
OptionallyStaticStepRange(@nospecialize x::OptionallyStaticStepRange) = x
function OptionallyStaticStepRange(x::Union{Base.Slice, Base.IdentityUnitRange})
    OptionallyStaticStepRange(x.indices)
end
function OptionallyStaticStepRange(x::Base.OneTo)
    _OptionallyStaticStepRange(static(1), static(1), Int(last(x)))
end
function OptionallyStaticStepRange(x::OptionallyStaticUnitRange)
    _OptionallyStaticStepRange(getfield(x, :start), static(1), getfield(x, :stop))
end
function OptionallyStaticStepRange(x::AbstractUnitRange)
    _OptionallyStaticStepRange(Int(first(x)), static(1), Int(last(x)))
end
function OptionallyStaticStepRange(x::AbstractRange)
    _OptionallyStaticStepRange(Int(first(x)), Int(step(x)), Int(last(x)))
end

# to make StepRange constructor inlineable, so optimizer can see `step` value
@inline function _steprange_last(start::StaticInt, step::StaticInt, stop::StaticInt)
    StaticInt(_steprange_last(Int(start), Int(step), Int(stop)))
end
@inline function _steprange_last(start::Union{StaticInt, Int},
        step::Union{StaticInt, Int},
        stop::Union{StaticInt, Int})
    _steprange_last(Int(start), Int(step), Int(stop))
end
@inline function _steprange_last(start::Int, step::Int, stop::Int)
    if stop === start
        return stop
    elseif step > 0
        if stop > start
            return stop - rem(stop - start, step)
        else
            return start - 1
        end
    else
        if stop > start
            return start + 1
        else
            return stop + rem(start - stop, -step)
        end
    end
end

OptionallyStaticUnitRange(@nospecialize x::OptionallyStaticUnitRange) = x
OptionallyStaticUnitRange(x::Base.OneTo) = OptionallyStaticUnitRange(static(1), Int(last(x)))
function OptionallyStaticUnitRange(x::Union{Base.Slice, Base.IdentityUnitRange})
    OptionallyStaticUnitRange(x.indices)
end
function OptionallyStaticUnitRange(x::OptionallyStaticStepRange)
    assert_unit_step(step(x))
    _OptionallyStaticUnitRange(getfield(x, :start), getfield(x, :stop))
end
function OptionallyStaticUnitRange(x::AbstractRange)
    assert_unit_step(step(x))
    _OptionallyStaticUnitRange(first(x), last(x))
end
function OptionallyStaticUnitRange{F, L}(x::AbstractRange) where {F, L}
    OptionallyStaticUnitRange(x)
end
function OptionallyStaticUnitRange(start::IntType, stop::IntType)
    _OptionallyStaticUnitRange(start, stop)
end
function OptionallyStaticUnitRange(start, stop)
    OptionallyStaticUnitRange(IntType(start), IntType(stop))
end
function OptionallyStaticUnitRange{StaticInt{F}, StaticInt{L}}() where {F, L}
    _OptionallyStaticUnitRange(StaticInt{F}(), StaticInt{L}())
end
function assert_unit_step(s::Int)
    s == 1 && return nothing
    errmsg(x) = throw(ArgumentError(LazyString("step must be 1, got ", s))) # avoid GC frame
    errmsg(s)
end

"""
    SUnitRange(start::Int, stop::Int)

An alias for `OptionallyStaticUnitRange` where both the start and stop are known statically.
"""
const SUnitRange{F, L} = OptionallyStaticUnitRange{StaticInt{F}, StaticInt{L}}
SUnitRange(start::Int, stop::Int) = SUnitRange{start, stop}()

"""
    SOneTo(n::Int)

An alias for `OptionallyStaticUnitRange` usfeul for statically sized axes.
"""
const SOneTo{L} = SUnitRange{1, L}
SOneTo(n::Int) = SOneTo{n}()
Base.oneto(::StaticInt{N}) where {N} = SOneTo{N}()

const OptionallyStaticRange{
    F, L} = Union{OptionallyStaticUnitRange{F, L},
    OptionallyStaticStepRange{F, <:Any, L}}

Base.first(x::OptionallyStaticRange{Int}) = getfield(x, :start)
Base.first(::OptionallyStaticRange{StaticInt{F}}) where {F} = F
Base.step(x::OptionallyStaticStepRange{<:Any, Int}) = getfield(x, :step)
Base.step(::OptionallyStaticStepRange{<:Any, StaticInt{S}}) where {S} = S
Base.last(x::OptionallyStaticRange{<:Any, Int}) = getfield(x, :stop)
Base.last(::OptionallyStaticRange{<:Any, StaticInt{L}}) where {L} = L

# FIXME this line causes invalidations
Base.:(:)(L::Integer, ::StaticInt{U}) where {U} = OptionallyStaticUnitRange(L, StaticInt(U))
Base.:(:)(::StaticInt{L}, U::Integer) where {L} = OptionallyStaticUnitRange(StaticInt(L), U)
function Base.:(:)(::StaticInt{L}, ::StaticInt{U}) where {L, U}
    OptionallyStaticUnitRange(StaticInt(L), StaticInt(U))
end
function Base.:(:)(::StaticInt{F}, ::StaticInt{S}, ::StaticInt{L}) where {F, S, L}
    OptionallyStaticStepRange(StaticInt(F), StaticInt(S), StaticInt(L))
end
function Base.:(:)(start::Integer, ::StaticInt{S}, ::StaticInt{L}) where {S, L}
    OptionallyStaticStepRange(start, StaticInt(S), StaticInt(L))
end
function Base.:(:)(::StaticInt{F}, ::StaticInt{S}, stop::Integer) where {F, S}
    OptionallyStaticStepRange(StaticInt(F), StaticInt(S), stop)
end
function Base.:(:)(::StaticInt{F}, step::Integer, ::StaticInt{L}) where {F, L}
    OptionallyStaticStepRange(StaticInt(F), step, StaticInt(L))
end
function Base.:(:)(start::Integer, step::Integer, ::StaticInt{L}) where {L}
    OptionallyStaticStepRange(start, step, StaticInt(L))
end
function Base.:(:)(start::Integer, ::StaticInt{S}, stop::Integer) where {S}
    OptionallyStaticStepRange(start, StaticInt(S), stop)
end
function Base.:(:)(::StaticInt{F}, step::Integer, stop::Integer) where {F}
    OptionallyStaticStepRange(StaticInt(F), step, stop)
end
Base.:(:)(start::StaticInt{F}, ::StaticInt{1}, stop::StaticInt{L}) where {F, L} = start:stop
Base.:(:)(start::Integer, ::StaticInt{1}, stop::StaticInt{L}) where {L} = start:stop
Base.:(:)(start::StaticInt{F}, ::StaticInt{1}, stop::Integer) where {F} = start:stop
function Base.:(:)(start::Integer, ::StaticInt{1}, stop::Integer)
    OptionallyStaticUnitRange(start, stop)
end

Base.isempty(r::OptionallyStaticUnitRange) = first(r) > last(r)
@inline function Base.isempty(x::OptionallyStaticStepRange)
    start = first(x)
    stop = last(x)
    if start === stop
        return false
    else
        s = step(x)
        s > 0 ? start > stop : start < stop
    end
end

function Base.checkindex(::Type{Bool},
        ::SUnitRange{F1, L1},
        ::SUnitRange{F2, L2}) where {F1, L1, F2, L2}
    (F1::Int <= F2::Int) && (L1::Int >= L2::Int)
end

function Base.getindex(
    r::OptionallyStaticUnitRange,
    i::AbstractUnitRange{<:Integer},
)
    s = OptionallyStaticUnitRange(i)
    @boundscheck checkbounds(r, s)
    f = getfield(r, :start)
    fnew = f - one(f)
    return (fnew + getfield(s, :start)):(fnew + getfield(s, :stop))
end

function Base.getindex(x::OptionallyStaticUnitRange{StaticInt{1}}, i::Int)
    @boundscheck checkbounds(x, i)
    i
end
function Base.getindex(x::OptionallyStaticUnitRange, i::Int)
    val = first(x) + (i - 1)
    @boundscheck ((i < 1) || val > last(x)) && throw(BoundsError(x, i))
    val::Int
end

## length
@inline function Base.length(x::OptionallyStaticUnitRange)
    start = first(x)
    stop = last(x)
    start > stop ? 0 : stop - start + 1
end
Base.length(r::OptionallyStaticStepRange) = _range_length(first(r), step(r), last(r))
@inline function _range_length(start::Int, s::Int, stop::Int)
    if s > 0
        stop < start ? 0 : div(stop - start, s) + 1
    else
        stop > start ? 0 : div(start - stop, -s) + 1
    end
end

Base.AbstractUnitRange{Int}(r::OptionallyStaticUnitRange) = r
function Base.AbstractUnitRange{T}(r::OptionallyStaticUnitRange) where {T}
    if isa(getfield(r, :start), StaticInt{1}) && T <: Integer
        return Base.OneTo{T}(T(last(r)))
    else
        return UnitRange{T}(T(first(r)), T(last(r)))
    end
end

Base.isdone(x::OptionallyStaticRange, state::Int) = state === last(x)
function _next(x::OptionallyStaticRange)
    new_state = first(x)
    (new_state, new_state)
end
@inline function _next(@nospecialize(x::OptionallyStaticUnitRange), state::Int)
    new_state = state + 1
    (new_state, new_state)
end
@inline function _next(x::OptionallyStaticStepRange, state::Int)
    new_state = state + step(x)
    (new_state, new_state)
end
@inline Base.iterate(x::OptionallyStaticRange) = isempty(x) ? nothing : _next(x)
@inline function Base.iterate(x::OptionallyStaticRange, s::Int)
    Base.isdone(x, s) ? nothing : _next(x, s)
end

Base.to_shape(x::OptionallyStaticRange) = length(x)
Base.to_shape(x::Base.Slice{T}) where {T <: OptionallyStaticRange} = length(x)
Base.axes(S::Base.Slice{<:OptionallyStaticUnitRange{StaticInt{1}}}) = (S.indices,)
Base.axes(S::Base.Slice{<:OptionallyStaticRange}) = (Base.IdentityUnitRange(S.indices),)

Base.axes(x::OptionallyStaticRange) = (Base.axes1(x),)
function Base.axes1(x::OptionallyStaticUnitRange)
    OptionallyStaticUnitRange(StaticInt(1), length(x))
end
Base.axes1(x::OptionallyStaticUnitRange{StaticInt{1}}) = x
function Base.axes1(x::OptionallyStaticUnitRange{StaticInt{F}, StaticInt{L}}) where {F, L}
    OptionallyStaticUnitRange(StaticInt(1), StaticInt(L - F + 1))
end
function Base.axes1(x::OptionallyStaticStepRange)
    OptionallyStaticUnitRange(StaticInt(1), length(x))
end
function Base.axes1(x::OptionallyStaticStepRange{
        StaticInt{F}, StaticInt{S}, StaticInt{L}}) where {
        F,
        S,
        L
}
    OptionallyStaticUnitRange(StaticInt(1), StaticInt(_range_length(F, S, L)))
end
Base.axes1(x::Base.Slice{<:OptionallyStaticUnitRange{One}}) = x.indices
Base.axes1(x::Base.Slice{<:OptionallyStaticRange}) = Base.IdentityUnitRange(x.indices)

function Base.:(-)(r::OptionallyStaticRange)
    s = isa(r, OptionallyStaticStepRange) ? -getfield(r, :step) : -One()
    (-getfield(r, :start)):s:(-getfield(r, :stop))
end

function Base.reverse(x::OptionallyStaticUnitRange)
    _OptionallyStaticStepRange(getfield(x, :stop), StaticInt(-1), getfield(x, :start))
end
function Base.reverse(x::OptionallyStaticStepRange)
    _OptionallyStaticStepRange(getfield(x, :stop), -getfield(x, :step), getfield(x, :start))
end

Base.show(io::IO, @nospecialize(x::OptionallyStaticRange)) = show(io, MIME"text/plain"(), x)
function Base.show(io::IO, ::MIME"text/plain", @nospecialize(r::OptionallyStaticUnitRange))
    print(io, "$(getfield(r, :start)):$(getfield(r, :stop))")
end
function Base.show(io::IO, ::MIME"text/plain", @nospecialize(r::OptionallyStaticStepRange))
    print(io, "$(getfield(r, :start)):$(getfield(r, :step)):$(getfield(r, :stop))")
end

# we overload properties because occasionally Base assumes that abstract range types have
# the same exact same set up as native types where `x.start === first(x)`
@inline function Base.getproperty(x::OptionallyStaticRange, s::Symbol)
    if s === :start
        return first(x)
    elseif s === :step
        return step(x)
    elseif s === :stop
        return last(x)
    else
        error("$x has no property $s")
    end
end

function Base.Broadcast.axistype(r::OptionallyStaticUnitRange{StaticInt{1}}, _)
    Base.OneTo(last(r))
end
function Base.Broadcast.axistype(_, r::OptionallyStaticUnitRange{StaticInt{1}})
    Base.OneTo(last(r))
end
function Base.Broadcast.axistype(r::OptionallyStaticUnitRange{StaticInt{1}},
        ::OptionallyStaticUnitRange{StaticInt{1}})
    Base.OneTo(last(r))
end
function Base.similar(::Type{<:Array{T}},
        axes::Tuple{OptionallyStaticUnitRange{StaticInt{1}},
            Vararg{
                Union{Base.OneTo,
                OptionallyStaticUnitRange{StaticInt{1}}}}}) where {
        T
}
    Array{T}(undef, map(last, axes))
end
function Base.similar(::Type{<:Array{T}},
        axes::Tuple{Base.OneTo, OptionallyStaticUnitRange{StaticInt{1}},
            Vararg{
                Union{Base.OneTo,
                OptionallyStaticUnitRange{StaticInt{1}}}}}) where {
        T
}
    Array{T}(undef, map(last, axes))
end

function Base.first(x::OptionallyStaticUnitRange, n::IntType)
    n < 0 && throw(ArgumentError("Number of elements must be nonnegative"))
    start = getfield(x, :start)
    OptionallyStaticUnitRange(start, min(start - one(start) + n, getfield(x, :stop)))
end
function Base.first(x::OptionallyStaticStepRange, n::IntType)
    n < 0 && throw(ArgumentError("Number of elements must be nonnegative"))
    start = getfield(x, :start)
    s = getfield(x, :step)
    stop = min(((n - one(n)) * s) + start, getfield(x, :stop))
    OptionallyStaticStepRange(start, s, stop)
end
function Base.last(x::OptionallyStaticUnitRange, n::IntType)
    n < 0 && throw(ArgumentError("Number of elements must be nonnegative"))
    stop = getfield(x, :stop)
    OptionallyStaticUnitRange(max(stop + one(stop) - n, getfield(x, :start)), stop)
end
function Base.last(x::OptionallyStaticStepRange, n::IntType)
    n < 0 && throw(ArgumentError("Number of elements must be nonnegative"))
    start = getfield(x, :start)
    s = getfield(x, :step)
    stop = getfield(x, :stop)
    OptionallyStaticStepRange(max(stop + one(stop) - (n * s), start), s, stop)
end
