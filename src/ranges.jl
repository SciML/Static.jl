
_int(x::Integer) = Int(x)
_int(@nospecialize x::Union{StaticInt, Int}) = x

"""
    OptionallyStaticUnitRange(start, stop) <: AbstractUnitRange{Int}

Similar to `UnitRange` except each field may be an `Int` or `StaticInt`. An
`OptionallyStaticUnitRange` is intended to be constructed internally from other valid
indices. Therefore, users should not expect the same checks are used to ensure construction
of a valid `OptionallyStaticUnitRange` as a `UnitRange`.
"""
struct OptionallyStaticUnitRange{F <: Union{Int, StaticInt}, L <: Union{Int, StaticInt}} <:
       AbstractUnitRange{Int}
    start::F
    stop::L

    function OptionallyStaticUnitRange(start::Union{Int, StaticInt},
                                       stop::Union{Int, StaticInt})
        new{typeof(start), typeof(stop)}(start, stop)
    end
    function OptionallyStaticUnitRange(start, stop)
        OptionallyStaticUnitRange(_int(start), _int(stop))
    end
    OptionallyStaticUnitRange(@nospecialize x::OptionallyStaticUnitRange) = x
    function OptionallyStaticUnitRange(x::AbstractRange)
        step(x) == 1 && return OptionallyStaticUnitRange(static_first(x), static_last(x))

        errmsg(x) = throw(ArgumentError("step must be 1, got $(step(x))")) # avoid GC frame
        errmsg(x)
    end
    function OptionallyStaticUnitRange{F, L}(x::AbstractRange) where {F, L}
        OptionallyStaticUnitRange(x)
    end
    function OptionallyStaticUnitRange{StaticInt{F}, StaticInt{L}}() where {F, L}
        new{StaticInt{F}, StaticInt{L}}()
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
struct OptionallyStaticStepRange{F <: Union{Int, StaticInt}, S <: Union{Int, StaticInt},
                                 L <: Union{Int, StaticInt}} <: OrdinalRange{Int, Int}
    start::F
    step::S
    stop::L

    global function _OptionallyStaticStepRange(@nospecialize(start::Union{Int, StaticInt}),
                                               @nospecialize(step::Union{Int, StaticInt}),
                                               @nospecialize(stop::Union{Int, StaticInt}))
        new{typeof(start), typeof(step), typeof(stop)}(start, step, stop)
    end
end
@noinline function OptionallyStaticStepRange(@nospecialize(start::Union{Int, StaticInt}),
                                             ::StaticInt{0},
                                             @nospecialize(stop::Union{Int, StaticInt}))
    throw(ArgumentError("step cannot be zero"))
end
# we don't need to check the `stop` if we know it acts like a unit range
function OptionallyStaticStepRange(@nospecialize(start::Union{Int, StaticInt}),
                                   step::StaticInt{1},
                                   @nospecialize(stop::Union{Int, StaticInt}))
    _OptionallyStaticStepRange(start, step, stop)
end
function OptionallyStaticStepRange(@nospecialize(start::Union{Int, StaticInt}),
                                   @nospecialize(step::StaticInt),
                                   @nospecialize(stop::Union{Int, StaticInt}))
    _OptionallyStaticStepRange(start, step, _steprange_last(start, step, stop))
end
function OptionallyStaticStepRange(start, step, stop)
    OptionallyStaticStepRange(_int(start), _int(step), _int(stop))
end
function OptionallyStaticStepRange(@nospecialize(start::Union{Int, StaticInt}),
                                   step::Int,
                                   @nospecialize(stop::Union{Int, StaticInt}))
    if step === 0
        throw(ArgumentError("step cannot be zero"))
    else
        _OptionallyStaticStepRange(start, step, _steprange_last(start, step, stop))
    end
end
OptionallyStaticStepRange(@nospecialize x::OptionallyStaticStepRange) = x
function OptionallyStaticStepRange(x::AbstractRange)
    _OptionallyStaticStepRange(_int(first(x)), _int(step(x)), _int(last(x)))
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

const OptionallyStaticRange = Union{<:OptionallyStaticUnitRange, <:OptionallyStaticStepRange
                                    }

# these probide a generic method for extracting potentially static values.
static_first(x::Base.OneTo) = StaticInt(1)
static_first(x::Union{Base.Slice, Base.IdentityUnitRange}) = static_first(x.indices)
static_first(x::OptionallyStaticRange) = getfield(x, :start)
static_first(x) = first(x)

static_step(x::AbstractUnitRange) = StaticInt(1)
static_step(x::OptionallyStaticStepRange) = getfield(x, :step)
static_step(x) = step(x)

static_last(x::OptionallyStaticRange) = getfield(x, :stop)
static_last(x) = last(x)
static_last(x::Union{Base.Slice, Base.IdentityUnitRange}) = static_last(x.indices)

Base.first(x::OptionallyStaticRange) = Int(static_first(x))
Base.step(x::OptionallyStaticStepRange) = Int(static_step(x))
Base.last(x::OptionallyStaticRange) = Int(static_last(x))

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

Base.isempty(r::OptionallyStaticUnitRange{One}) = last(r) <= 0
Base.isempty(r::OptionallyStaticUnitRange) = first(r) > last(r)
function Base.isempty(r::OptionallyStaticStepRange)
    (r.start != r.stop) & ((r.step > 0) != (r.stop > r.start))
end

function Base.checkindex(::Type{Bool},
                         ::SUnitRange{F1, L1},
                         ::SUnitRange{F2, L2}) where {F1, L1, F2, L2}
    (F1::Int <= F2::Int) && (L1::Int >= L2::Int)
end

function Base.getindex(r::OptionallyStaticUnitRange,
                       s::AbstractUnitRange{<:Integer})
    @boundscheck checkbounds(r, s)
    f = static_first(r)
    fnew = f - one(f)
    return (fnew + static_first(s)):(fnew + static_last(s))
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
@inline function Base.length(r::OptionallyStaticUnitRange)
    isempty(r) ? 0 : last(r) - first(r) + 1
end
Base.length(r::OptionallyStaticStepRange) = _range_length(first(r), step(r), last(r))
_range_length(start, s, stop) = nothing
@inline function _range_length(start::Int, s::Int, stop::Int)
    if s > 0
        if stop < start  # isempty
            return 0
        else
            return div(stop - start, s) + 1
        end
    else
        if stop > start  # isempty
            return 0
        else
            return div(start - stop, -s) + 1
        end
    end
end

Base.AbstractUnitRange{Int}(r::OptionallyStaticUnitRange) = r
function Base.AbstractUnitRange{T}(r::OptionallyStaticUnitRange) where {T}
    start = static_first(r)
    if isa(start, StaticInt{1}) && T <: Integer
        return Base.OneTo{T}(T(static_last(r)))
    else
        return UnitRange{T}(T(static_first(r)), T(static_last(r)))
    end
end

@inline function Base.iterate(r::OptionallyStaticRange)
    isempty(r) && return nothing
    fi = Int(first(r))
    fi, fi
end
Base.iterate(::SUnitRange{F, L}) where {F, L} = L < F ? nothing : (F, F)
function Base.iterate(::SOneTo{n}, s::Int) where {n}
    if s < n::Int
        s2 = s + 1
        return (s2, s2)
    else
        return nothing
    end
end

Base.to_shape(x::OptionallyStaticRange) = length(x)
Base.to_shape(x::Base.Slice{T}) where {T <: OptionallyStaticRange} = Base.length(x)
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
function Base.axes1(x::OptionallyStaticStepRange{StaticInt{F}, StaticInt{S}, StaticInt{L}}) where {
                                                                                                   F,
                                                                                                   S,
                                                                                                   L
                                                                                                   }
    OptionallyStaticUnitRange(StaticInt(1), StaticInt(_range_length(F, S, L)))
end
Base.axes1(x::Base.Slice{<:OptionallyStaticUnitRange{One}}) = x.indices
Base.axes1(x::Base.Slice{<:OptionallyStaticRange}) = Base.IdentityUnitRange(x.indices)

Base.:(-)(r::OptionallyStaticRange) = (-static_first(r)):(-static_step(r)):(-static_last(r))

function Base.reverse(x::OptionallyStaticUnitRange)
    _OptionallyStaticStepRange(getfield(x, :stop), StaticInt(-1), getfield(x, :start))
end
function Base.reverse(x::OptionallyStaticStepRange)
    _OptionallyStaticStepRange(getfield(x, :stop), -getfield(x, :step), getfield(x, :start))
end

function Base.show(io::IO, ::MIME"text/plain", @nospecialize(r::OptionallyStaticUnitRange))
    print(io, "$(getfield(r, :start)):$(getfield(r, :stop))")
end
function Base.show(io::IO, ::MIME"text/plain", @nospecialize(r::OptionallyStaticStepRange))
    print(io, "$(getfield(r, :start)):$(getfield(r, :step)):$(getfield(r, :stop))")
end

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
