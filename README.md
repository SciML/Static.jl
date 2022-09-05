# Static

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://sciml.github.io/Static.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://sciml.github.io/Static.jl/dev)
[![Build Status](https://github.com/SciML/Static.jl/workflows/CI/badge.svg)](https://github.com/SciML/Static.jl/actions)
[![Coverage](https://codecov.io/gh/SciML/Static.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/SciML/Static.jl)

`Static` defines a set of types (`True`, `False`, `StaticInt{value::Int}`, `StaticFloat64{value::Float64}`, `StaticSymbol{value::Symbol}`) that may be dispatched on (similar to `Base.Val{value}`). Unlike `Base.Val`, instances of these types provide "static" values (meaning known at compile time) that in many cases work interchangeably with dynamic values (meaning the value is known at runtime but not compile time). This is particularly useful when designing types whose fields may be dynamically or statically known, such as a range whose size may be dynamic or statically known.

Generic conversion to static values, dynamic values, and compile time known values is accomplished with the methods `static`, `dynamic`, and `known`, respectively.

```julia
julia> using Static

julia> static(1)
static(1)

julia> dynamic(static(1))
1

julia> dynamic(1)
1

julia> typeof(static(1))
StaticInt{1}

julia> known(typeof(static(1)))
1

julia> known(typeof(1)) === nothing  # `Int`  has no compile time known value
true

```
