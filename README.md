# Static

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://sciml.github.io/Static.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://sciml.github.io/Static.jl/dev)
[![Build Status](https://github.com/SciML/Static.jl/workflows/CI/badge.svg)](https://github.com/SciML/Static.jl/actions)
[![Coverage](https://codecov.io/gh/SciML/Static.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/SciML/Static.jl)

`Static` defines a limited set of statically parameterized types and a common interface that is shared between them. Defining a new static type that conforms with this interface only requires defining the following:

* `Static.static(::T)` - given the non-static type `T` return its static counterpart.
* `Static.is_static(::Type{S})` - given the static type `S` return `True()`.
* `Static.known(::Type{S})`- given the static type `S` return the known non-static value.

Fore example, the following would appropriately define the interface for `StaticChar`

```julia
Static.static(x::Char) = StaticChar(x)
Static.is_static(::Type{T}) where {T<:StaticChar} = True()
Static.known(::Type{StaticChar{C}}) where {C} = C::Char
```
