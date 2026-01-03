```@meta
CurrentModule = Static
```

# Static.jl

`Static.jl` provides a set of statically-parameterized types for dispatch that work seamlessly with dynamic values. These types enable compile-time optimizations while maintaining ergonomic interoperability with standard Julia types.

## Overview

The package defines several static types:
- `True` and `False` - Static booleans
- `StaticInt{N}` - Static integers
- `StaticFloat64{N}` - Static floating-point numbers
- `StaticSymbol{S}` - Static symbols

Unlike `Base.Val`, these types provide "static" values (known at compile time) that work interchangeably with dynamic values in many contexts.

## Quick Start

```julia
using Static

# Create static values
x = static(1)              # StaticInt{1}()
b = static(true)           # True()
s = static(:my_symbol)     # StaticSymbol{:my_symbol}()

# Convert back to dynamic
dynamic(static(1))         # 1

# Extract compile-time-known values from types
known(typeof(static(1)))   # 1
known(typeof(1))           # nothing
```

## Key Functions

The package provides three core conversion functions:
- `static(x)` - Convert a value to its static form
- `dynamic(x)` - Convert a static value back to its dynamic form
- `known(T)` - Extract the compile-time-known value from a type

## API Reference

```@index
```

```@autodocs
Modules = [Static]
```
