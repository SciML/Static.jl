```@meta
CurrentModule = Static
```

# Developer API

The names on this page are versioned public API for package developers building static
indexing and inference utilities. They are not the primary user interface. Applications
should prefer the exported API unless they are implementing integrations that require these
lower-level contracts.

## Optionally Static Ranges

```@docs
OptionallyStaticRange
OptionallyStaticUnitRange
OptionallyStaticStepRange
SUnitRange
SOneTo
```

## Tuple Operations

```@docs
eachop
eachop_tuple
reduce_tup
```

## Static Comparisons

```@docs
eq
ne
gt
ge
le
lt
```

## Function Constructors

```@docs
mul
add
```

## Index

```@index
Pages = ["developer_api.md"]
```
