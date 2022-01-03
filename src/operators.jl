
"""
    eq(x, y)

Equivalent to `!=` but if `x` and `y` are both static returns a `StaticBool.
"""
eq(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x == y)
eq(x::X) where {X} = Fix2(eq, x)

"""
    ne(x, y)

Equivalent to `!=` but if `x` and `y` are both static returns a `StaticBool.
"""
ne(x::X, y::Y) where {X,Y} = !eq(x, y)
ne(x::X) where {X} = Fix2(ne, x)

"""
    gt(x, y)

Equivalent to `>` but if `x` and `y` are both static returns a `StaticBool.
"""
gt(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x > y)
gt(x::X) where {X} = Fix2(gt, x)

"""
    ge(x, y)

Equivalent to `>=` but if `x` and `y` are both static returns a `StaticBool.
"""
ge(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x >= y)
ge(x::X) where {X} = Fix2(ge, x)

"""
    le(x, y)

Equivalent to `<=` but if `x` and `y` are both static returns a `StaticBool.
"""
le(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x <= y)
le(x::X) where {X} = Fix2(le, x)

"""
    lt(x, y)

Equivalent to `<` but if `x` and `y` are both static returns a `StaticBool.
"""
lt(x::X, y::Y) where {X,Y} = ifelse(is_static(X) & is_static(Y), static, identity)(x < y)
lt(x::X) where {X} = Fix2(lt, x)

"""
    mul(x) -> Base.Fix2(*, x)
    mul(x, y) -> 

Equivalent to `*` but allows for lazy multiplication when passing functions.
"""
mul(x) = Fix2(*, x)
const Mul{X} = Fix2{typeof(*),X}

"""
    add(x) -> Base.Fix2(+, x)
    add(x, y) -> 

Equivalent to `+` but allows for lazy addition when passing functions.
"""
add(x) = Fix2(+, x)
const Add{X} = Fix2{typeof(+),X}

import Base: ∘
const compose = ∘
compose(x::Mul{Int}, ::Add{StaticInt{0}}) = x
compose(x::Mul{StaticInt{X}}, ::Add{StaticInt{0}}) where {X} = x
compose(x::Mul{StaticInt{0}}, ::Add{StaticInt{0}}) = x
compose(x::Mul{StaticInt{1}}, ::Add{StaticInt{0}}) = x
compose(x::Mul{StaticInt{0}}, y::Add{StaticInt{Y}}) where {Y} = x
compose(::Mul{StaticInt{1}}, y::Add{StaticInt{Y}}) where {Y} = y
compose(x::Mul{StaticInt{0}}, y::Add{Int}) = x
compose(::Mul{StaticInt{1}}, y::Add{Int}) = y
compose(::Mul{StaticInt{X}}, ::Mul{StaticInt{Y}}) where {X,Y} = Fix2(*, static(X * Y))
compose(x::Mul{StaticInt{0}}, y::Mul{Int}) = x
compose(::Mul{Int}, y::Mul{StaticInt{0}}) = y
compose(::Mul{StaticInt{1}}, y::Mul{Int}) = y
compose(x::Mul{Int}, ::Mul{StaticInt{1}}) = x

