
"""
    StaticSymbol

A statically typed `Symbol`.
"""
struct StaticSymbol{s}
    StaticSymbol{s}() where {s} = new{s::Symbol}()
    StaticSymbol(s::Symbol) = new{s}()
    StaticSymbol(x::StaticSymbol) = x
    StaticSymbol(x) = StaticSymbol(Symbol(x))
end
StaticSymbol(x, y) = StaticSymbol(Symbol(x, y))
StaticSymbol(x::StaticSymbol, y::StaticSymbol) = _cat_syms(x, y)
@generated function _cat_syms(::StaticSymbol{x}, ::StaticSymbol{y}) where {x,y}
    :(StaticSymbol{$(QuoteNode(Symbol(x, y)))}())
end
StaticSymbol(x, y, z...) = StaticSymbol(StaticSymbol(x, y), z...)

Base.Symbol(@nospecialize(s::StaticSymbol)) = known(s)

Base.:(==)(@nospecialize(x::StaticSymbol), @nospecialize(y::StaticSymbol)) = x === y
Base.:(==)(@nospecialize(x::StaticSymbol), y::Symbol) = known(typeof(x)) === y
Base.:(==)(x::Symbol, @nospecialize(y::StaticSymbol)) = x === known(typeof(y))

