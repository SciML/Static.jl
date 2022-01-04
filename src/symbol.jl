
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
    return :(StaticSymbol{$(QuoteNode(Symbol(x, y)))}())
end
StaticSymbol(x, y, z...) = StaticSymbol(StaticSymbol(x, y), z...)

Base.Symbol(::StaticSymbol{s}) where {s} = s::Symbol

Base.:(==)(::StaticSymbol{X}, ::StaticSymbol{Y}) where {X,Y} = X === Y
Base.:(==)(@nospecialize(x::StaticSymbol), y::Symbol) = dynamic(x) === y
Base.:(==)(x::Symbol, @nospecialize(y::StaticSymbol)) = x === dynamic(x)

Base.show(io::IO, ::StaticSymbol{s}) where {s} = print(io, "static(:$s)")

