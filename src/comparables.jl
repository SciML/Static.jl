#=
    alternative comparison operators

    | Base op | alt fn |
    |---------|--------|
    |   ==    |   eq   |
    |   !=    |   ne   |
    |   <=    |   le   |
    |   <     |   lt   |
    |   >=    |   ge   |
    |   >     |   gt   |

    for most signatures alt_fn aliases the Base op
        alt_fn(x, y) = op(x, y)
    iff each of x, y are <: Union{<static_types>}
       alt_fn(x, y} = StaticBool(op(dynamic(x), dynamic(y)))
=#
for (alt, op) in [(:(eq), :(==)), (:(ne), :(!=)), (:(le), :(<=)),
                  (:(lt), :(<)),  (:(ge), :(>=)), (:(gt), :(<))]
    for (S,T) in ((:StaticBool, :Bool), (:StaticInt, :Int), (:StaticFloat64, :Float64))
        @eval begin
            $alt(x::$S{X}, y::$S{Y}) where {X,Y} = static(Base.$op(X,Y))
            $alt(x::$S{X}, y::$T) where {X} = Base.$op(X,y)
            $alt(x::$T, y::$S{Y}) where {Y} = Base.$op(x,Y)
        end
    end    
end

for alt in [:eq, :ne, :le, :lt, :ge, :gt]
    @eval $alt(x::T) where T = Base.Fix2($alt, x)
end
    
