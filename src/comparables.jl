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
    @eval begin
        $alt(x::X) where {X} = Base.Fix2($alt, x)
        $alt(x::X, y::Y) where {X,Y} =
            ifelse(is_static(X) & is_static(Y), static, identity)(Base.$op(x,y))
     end
end

#=
for f in [:(==), :(!=), :(<), :(≤), :(>), :(≥)]
    @eval begin
        @inline Base.$f(::StaticInt{M}, ::StaticInt{N}) where {M,N} = $f(M, N)
        @inline Base.$f(::StaticInt{M}, x::Int) where {M} = $f(M, x)
        @inline Base.$f(x::Int, ::StaticInt{M}) where {M} = $f(x, M)
    end
end
=#
