
@inline nstatic(::Val{N}) where {N} = ntuple(StaticInt, Val(N))

invariant_permutation(::Any, ::Any) = False()
function invariant_permutation(x::T, y::T) where {N,T<:Tuple{Vararg{StaticInt,N}}}
    if x === nstatic(Val(N))
        return True()
    else
        return False()
    end
end

permute(x::Tuple, perm::Val) = permute(x, static(perm))
permute(x::Tuple{Vararg{Any}}, perm::Tuple{Vararg{StaticInt}}) = eachop(getindex, perm, x)
function permute(x::Tuple{Vararg{Any,K}}, perm::Tuple{Vararg{StaticInt,K}}) where {K}
    if invariant_permutation(perm, perm) === False()
        return eachop(getindex, perm, x)
    else
        return x
    end
end

"""
    eachop(op, args...; iterator::Tuple{Vararg{StaticInt}}) -> Tuple

Produces a tuple of `(op(args..., iterator[1]), op(args..., iterator[2]),...)`.
"""
eachop(op, itr, args...) = _eachop(op, itr, args)
@generated function _eachop(op, ::I, args::A) where {A,I}
    t = Expr(:tuple)
    narg = length(A.parameters)
    for p in I.parameters
        call_expr = Expr(:call, :op)
        if narg > 0
            for i in 1:narg
                push!(call_expr.args, :(getfield(args, $i)))
            end
        end
        push!(call_expr.args, :(StaticInt{$(p.parameters[1])}()))
        push!(t.args, call_expr)
    end
    Expr(:block, Expr(:meta, :inline), t)
end

"""
    eachop_tuple(op, arg, args...; iterator::Tuple{Vararg{StaticInt}}) -> Type{Tuple}

Produces a tuple type of `Tuple{op(arg, args..., iterator[1]), op(arg, args..., iterator[2]),...}`.
Note that if one of the arguments passed to `op` is a `Tuple` type then it should be the first argument
instead of one of the trailing arguments, ensuring type inference of each element of the tuple.
"""
eachop_tuple(op, itr, arg, args...) = _eachop_tuple(op, itr, arg, args)
@generated function _eachop_tuple(op, ::I, arg, args::A) where {A,I}
    t = Expr(:curly, Tuple)
    narg = length(A.parameters)
    for p in I.parameters
        call_expr = Expr(:call, :op, :arg)
        if narg > 0
            for i in 1:narg
                push!(call_expr.args, :(getfield(args, $i)))
            end
        end
        push!(call_expr.args, :(StaticInt{$(p.parameters[1])}()))
        push!(t.args, call_expr)
    end
    Expr(:block, Expr(:meta, :inline), t)
end


#=
    find_first_eq(x, collection::Tuple)

Finds the position in the tuple `collection` that is exactly equal (i.e. `===`) to `x`.
If `x` and `collection` are static (`is_static`) and `x` is in `collection` then the return
value is a `StaticInt`.
=#
@generated function find_first_eq(x::X, itr::I) where {X,N,I<:Tuple{Vararg{Any,N}}}
    if (is_static(X) & is_static(I)) === True()
        return Expr(:block, Expr(:meta, :inline),
            :(Base.Cartesian.@nif $(N + 1) d->(x === getfield(itr, d)) d->(static(d)) d->(nothing)))
    else
        return Expr(:block, Expr(:meta, :inline),
            :(Base.Cartesian.@nif $(N + 1) d->(x === getfield(itr, d)) d->(d) d->(nothing)))
    end
end

