using SciMLTesting, Static, Test

run_qa(
    Static;
    api_docs_kwargs = (; rendered = true),
    explicit_imports = true,
    aqua_kwargs = (; ambiguities = (; recursive = false)),
    # Aqua piracies: `Base.promote_shape` overload on `Tuple{Vararg{Union{Int,StaticInt}}}`
    # subsumes plain `Tuple{Vararg{Int}}` — genuine Base piracy.
    # Tracked in https://github.com/SciML/Static.jl/issues/181
    aqua_broken = (:piracies,),
    ei_kwargs = (;
        # Base / Base.Broadcast / Base.IteratorsMD internals Static accesses but that
        # are not (yet) declared public upstream; ignore until Base marks them public.
        all_qualified_accesses_are_public = (;
            ignore = (
                Symbol("@propagate_inbounds"), :AbstractCartesianIndex, :Cartesian,
                :Fix2, :IdentityUnitRange, :IteratorsMD, :OneTo, :Slice,
                :TwicePrecision, :axes1, :axistype, :fptosi, :front, :isdone,
                :issingletontype, :oneto, :setindex, :sitofp, :split, :tail,
                :to_index, :to_shape,
            ),
        ),
        # `ifelse` is exported-but-not-declared-public by IfElse.jl (its sole purpose).
        all_explicit_imports_are_public = (; ignore = (:ifelse,)),
    ),
)
