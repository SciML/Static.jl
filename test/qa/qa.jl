using SciMLTesting, Static, Test

run_qa(
    Static;
    aqua_kwargs = (; ambiguities = (; recursive = false)),
    ei_kwargs = (;
        all_qualified_accesses_are_public = (;
            ignore = (
                # Base requires these internal hooks to implement custom Cartesian indices.
                :AbstractCartesianIndex, :IteratorsMD, :setindex, :split,
                # Base's range and broadcast machinery dispatches through these internal
                # types and hooks when preserving custom axes.
                :IdentityUnitRange, :Slice, :TwicePrecision, :axes1, :axistype,
                :oneto, :to_shape,
            ),
        ),
    ),
)
