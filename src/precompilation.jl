# Precompilation workload for Static.jl
# This file precompiles commonly used code paths to reduce TTFX

using PrecompileTools: @compile_workload, @setup_workload

@setup_workload begin
    # Minimal setup - define some test values
    @compile_workload begin
        # Core static values with common types
        x1 = static(1)
        x2 = static(2)
        x3 = static(3)
        x4 = static(4)

        # Arithmetic operations
        _ = x1 + x2
        _ = x2 - x1
        _ = x1 * x2
        _ = x2 / x1

        # StaticBool operations
        t = True()
        f = False()
        _ = ifelse(t, 1, 2)
        _ = ifelse(f, 1, 2)
        _ = !t
        _ = !f
        _ = t & f
        _ = t | f

        # StaticFloat64 operations
        f1 = StaticFloat64(1.0)
        f2 = StaticFloat64(2.0)
        _ = f1 + f2
        _ = f2 - f1
        _ = f1 * f2
        _ = f2 / f1

        # StaticSymbol operations
        _ = StaticSymbol(:x)
        _ = StaticSymbol(:y)

        # known/dynamic conversions (8.5ms TTFX)
        _ = known(typeof(x1))
        _ = known(typeof(t))
        _ = known(typeof(f1))
        _ = dynamic(x1)
        _ = dynamic(t)
        _ = dynamic(f1)

        # is_static checks
        _ = is_static(typeof(x1))
        _ = is_static(Int)

        # Range operations (heavily used in array indexing)
        r1 = static(1):static(10)
        _ = length(r1)
        _ = first(r1)
        _ = last(r1)

        r2 = static(1):static(2):static(10)
        _ = length(r2)
        _ = step(r2)

        r3 = 1:static(10)
        _ = length(r3)

        r4 = static(1):10
        _ = length(r4)

        # NDIndex operations
        _ = NDIndex(x1, 2, x3)
        _ = NDIndex(1, 2, 3)
        _ = NDIndex(x1, x2)

        # Tuple operations with reduce_tup (72ms TTFX - biggest bottleneck!)
        # Precompile for common tuple sizes and element types
        _ = reduce_tup(+, (1, 2))
        _ = reduce_tup(+, (1, 2, 3))
        _ = reduce_tup(+, (1, 2, 3, 4))
        _ = reduce_tup(+, (1, 2, 3, 4, 5))
        _ = reduce_tup(+, (1, 2, 3, 4, 5, 6))
        _ = reduce_tup(+, (1, 2, 3, 4, 5, 6, 7, 8))

        _ = reduce_tup(+, (1.0, 2.0))
        _ = reduce_tup(+, (1.0, 2.0, 3.0))
        _ = reduce_tup(+, (1.0, 2.0, 3.0, 4.0))

        _ = reduce_tup(*, (1, 2, 3, 4))
        _ = reduce_tup(*, (1.0, 2.0, 3.0, 4.0))

        # eachop operations (5.8ms TTFX)
        itr2 = nstatic(Val(2))
        itr3 = nstatic(Val(3))
        itr4 = nstatic(Val(4))
        _ = eachop(identity, itr2)
        _ = eachop(identity, itr3)
        _ = eachop(identity, itr4)
        _ = eachop(getindex, itr4, (1, 2, 3, 4))

        # Comparison operations (6ms TTFX)
        _ = lt(x1, x2)
        _ = le(x1, x2)
        _ = gt(x2, x1)
        _ = ge(x2, x1)
        _ = eq(x1, x1)
        _ = ne(x1, x2)

        # static_promote operations
        _ = static_promote(x1, x1)

        # Permute operations (used internally)
        _ = permute((1, 2, 3), (x3, x2, x1))

        # Integer/float conversions
        _ = Int(x1)
        _ = Float64(x1)

        # Base.ntuple with StaticInt
        _ = ntuple(identity, x4)
    end
end
