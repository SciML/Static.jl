using Static
using BenchmarkTools
using Test

# Helper function to test zero allocations using BenchmarkTools
# BenchmarkTools handles variable interpolation properly to avoid closure boxing
function test_zero_allocs(name, setup, expr)
    b = eval(quote
        let $setup
            @benchmark $expr
        end
    end)
    allocs = minimum(b).allocs
    @test allocs == 0
    allocs == 0 || @warn "$name allocates: $allocs allocations"
end

@testset "Allocation Tests" begin
    @testset "Core Type Operations" begin
        # static() on StaticInt should not allocate
        x42 = static(42)
        @test (@allocated static(x42)) == 0

        # dynamic() should not allocate
        @test (@allocated dynamic(x42)) == 0

        # known() should not allocate
        @test (@allocated known(x42)) == 0
    end

    @testset "Arithmetic Operations" begin
        x = static(10)
        y = static(5)

        # Binary operations on StaticInt should not allocate
        b = @benchmark $x + $y
        @test minimum(b).allocs == 0

        b = @benchmark $x * $y
        @test minimum(b).allocs == 0

        b = @benchmark $x - $y
        @test minimum(b).allocs == 0

        b = @benchmark $x ÷ $y
        @test minimum(b).allocs == 0
    end

    @testset "Comparison Operations" begin
        x = static(10)
        y = static(5)

        b = @benchmark Static.eq($x, $y)
        @test minimum(b).allocs == 0

        b = @benchmark Static.lt($x, $y)
        @test minimum(b).allocs == 0

        b = @benchmark Static.gt($x, $y)
        @test minimum(b).allocs == 0

        b = @benchmark Static.le($x, $y)
        @test minimum(b).allocs == 0

        b = @benchmark Static.ge($x, $y)
        @test minimum(b).allocs == 0

        b = @benchmark Static.ne($x, $y)
        @test minimum(b).allocs == 0
    end

    @testset "Range Operations" begin
        r = Static.OptionallyStaticUnitRange(static(1), static(100))

        b = @benchmark first($r)
        @test minimum(b).allocs == 0

        b = @benchmark last($r)
        @test minimum(b).allocs == 0

        b = @benchmark length($r)
        @test minimum(b).allocs == 0

        b = @benchmark $r[50]
        @test minimum(b).allocs == 0

        # Step range operations
        sr = Static.OptionallyStaticStepRange(static(1), static(2), static(99))

        b = @benchmark first($sr)
        @test minimum(b).allocs == 0

        b = @benchmark step($sr)
        @test minimum(b).allocs == 0

        b = @benchmark length($sr)
        @test minimum(b).allocs == 0
    end

    @testset "Tuple Operations" begin
        t4 = (1, 2, 3, 4)
        t8 = (1, 2, 3, 4, 5, 6, 7, 8)

        # reduce_tup should not allocate
        b = @benchmark Static.reduce_tup(+, $t4)
        @test minimum(b).allocs == 0

        b = @benchmark Static.reduce_tup(+, $t8)
        @test minimum(b).allocs == 0

        # find_first_eq should not allocate
        b = @benchmark Static.find_first_eq(static(3), $t8)
        @test minimum(b).allocs == 0

        b = @benchmark Static.find_first_eq(3, $t8)
        @test minimum(b).allocs == 0

        # eachop should not allocate
        itr = Static.nstatic(Val(4))
        b = @benchmark Static.eachop(getindex, $itr, $t8)
        @test minimum(b).allocs == 0

        # permute should not allocate
        perm = (static(4), static(3), static(2), static(1))
        b = @benchmark Static.permute($t4, $perm)
        @test minimum(b).allocs == 0

        # invperm should not allocate
        b = @benchmark invperm($perm)
        @test minimum(b).allocs == 0
    end

    @testset "NDIndex Operations" begin
        idx1 = NDIndex(static(1), static(2), static(3))
        idx2 = NDIndex(static(2), static(3), static(4))

        # getindex should not allocate
        b = @benchmark $idx1[static(1)]
        @test minimum(b).allocs == 0

        b = @benchmark $idx1[1]
        @test minimum(b).allocs == 0

        # arithmetic should not allocate
        b = @benchmark $idx1 + $idx2
        @test minimum(b).allocs == 0

        b = @benchmark $idx1 - $idx2
        @test minimum(b).allocs == 0

        # comparison should not allocate
        b = @benchmark isless($idx1, $idx2)
        @test minimum(b).allocs == 0

        # min/max should not allocate
        b = @benchmark min($idx1, $idx2)
        @test minimum(b).allocs == 0

        b = @benchmark max($idx1, $idx2)
        @test minimum(b).allocs == 0
    end

    @testset "StaticFloat64 Operations" begin
        sf = static(0.5)

        b = @benchmark sqrt($sf)
        @test minimum(b).allocs == 0

        b = @benchmark sin($sf)
        @test minimum(b).allocs == 0

        b = @benchmark cos($sf)
        @test minimum(b).allocs == 0

        b = @benchmark exp($sf)
        @test minimum(b).allocs == 0
    end
end
