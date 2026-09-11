using Static, BenchmarkTools
using StableRNGs

const SUITE = BenchmarkGroup()
const rng = StableRNG(123)

x = rand(rng, 100)
s3 = static(3)
s_true = static(true)

# =============================================================================
# static / dynamic conversions and queries
# =============================================================================

SUITE["convert"] = BenchmarkGroup()

SUITE["convert"]["static"] = @benchmarkable static(3)
SUITE["convert"]["dynamic"] = @benchmarkable dynamic($s3)
SUITE["convert"]["known"] = @benchmarkable known($s3)
SUITE["convert"]["is_static"] = @benchmarkable is_static($s3)
SUITE["convert"]["static_promote"] = @benchmarkable static_promote($s3, 3)

# =============================================================================
# Arithmetic on static values (compile-time specialization paths)
# =============================================================================

SUITE["arith"] = BenchmarkGroup()

SUITE["arith"]["add"] = @benchmarkable $s3 + $s3
SUITE["arith"]["mul"] = @benchmarkable $s3 * $s3

SUITE["arith"]["lt"] = @benchmarkable $s3 < static(5)

# =============================================================================
# Static-bool logic
# =============================================================================

SUITE["logic"] = BenchmarkGroup()

SUITE["logic"]["and"] = @benchmarkable $s_true & static(true)
SUITE["logic"]["ifelse"] = @benchmarkable ifelse($s_true, 1, 2)

# =============================================================================
# NDIndex iteration
# =============================================================================

SUITE["ndindex"] = BenchmarkGroup()

SUITE["ndindex"]["construct"] = @benchmarkable NDIndex(static(1), 2, static(3))
