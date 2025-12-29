using ExplicitImports
using Static
using Test

@testset "ExplicitImports" begin
    @test check_no_implicit_imports(Static) === nothing
    @test check_no_stale_explicit_imports(Static) === nothing
end
