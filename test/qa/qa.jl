using Static, Aqua, ExplicitImports
using Test

@testset "Aqua" begin
    Aqua.find_persistent_tasks_deps(Static)
    Aqua.test_ambiguities(Static, recursive = false)
    Aqua.test_deps_compat(Static)
    Aqua.test_piracies(Static, broken = true)
    Aqua.test_project_extras(Static)
    Aqua.test_stale_deps(Static)
    Aqua.test_unbound_args(Static)
    Aqua.test_undefined_exports(Static)
end

@testset "ExplicitImports" begin
    @test check_no_implicit_imports(Static) === nothing
    @test check_no_stale_explicit_imports(Static) === nothing
end
