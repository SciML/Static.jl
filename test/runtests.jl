const GROUP = get(ENV, "GROUP", "All")

if GROUP == "QA"
    import Pkg
    Pkg.activate(joinpath(@__DIR__, "qa"))
    Pkg.develop(Pkg.PackageSpec(path = joinpath(@__DIR__, "..")))
    Pkg.instantiate()
    include(joinpath(@__DIR__, "qa", "qa.jl"))
end

if GROUP == "All" || GROUP == "Core"
    include(joinpath(@__DIR__, "core_tests.jl"))
end
