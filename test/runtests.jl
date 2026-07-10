if get(ENV, "GROUP", "") == "QA"
    import Pkg
    Pkg.activate(joinpath(@__DIR__, "qa"))
    Pkg.instantiate()
end

using SciMLTesting
run_tests()
