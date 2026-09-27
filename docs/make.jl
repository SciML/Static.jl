using Static
using Documenter

makedocs(;
    modules = [Static],
    authors = "chriselrod, ChrisRackauckas, Tokazama",
    repo = Documenter.Remotes.GitHub("SciML", "Static.jl"),
    sitename = "Static.jl",
    format = Documenter.HTML(;
        prettyurls = get(ENV, "CI", "false") == "true",
        canonical = "https://SciML.github.io/Static.jl",
        assets = String[]
    ),
    pages = [
        "Home" => "index.md",
        "Public API" => "public_api.md",
        "Developer API" => "developer_api.md",
    ]
)

deploydocs(;
    repo = "github.com/SciML/Static.jl"
)
