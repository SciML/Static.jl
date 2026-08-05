using Static
using Documenter

makedocs(;
    modules = [Static],
    authors = "chriselrod, ChrisRackauckas, Tokazama",
    repo = "https://github.com/SciML/Static.jl/blob/{commit}{path}#L{line}",
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
