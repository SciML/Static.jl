using Static
using Documenter

makedocs(;
    modules=[Static],
    authors="chriselrod, ChrisRackauckas, Tokazama",
    repo="https://github.com/chriselrod/Static.jl/blob/{commit}{path}#L{line}",
    sitename="Static.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://chriselrod.github.io/Static.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/chriselrod/Static.jl",
)
