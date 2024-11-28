using JiL
using Documenter

DocMeta.setdocmeta!(JiL, :DocTestSetup, :(using JiL); recursive=true)

makedocs(;
    modules=[JiL],
    authors="António Menezes Leitão <antonio.menezes.leitao@gmail.com>",
    sitename="JiL.jl",
    format=Documenter.HTML(;
        canonical="https://aptmcl.github.io/JiL.jl",
        edit_link="master",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/aptmcl/JiL.jl",
    devbranch="master",
)
