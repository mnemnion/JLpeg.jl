using Documenter
using DocumenterInterLinks
using JLpeg
DocMeta.setdocmeta!(JLpeg, :DocTestSetup, :(using JLpeg); recursive=true)

links = InterLinks(
    "Julia" => (
        "https://docs.julialang.org/en/v1/",
        joinpath(@__DIR__, "src/inventories", "Julia.toml")
    ),
)

makedocs(
    sitename  =  "JLpeg",
    format    =  Documenter.HTML(),
    modules   =  [JLpeg],
    pages = [
        "Introduction" => "index.md",
        "Manual" => Any[
            "Patterns and Combination" => "guide/patterns.md",
            "Captures and Actions" => "guide/captures.md",
            "Working with Matched Data" => "guide/pegmatch.md",
        ],
        "reference.md",
        "internals.md",
    ],
    checkdocs =  :exports,
    # , checkdocs = :none
    # , doctest = :fix
    plugins = [links, ],
)


deploydocs(
    repo = "github.com/mnemnion/JLpeg.jl.git",
    branch="gh-pages",
    devurl="dev",
    versions=["stable" => "v^", "v#.#"],
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
