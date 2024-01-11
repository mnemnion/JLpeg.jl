using Documenter
using JLpeg
DocMeta.setdocmeta!(JLpeg, :DocTestSetup, :(using JLpeg); recursive=true)

makedocs(
    sitename  =  "JLpeg",
    format    =  Documenter.HTML(),
    modules   =  [JLpeg],
    pages = [
        "JLpeg Guide" => "index.md",
        "reference.md",
        "internals.md",
    ],
    checkdocs =  :exports,
    # , checkdocs = :none
    # , doctest = :fix
)

deploydocs(
    repo = "github.com/mnemion/JLpeg.jl.git",
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
