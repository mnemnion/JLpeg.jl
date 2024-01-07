using Documenter
using JLpeg
DocMeta.setdocmeta!(JLpeg, :DocTestSetup, :(using JLpeg); recursive=true)

makedocs(
    sitename  =  "JLpeg",
    format    =  Documenter.HTML(),
    modules   =  [JLpeg],
    checkdocs =  :exports,
    # , checkdocs = :none
    # , doctest = :fix
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
