
module JLpeg

include("vm.jl")
include("dialects.jl")

export P, S, R, B, C, Cg, Cp, Cr, A, Anow, T, ←, ¬, Rule, Grammar, Pattern
export PegMatch, PegFail, PegError
export match, compile!, extrasugar, modulesugar
export  @P_str, @R_str, @S_str, @grammar, @rule
export re

end
