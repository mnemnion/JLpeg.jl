
module JLpeg

include("vm.jl")
include("dialects.jl")

export P, S, R, C, Cg, Cp, Cr, A, Anow, ←, ¬, Rule, Grammar, Pattern
export match, compile!, extrasugar, modulesugar
export  @P_str, @R_str, @S_str, @grammar, @rule
export re

end
