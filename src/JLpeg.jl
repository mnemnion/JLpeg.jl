module JLpeg

# Write your package code here.
include("vm.jl")

export match, compile!, P, S, R, PSeq, PChar, PAny, PChoice
export Pattern, Instruction, @P_str, @R_str, @S_str

end
