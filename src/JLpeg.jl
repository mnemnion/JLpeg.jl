
module JLpeg

using BitPermutations
using OrderedCollections
using InteractiveUtils

export P, S, R, B, C, Cg, Cp, Cc, Cr, A, At, Avm!, T, M, K, CM, CK, Rule, Grammar, Pattern
export <--, ←, ¬, <|, <-->, ⟷, ↔
export PegMatch, PegFail, PegError
export match, compile!, generate
export @P_str, @R_str, @S_str, @grammar, @grammar!, @rule, @rule!
export re

include("types.jl")
include("pattern.jl")
include("interface.jl")
import JLpeg.Combinators: *, -, %, |, ^, ~, !, >>, >:, inv

include("compile.jl")
include("generator.jl")
include("pegmatch.jl")
include("vm.jl")
include("printing.jl")
include("grammar.jl")
include("dialects.jl")

end
