using BenchmarkTools
using JLpeg

kjv = read("kingjames.txt", String)

suite = BenchmarkGroup()

const people = compile!(((P"Abraham",) | (P"Sarah",) | (P"Moses",) | (P"Matthew",) | (P"Tubalcain",) | P(1))^1)
const kjv_end = compile!(P"" >> P"www.gutenberg.org.")
const fourteen = compile!(P"" >> ((R"az" | R"AZ")^[14],))

suite["people"] = @benchmarkable match($people, $kjv)
suite["to_end"] = @benchmarkable match($kjv_end, $kjv)
suite["fourteen"] = @benchmarkable match($fourteen, $kjv)

# Might split this into its own file

@grammar brackets begin
    :brackets ← "[" * :brackets^0 * "]"
 end

compile!(brackets)

const longbracket = "["^1000000 * "]"^1000000

suite["brace_match"] = @benchmarkable match($brackets, $longbracket)

tune!(suite)