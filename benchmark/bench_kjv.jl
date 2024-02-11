using BenchmarkTools
using JLpeg

kjv = read("kingjames.txt", String)

trials = BenchmarkGroup()

people = compile!(((P"Abraham",) | (P"Sarah",) | (P"Moses",) | (P"Matthew",) | (P"Tubalcain",) | P(1))^1)
kjv_end = compile!(P"" >> P"www.gutenberg.org.")
fourteen = compile!(P"" >> ((R"az" | R"AZ")^[14],))

trials["people"] = @benchmarkable match($people, $kjv)
trials["to_end"] = @benchmarkable match($kjv_end, $kjv)
trials["fourteen"] = @benchmarkable match($fourteen, $kjv)

tune!(trials)