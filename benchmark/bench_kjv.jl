using BenchmarkTools
using JLpeg

kjv = read("kingjames.txt", String)

trials = BenchmarkGroup()

people = ((P"Abraham",) | (P"Sarah",) | (P"Moses",) | (P"Matthew",) | (P"Tubalcain",) | P(1))^1
kjv_end = P"" >> P"www.gutenberg.org."

trials["people"] = @benchmarkable match($people, $kjv)
trials["to_end"] = @benchmarkable match($kjv_end, $kjv)