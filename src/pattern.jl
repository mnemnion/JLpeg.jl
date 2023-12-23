# Patterns for JLpeg parser engine

"""
Container for various patterns and grammars.
Always has `val`, which may be primitive or a Vector{Pattern},
and `code`, a Vector{Instruction}.
"""
abstract type Pattern end

"A bytecode instruction"
abstract type Instruction end


function Inst()
    Vector{Instruction}(undef, 0)
end

struct PChar <: Pattern
    val::AbstractChar
    code::Vector{Instruction}
    PChar(val::AbstractChar) = new(val, Inst())
end

struct PAny <: Pattern
    val::UInt32
    code::Vector{Instruction}
    PAny(val::UInt) = new(val, Inst())
end

struct PSeq <: Pattern 
    val::Vector{Pattern}
    code::Vector{Instruction}
end

struct PChoice<:Pattern
    val::Vector{Pattern}
    code::Vector{Instruction}
end

function PSeq(str::AbstractString)
    val = Vector{Pattern}(undef, 0)
    code = Inst()
    for char in str
        push!(val, PChar(char))
    end
    PSeq(val, code)
end


function PSeq(a::Pattern, b::Pattern)
    val = optimizeP(a, b)
    PSeq(val, Inst())
end

optimizePSeq(a::Pattern, b::Pattern) = [a, b]
optimizePSeq(a::PSeq, b::PSeq) = vcat(a.val, b.val)

function PChoice(a::Pattern, b::Pattern)
    val = optimizePChoice(a, b)
    PChoice(val, Inst())
end

optimizePChoice(a::PChoice, b::PChoice) = vcat(a.val, b.val)

function optimizePChoice(a::PChoice, b::Pattern)
    val = copy(a.val)
    push!(val, b)
    val
end

optimizePChoice(a::Pattern, b::Pattern) = [a, b]

P(s::AbstractString) = PSeq(s)
P(c::AbstractChar) = PChar(c)
P(n::UInt) = PAny(n)

Base.:*(a::Pattern, b::Pattern) = PSeq(a, b)
Base.:|(a::Pattern, b::Pattern) = PChoice(a, b)