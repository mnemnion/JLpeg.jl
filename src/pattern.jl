# Patterns for JLpeg parser engine

"""
Container for various patterns and grammars.
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
    val::Int
    code::Vector{Instruction}
    PAny(val::Int) = new(val, Inst())
end

struct PSeq <: Pattern 
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
    val = [a, b]
    PSeq(val, Inst())
end