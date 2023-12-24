# Patterns for JLpeg parser engine

using Match

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

struct PSet <: Pattern
    val::AbstractString
    code::Vector{Instruction}
    PSet(val::AbstractString) = new(val, Inst())
end

struct PRange <: Pattern
    val::Tuple{AbstractChar, AbstractChar}
    code::Vector{Instruction}
    function PRange(str::AbstractString)
        a, b = (nothing, nothing)
        for (idx, char) in enumerate(str)
            if idx == 1
                a = char
            elseif idx == 2
                b = char 
            else
                error("Range must be two characters")
            end
        end
        if a ≥ b
            error(lazy"Range must be from low to high, got $a > $b")
        end
        new((a, b), Inst())
    end
end

struct PAny <: Pattern
    val::UInt32
    code::Vector{Instruction}
    PAny(val::UInt) = new(val, Inst())
end

struct PStar <: Pattern
    val::Tuple{Pattern,Int}
    code::Vector{Instruction}
    PStar(patt::Pattern, n::UInt) = new((patt, n), Inst())
end

struct PSeq <: Pattern 
    val::Vector{Pattern}
    code::Vector{Instruction}
end

struct PChoice <: Pattern
    val::Vector{Pattern}
    code::Vector{Instruction}
end

struct PTrue <: Pattern 
   val::Nothing
   code::Vector{Instruction}
   PTrue() = new(nothing, Inst())
end

struct PFalse <: Pattern 
    val::Nothing
    code::Vector{Instruction}
    PFalse() = new(nothing, Inst())
 end 

# TODO the rest of these need to be concrete:

abstract type PRunTime <:Pattern end
abstract type PNot <:Pattern end
abstract type PBehind <:Pattern end
abstract type PCapture <:Pattern end
abstract type PGrammar <:Pattern end
abstract type PRule <:Pattern end
abstract type PTXInfo <:Pattern end
abstract type PAnd <:Pattern end
abstract type PThrow <:Pattern end
abstract type PCall <:Pattern end
abstract type POpenCall <:Pattern end

# TODO this lets me smuggle them into tests and (with caution!) optimizations 

function PSeq(str::AbstractString)
    val = Vector{Pattern}(undef, 0)
    code = Inst()
    if length(str) == 0
        push!(val, PTrue())
    end
    for char in str
        push!(val, PChar(char))
    end
    PSeq(val, code)
end


function PSeq(a::Pattern, b::Pattern)
    val = optimizePSeq(a, b)
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

# A choice between two sets is just the union of those sets
optimizePChoice(a::PSet, b::PSet) = [PSet(a.val * b.val)]
optimizePChoice(a::Pattern, b::Pattern) = [a, b]

P(s::AbstractString) = PSeq(s)
P(c::AbstractChar) = PChar(c)
P(n::UInt) = PAny(n)
P(b::Bool) = if b PTrue() else PFalse() end
S(s::AbstractString) = PSet(s)
R(s::AbstractString) = PRange(s)

Base.:*(a::Pattern, b::Pattern) = PSeq(a, b)
Base.:|(a::Pattern, b::Pattern) = PChoice(a, b)
Base.:^(a::Pattern, b::Int)  = PStar(a, b)