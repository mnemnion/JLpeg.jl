# Patterns for JLpeg parser engine

"""
Container for various patterns and grammars.
Always has `val`, which may be primitive or a Vector{Pattern},
and `code`, an IVector. Some patterns have a field
unique to that pattern type.
"""
abstract type Pattern <: AbstractPattern end

"""
    PegError(msg)

An error while constructing a JLPeg Pattern.
"""
struct PegError <: Exception
    msg::String
end

"A bytecode instruction"
abstract type Instruction end

"A kind of capture"
@enum CapKind::UInt8 begin
    Cposition   # ✅ Captures the empty string to record an offset
    Cconst      # [ ]
    Cbackref    # Might be a different mechanism
    Csimple     # ✅ captures a substring of the region matched
    Crange      # ✅ captures a UnitRange [first:last] of region
    Cexpr       # [ ] Captures the group and makes an Expr with head :symbol
    Caction     # [ ] an action taken on a successful match.
    Csymbol     # ✅ captures its match as a pair `:symbol => "match"` (:symbol can be a string)
    Cfold       # [ ] # TODO figure out the definition
    Cruntime    # [ ] a Caction but applied at runtime
    Cgroup      # ✅ groups all its captures into a Vector.
end

"A Vector of `Instructions` representing a complete pattern."
const IVector = Vector{Instruction}

"""
    AuxDict = Dict{Symbol, Any}

The `.aux` field of any compound `Pattern`, contains the auxiliary data
needed to correctly compile the pattern.  Everything in any `patt.val[n].aux`
is merged with `patt.aux`, through a non-trivial algorithm aka the shape is
somtimes transformed. Like all pattern data, `patt.aux` is treated as immutable
after `compile!` returns.

Possible fields include:

- `:cap`:  The supplemental information of a PCapture. This is promoted within the
           PCapture to `:caps`, see below. Type of the value is based on `CapKind`:
    - `CSimple`: `:cap isa Nothing`
    - `CSymbol`: `:cap isa Symbol`
    - To Be continued...

- `:caps`:  A mapping of capture closing `Instruction`s to the value of `:cap` for
            the originating capture. Will be either a Pair or a Dict, depending.

**To Be Continued**

"""
const AuxDict = Dict{Symbol, Any}

function Inst()
    IVector(undef, 0)
end

struct PChar <: Pattern
    val::AbstractChar
    code::IVector
    PChar(val::AbstractChar) = new(val, Inst())
end

struct PSet <: Pattern
    val::AbstractString
    code::IVector
    PSet(val::AbstractString) = new(val, Inst())
    PSet(a, b) = new(a, b)
end

"special PSet constructor direct from BitVec opcode"
function PSet(val::IVector)
    PSet("", val)
end

struct PRange <: Pattern
    val::Tuple{AbstractChar, AbstractChar}
    code::IVector
    function PRange(str::AbstractString)
        a, b = (nothing, nothing)
        for (idx, char) in enumerate(str)
            if idx == 1
                a = char
            elseif idx == 2
                b = char
            else
                @error lazy"Range must be two characters: $char"
            end
        end
        if a ≥ b
            error(lazy"Range must be from low to high, got $a > $b")
        end
        new((a, b), Inst())
    end
    PRange(a::AbstractChar, b::AbstractChar) = new((a, b), Inst())
end

struct PBehind <: Pattern
    val::Vector{Pattern}
    code::IVector
    aux::AuxDict
    PBehind(val::Pattern) = new([val], Inst(), Dict())
end

struct PAny <: Pattern
    val::UInt32
    code::IVector
    PAny(val::UInt) = new(val, Inst())
end

struct PAnd <: Pattern
    val::Vector{Pattern}
    code::IVector
    aux::AuxDict
    PAnd(val::Pattern) = new([val], Inst(), Dict())
end

struct PNot <: Pattern
    val::Vector{Pattern}
    code::IVector
    aux::AuxDict
    PNot(val::Pattern) = new([val], Inst(), Dict())
end

struct PDiff <: Pattern
    val::Vector{Pattern}
    code::IVector
    aux::AuxDict
    PDiff(a::Pattern, b::Pattern) = new([a, b], Inst(), Dict())
end

"Includes n, dictating the sort of repetition"
struct PStar <: Pattern
    val::Vector{Pattern}
    code::IVector
    aux::AuxDict
    n::Int
    PStar(patt::Pattern, n::Int) = new([patt], Inst(), Dict(), n)
end

struct PSeq <: Pattern
    val::Vector{Pattern}
    code::IVector
    aux::AuxDict
end

struct PChoice <: Pattern
    val::Vector{Pattern}
    code::IVector
    aux::AuxDict
end

struct PTrue <: Pattern
   val::Nothing
   code::IVector
   PTrue() = new(nothing, Inst())
end

struct PFalse <: Pattern
    val::Nothing
    code::IVector
    PFalse() = new(nothing, Inst())
 end

struct POpenCall <: Pattern
    val::Symbol
    code::IVector
    POpenCall(sym::Symbol) = new(sym, Inst())
end

POpenCall(s::AbstractString) = POpenCall(Symbol(s))
# Make sure the macros work as often as possible:
POpenCall(p::POpenCall) = p

# TODO I'm not in fact using this and should do so or get rid of it.
struct PCall <: Pattern
    val::Symbol
    code::IVector
    aux::AuxDict
    ref::Pattern
    """
    PCall(patt::POpenCall, ref::Pattern)

    Create a PCall from a POpenCall once the reference is established.
    """
    function PCall(patt::POpenCall, ref::Pattern)
        new(patt.sym, patt.code, Dict(), ref)
    end
end

struct PRule <: Pattern
    val::Vector{Pattern}
    code::IVector
    name::Symbol
    aux::AuxDict
    PRule(name::Symbol, val::Pattern) = new([val], Inst(), name, Dict())
end


struct PGrammar <: Pattern
    val::Vector{PRule}
    code::IVector
    start::Symbol
    aux::AuxDict
    function PGrammar(start::PRule, rest::Vararg{PRule})
        start_sym = start.name
        val = [start]
        append!(val, rest)
        new(val, Inst(), start_sym, Dict())
    end
end

"""
Global count of capture pattern tags.

If you ever overflow this, please tell me what you were doing. -Sam
"""
capcounter::UInt16 = 0

struct PCapture <: Pattern
    val::Vector{Pattern}
    code::IVector
    kind::CapKind
    aux::AuxDict
    tag::UInt16
    function PCapture(a::Pattern, k::CapKind)
        global capcounter += 1
        new([a], Inst(), k, AuxDict(), capcounter)
    end
    function PCapture(a::Pattern, k::CapKind, aux::AuxDict)
        global capcounter += 1
        new([a], Inst(), k, aux, capcounter)
    end
end
# TODO the rest of these need to be concrete:

"""
Global count of throw label tags.

If you ever overflow this, _really_ tell me what you were doing. -Sam
"""
global throwcounter::UInt16 = 0
struct PThrow <: Pattern
    val::Symbol
    code::IVector
    tag::UInt16
    function PThrow(val::Symbol)
        global throwcounter += 1
        new(val, Inst(), throwcounter)
    end
end

abstract type PRunTime <:Pattern end
abstract type PTXInfo <:Pattern end

const PAuxT = Union{PAnd,PNot,PDiff,PStar,PSeq,PChoice,PCall,PRule,PGrammar,PCapture,PRunTime,PBehind}

# TODO add PCompiled for string-dumped rules and grammars

function PSeq(str::AbstractString)
    val = Vector{Pattern}(undef, 0)
    code = Inst()
    if length(str) == 0
        push!(val, PTrue())
    end
    for char in str
        push!(val, PChar(char))
    end
    PSeq(val, code, Dict())
end


function PSeq(a::Pattern, b::Pattern)
    val = optimizePSeq(a, b)
    PSeq(val, Inst(), AuxDict())
end

optimizePSeq(a::Pattern, b::Pattern) = [a, b]
optimizePSeq(a::PSeq, b::PSeq) = vcat(a.val, b.val)

function PChoice(a::Pattern, b::Pattern)
    val = optimizePChoice(a, b)
    PChoice(val, Inst(), AuxDict())
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
