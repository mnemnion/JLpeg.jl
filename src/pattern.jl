# Patterns for JLpeg parser engine

function Inst()
    IVector(undef, 0)
end

struct PChar <: Pattern
    val::AbstractChar
    code::IVector
    PChar(val::AbstractChar) = new(val, Inst())
    PChar(val::AbstractChar, code::IVector) = new(val, code)
end

const CharSet = UnitRangesSortedSet{Char}

struct PSet <: Pattern
    val::CharSet
    code::IVector
    PSet(val::AbstractString) = new(CharSet(collect(val)), Inst())
    PSet(val::Vector{AbstractChar}) = new(CharSet(val), Inst())
    PSet(val::Tuple{AbstractChar,AbstractChar}) = new(CharSet(val[1]:val[2]), Inst())
    PSet(val::CharSet) = new(val, Inst())
    PSet(inst::IVector) = new(CharSet(), inst)
    PSet(val::CharSet, inst::IVector) = new(val, inst)
end

struct PBehind <: Pattern
    val::PVector
    code::IVector
    aux::AuxDict
    PBehind(val::Pattern) = new([val], Inst(), Dict())
    PBehind(val::Pattern, code::IVector, aux::AuxDict) = new([val], code, aux)
end

struct PAny <: Pattern
    val::UInt32
    code::IVector
    PAny(val::UInt) = new(val, Inst())
    PAny(val::UInt, code::IVector) = new(val, code)
end

struct PByte <: Pattern
   val::UInt8
   code::IVector
   PByte(val::Integer) = new(UInt8(val), Inst())
   PByte(val::UInt8, code::IVector) = new(val, code)
end

struct PAnd <: Pattern
    val::PVector
    code::IVector
    aux::AuxDict
    PAnd(val::Pattern) = new([val], Inst(), Dict())
end

struct PNot <: Pattern
    val::PVector
    code::IVector
    aux::AuxDict
    PNot(val::Pattern) = new([val], Inst(), Dict())
    PNot(val::Pattern, code::IVector, aux::AuxDict) = new([val], code, aux)
end

struct PDiff <: Pattern
    val::PVector
    code::IVector
    aux::AuxDict
    PDiff(a::Pattern, b::Pattern) = new([a, b], Inst(), Dict())
    PDiff(val::Pattern, code::IVector, aux::AuxDict) = new([val], code, aux)
end

"Includes n, dictating the sort of repetition"
struct PStar <: Pattern
    val::PVector
    code::IVector
    aux::AuxDict
    n::Int
    PStar(patt::Pattern, n::Int) = new([patt], Inst(), Dict(), n)
    PStar(val::Pattern, code::IVector, aux::AuxDict, n) = new([val], code, aux, n)
end

struct PSeq <: Pattern
    val::PVector
    code::IVector
    aux::AuxDict
end

struct PChoice <: Pattern
    val::PVector
    code::IVector
    aux::AuxDict
end

struct PTrue <: Pattern
   val::Nothing
   code::IVector
   PTrue() = new(nothing, Inst())
   PTrue(val::Nothing, code::IVector) = new(val, code)
end

struct PFalse <: Pattern
    val::Nothing
    code::IVector
    PFalse() = new(nothing, Inst())
    PFalse(val::Nothing, code::IVector) = new(val, code)
 end

struct POpenCall <: Pattern
    val::Symbol
    code::IVector
end
POpenCall(sym::Symbol) = POpenCall(sym, Inst())

POpenCall(s::AbstractString) = POpenCall(Symbol(s))
# Make sure the macros work as often as possible:
POpenCall(p::POpenCall) = p

struct PCall <: Pattern
    val::Symbol
    code::IVector
    aux::AuxDict
    ref::Pattern
end

"""
PCall(patt::POpenCall, ref::Pattern)

Create a PCall from a POpenCall once the reference is established.
"""
function PCall(patt::POpenCall, ref::Pattern)
    PCall(patt.val, patt.code, AuxDict(), ref)
end
struct PRule <: Pattern
    val::PVector
    code::IVector
    name::Symbol
    aux::AuxDict
end

"""
    Rule(name::Symbol, patt::Pattern)

Sugar-free form of [`@rule`](@ref), creates a rule from `patt`, assigning it
the name `name`.
"""
function PRule(name::Symbol, val::Pattern)
    PRule([val], Inst(), name, AuxDict())
end

struct PGrammar <: Pattern
    val::PVector
    code::IVector
    start::Symbol
    aux::AuxDict
end

function PGrammar(start::PRule, rest::Vararg{PRule})
    start_sym = start.name
    val = [start]
    append!(val, rest)
    PGrammar(val, Inst(), start_sym, Dict())
end

"""
Global count of capture pattern tags.

Will roll over at 0xffff, so a given pattern is limited to that
many captures.
"""
capcounter::UInt16 = 0

struct PCapture <: Pattern
    val::PVector
    code::IVector
    kind::CapKind
    aux::AuxDict
    cap::Any
    tag::UInt16
end
function PCapture(a::Pattern, k::CapKind, cap::Any)
    global capcounter += 1
    PCapture([a], Inst(), k, AuxDict(), cap, capcounter)
end

"""
Global count of throw label tags.

Can't have more than 0xffff in one pattern. Sorry.
"""
global throwcounter::UInt16 = 0
struct PThrow <: Pattern
    val::Symbol
    code::IVector
    tag::UInt16
end
function PThrow(val::Symbol)
    global throwcounter += 1
    PThrow(val, Inst(), throwcounter)
end

const markmap = Dict{Symbol,UInt16}()
global markcounter::UInt16 = 0

function _getmark!(mark::Symbol)::UInt16
    get!(markmap, mark) do
        global markcounter += 1
        return markcounter
    end
end

struct PMark <: Pattern
    val::PVector
    code::IVector
    mark::Symbol
    tag::UInt16
    aux::AuxDict
end
function PMark(patt::Pattern, mark::Symbol)
    tag = _getmark!(mark)
    PMark([patt], Inst(), mark, tag, AuxDict())
end

const checktotag = IdDict{Union{Symbol,Function},UInt16}()
global checkcounter::UInt16 = 0

function _getcheck!(check::Union{Symbol,Function})::UInt16
    get!(checktotag, check) do
        if checkcounter == 0xffff
            # roll over without clobbering builtins
            global checkcounter = 0x0009
        else
            global checkcounter += 1
        end
        return checkcounter
    end
end

# Add built-ins first, tag values are hard-coded in the VM
_getcheck!(:(==))
_getcheck!(:length)
_getcheck!(:close)
_getcheck!(:always)
_getcheck!(:gt)
_getcheck!(:lt)
_getcheck!(:gte)
_getcheck!(:lte)

struct PCheck <: Pattern
    val::PVector
    code::IVector
    mark::Symbol
    tag::UInt16
    check::Union{Symbol,Function}
    check_tag::UInt16
    aux::AuxDict
end
function PCheck(patt::Pattern, mark::Symbol, check::Union{Symbol,Function})
    tag = _getmark!(mark)
    check_tag = _getcheck!(check)
    PCheck([patt], Inst(), mark, tag, check, check_tag, AuxDict())
end
PCheck(patt::Pattern, mark::Symbol) = PCheck(patt, mark, :(==))

abstract type PRunTime <:Pattern end
abstract type PTXInfo <:Pattern end

"Patterns which contain at least one enclosed or combined Pattern."
const PAuxT = Union{PAnd,PNot,PDiff,PStar,PSeq,PChoice,PCall,PRule,PGrammar,PCapture,PRunTime,PBehind,PMark,PCheck}

"Patterns which don't contain other patterns."
const PPrimitive = Union{PChar,PSet,PAny,PTrue,PFalse,PThrow,PCall,POpenCall}

function Base.getindex(patt::Pattern, i::Integer)
    if !isa(patt.val, PVector)
        error("Can't index primitive Pattern $(typeof(patt))")
    else
        return patt.val[i]
    end
end

function Base.iterate(patt::Pattern, args...)
    if !isa(patt.val, PVector)
        error("Can't iterate primitive Pattern $(typeof(patt))")
    else
        return iterate(patt.val, args...)
    end
end

function PSeq(str::AbstractString)
    val = PVector(undef, 0)
    code = Inst()
    if length(str) == 0
        return PTrue()
    elseif length(str) == 1
        return PChar(first(str))
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
optimizePChoice(a::PSet, b::PSet) = [PSet(vcat(a.val, b.val))]
optimizePChoice(a::Pattern, b::Pattern) = [a, b]
