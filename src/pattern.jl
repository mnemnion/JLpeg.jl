# Patterns for JLpeg parser engine

"""
Container for various patterns and grammars.
Always has `val`, which may be primitive or a Vector{Pattern},
and `code`, an IVector. Some patterns have a field
unique to that pattern type.
"""
abstract type Pattern end

"A bytecode instruction"
abstract type Instruction end

const IVector = Vector{Instruction}



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

struct PAny <: Pattern
    val::UInt32
    code::IVector
    PAny(val::UInt) = new(val, Inst())

end

struct PAnd <: Pattern
    val::Vector{Pattern}
    code::IVector
    PAnd(val::Pattern) = new([val], Inst())
end

struct PNot <: Pattern
    val::Vector{Pattern}
    code::IVector
    PNot(val::Pattern) = new([val], Inst())
end

struct PDiff <: Pattern
    val::Vector{Pattern}
    code::IVector
    PDiff(a::Pattern, b::Pattern) = new([a, b], Inst())
end

"Includes n, dictating the sort of repetition"
struct PStar <: Pattern
    val::Vector{Pattern}
    code::IVector
    n::Int
    PStar(patt::Pattern, n::Int) = new([patt], Inst(), n)
end

struct PSeq <: Pattern
    val::Vector{Pattern}
    code::IVector
end

struct PChoice <: Pattern
    val::Vector{Pattern}
    code::IVector
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
    meta::Dict{AbstractString, Any}
    POpenCall(sym::Symbol) = new(sym, Inst(), Dict())
end

POpenCall(s::AbstractString) = POpenCall(Symbol(s))

# TODO I'm not in fact using this and should do so or get rid of it.
struct PCall <: Pattern
    val::Symbol
    code::IVector
    meta::Dict{AbstractString, Any}
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
    meta::Dict{Symbol, Any}
    PRule(name::Symbol, val::Pattern) = new([val], Inst(), name, Dict())
end

struct PGrammar <: Pattern
    val::Vector{PRule}
    code::IVector
    start::Symbol
    meta::Dict{Symbol, Any}
    function PGrammar(start::PRule, rest::Vararg{PRule})
        start_sym = start.name
        val = [start]
        append!(val, rest)
        new(val, Inst(), start_sym, Dict())
    end
end

# TODO the rest of these need to be concrete:

abstract type PRunTime <:Pattern end
abstract type PBehind <:Pattern end
abstract type PCapture <:Pattern end
abstract type PTXInfo <:Pattern end
abstract type PThrow <:Pattern end

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



"""
    P(p::Union{AbstractString,AbstractChar,Int,Bool,Symbol})::Pattern

Create a Pattern.

If `p` is a String, this matches that string.
If `p` is a positive Int, it matches that many characters.
If `p` is `true`, the rules succeeds, if `false`, the rule fails.
If `p` is a Symbol, this represents a call to the rule with that name.
If `p` is a negative Int, matches if that many characters remain, consumes no input.
"""
function P(p::Any)::Pattern end

P(s::AbstractString) = PSeq(s)
P(c::AbstractChar) = PChar(c)
P(n::Integer) = n ≥ 0 ? PAny(UInt(n)) : PAnd(PAny(UInt(-n)))
P(b::Bool) = b ? PTrue() : PFalse()
P(sym::Symbol) = POpenCall(sym)
const Grammar = PGrammar
const Rule = PRule

"""
    S(s::AbstractString)

Create a pattern matching any charcter in the string.
"""
S(s::AbstractString) = PSet(s)


"""
    R(s::AbstractString)

Create a pattern ranging from the first to the second character.
`s` must be two codepoints long, and the first must be lower-valued
than the second.
"""
R(s::AbstractString) = PRange(s)
R(a::AbstractChar, b::AbstractChar) = PRange(a, b)

# Operators

Base.:*(a::Pattern, b::Pattern) = PSeq(a, b)
Base.:*(a::Pattern, b::Symbol)  = PSeq(a, POpenCall(b))
Base.:*(a::Symbol, b::Pattern)  = PSeq(POpenCall(a), b)
Base.:*(a::Pattern, b::Union{Integer,String}) = PSeq(a, P(b))
Base.:*(a::Union{Integer,String}, b::Pattern) = PSeq(P(a), b)

Base.:|(a::Pattern, b::Pattern) = PChoice(a, b)
Base.:|(a::Pattern, b::Symbol)  = PChoice(a, POpenCall(b))
Base.:|(a::Symbol, b::Pattern)  = PChoice(POpenCall(a), b)
Base.:|(a::Pattern, b::Union{Integer,String}) = PChoice(a, P(b))
Base.:|(a::Union{Integer,String}, b::Pattern) = PChoice(P(a), b)

Base.:-(a::Pattern, b::Pattern) = PDiff(a, b)
Base.:-(a::Pattern, b::Union{Integer,String}) = PDiff(a, P(b))
Base.:-(a::Union{Integer,String}, b::Pattern) = PDiff(P(a), b)
Base.:-(a::Pattern, b::Symbol)  = PDiff(a, POpenCall(b))
Base.:-(a::Symbol, b::Pattern)  = PDiff(POpenCall(a), b)

Base.:^(a::Pattern, b::Integer)  = PStar(a, b)
Base.:~(a::Pattern) = PAnd(a)
Base.:!(a::Pattern) = PNot(a)
¬(a::Pattern) = PNot(a)

Base.:<=(a::Symbol, b::Pattern) = PRule(a, b)
←(a::Symbol, b::Pattern) = PRule(a,b)

# This little dance gets around a quirk of how negative powers
# are handled by Julia:
Base.:^(a::Tuple{Pattern, Nothing}, b::Int) = PStar(a[1], -b)
Base.inv(a::Pattern) = (a, nothing)

# Fast-forward operator
function Base.:>>(a::Pattern, b::Pattern)
    a * (!b * P(1))^0 * b
end
Base.:>>(a::String, b::Pattern) = P(a) >> b

"""
    extrasugar()

Perform type piracy of the gravest kind, allowing symbols to be
interpreted as calls to pattern rules for all combining and
modifying forms.
"""
function extrasugar()
    @eval Base.:!(a::Symbol) = PNot(POpenCall(a))
    @eval Base.:|(a::Symbol, b::Symbol) = PChoice(POpenCall(a), POpenCall(b))
    @eval Base.:*(a::Symbol, b::Symbol) = PSeq(POpenCall(a), POpenCall(b))
    @eval Base.:~(a::Symbol) = PAnd(POpenCall(a))
    @eval Base.:^(a::Symbol, b::Int)  = PStar(POpenCall(a), b)
end

"""
    modulesugar()

Introduces operator overloads to `:symbol`s limited to the enclosing module scope.

"""
function modulesugar()
    @eval $(@__MODULE__).:!(a::Symbol) = PNot(POpenCall(a))
    @eval $(@__MODULE__).:|(a::Symbol, b::Symbol) = PChoice(POpenCall(a), POpenCall(b))
    @eval $(@__MODULE__).:*(a::Symbol, b::Symbol) = PSeq(POpenCall(a), POpenCall(b))
    @eval $(@__MODULE__).:~(a::Symbol) = PAnd(POpenCall(a))
    @eval $(@__MODULE__).:^(a::Symbol, b::Int)  = PStar(POpenCall(a), b)
end

"""Helper for macros"""
function compile_raw_string(str::String)::String
    # Mapping of C escape sequences to their Julia equivalents
    c_escapes = Dict(
        "\\a" => "\a", "\\b" => "\b", "\\f" => "\f",
        "\\n" => "\n", "\\r" => "\r", "\\t" => "\t",
        "\\v" => "\v", "\\\\" => "\\", "\\'" => "'",
        "\\\"" => "\"", "\\?" => "?", "\\0" => "\0"
    )

    # Replace each C escape sequence with its Julia equivalent

    str = replace(str, c_escapes...)

    str = replace(str, r"\\x[0-9a-fA-F]{1,2}|\\[0-7]{1,3}|\\u[0-9a-fA-F]{1:6}" =>
    s -> begin
        esc = s[2]
        base = (esc == 'u' || esc == 'x') ? 16 : 8
        Char(parse(UInt32, s[3:end], base=base))
    end)

    return str
end

"""
    P"str"

Calls P(str) on the string, in close imitation of Lua's calling convention.
"""
macro P_str(str)
    P(compile_raw_string(str))
end

"""
    R"str"

Calls R(str) on the string, in close imitation of Lua's calling convention.
"""
macro R_str(str)
    R(compile_raw_string(str))
end

"""
    S"str"

Calls S(str) on the string, in close imitation of Lua's calling convention.
"""
macro S_str(str)
    S(compile_raw_string(str))
end

"""
    @grammar(name, rules)

Syntax sugar for defining a set of rules as a single grammar. Expects a block `rules`,
each of which is a rule-pair as can be created with `<=` or `←`, with some differences:

- Rule and call symbols don't need the `:`, although this is valid.
- Strings, number, and booleans are converted to patterns, even at the head of a rule.
"""
macro grammar(name, rules)

end