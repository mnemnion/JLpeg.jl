# Interface for JLPeg.

include("grammar.jl")

"""
    P(p::Union{AbstractString,AbstractChar,Integer,Bool,Symbol})::Pattern

Create a Pattern.

If `p` is a String, this matches that string.
If `p` is a positive Integer, it matches that many characters.
If `p` is `true`, the rules succeeds, if `false`, the rule fails.
If `p` is a Symbol, this represents a call to the rule with that name.
If `p` is a negative Integer, matches if that many characters remain, consumes no input.
If `p` is already a Pattern, it is simply returned.
"""
function P end

const Patternable = Union{AbstractString,AbstractChar,Integer,Bool,Symbol}
const Grammar = PGrammar
const Rule = PRule
const CapSym = Union{Symbol,AbstractString}

P(s::AbstractString) = PSeq(s)
P(c::AbstractChar) = PChar(c)
P(n::Integer) = n ≥ 0 ? PAny(UInt(n)) : PAnd(PAny(UInt(-n)))
P(b::Bool) = b ? PTrue() : PFalse()
P(sym::Symbol) = POpenCall(sym)
P(p::Pattern) = p


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

# Captures

"""
    C(patt::Pattern)

Create a capture. Matching `patt` with return the matched substring.
"""
C(patt::Pattern) = PCapture(patt, Csimple, AuxDict(:cap => nothing))
C(p::Patternable) = PCapture(P(p), Csimple, AuxDict(:cap => nothing))

"""
    C(patt::Pattern, sym::Union{Symbol,AbstractString})

Create a named capture with key :sym or "sym".
"""
function C(patt::Pattern, sym::CapSym)
    aux = AuxDict(:cap => sym)
    PCapture(patt, Csymbol, aux)
end
C(p::Patternable, sym::CapSym) = C(P(p), sym)
C(p::Vector) = Cg(p)
C(p::Vector, sym::CapSym) = Cg(p, sym)

"""
    Cg(patt::Pattern [, sym::Union{Symbol,AbstractString}])

Create a group capture, which groups all captures from P into a vector inside
the `PegMatch` object.  If `sym` is provided, the group will be found at that
key.
"""
function Cg(patt::Pattern, sym::Union{CapSym,Nothing})
    PCapture(patt, Cgroup, AuxDict(:cap => sym))
end
Cg(p::Patternable, sym::Union{CapSym, Nothing}) = Cg(P(p), sym)
Cg(p::Union{Patternable, Pattern}) = Cg(p, nothing)
function Cg(p::Vector)
    if length(p) > 1
        error("Cg with Vector must have a length of 1")
    end
    return Cg(p[1])
end

"""
    Cp()

Captures the empty string in `.captures`, consuming no input.  Useful for the
side effect, of storing the corresponding offset.
"""
function Cp()
    PCapture(P(true), Cposition, AuxDict(:cap => nothing))
end

"""
    Cr(patt::Pattern, sym::Union{CapSym, Nothing})

Captures a UnitRange of matches in `patt`, optionally keyed by `sym`.
Convenient for substitutions and annotations.
"""
function Cr(patt::Pattern, sym::Union{CapSym, Nothing})
    PCapture(patt, Crange, AuxDict(:cap => sym))
end

Cr(p::Patternable, sym::Union{CapSym, Nothing}) = Cr(P(p), sym)
Cr(p::Union{Patternable, Pattern}) = Cr(p, nothing)

"""
    A(patt::Pattern, fn::Function)

Acts as a grouping capture for `patt`, applying `fn` to a successful match
with the captures as arguments (not as a single Vector). If `patt` contains
no captures, the capture is the SubString.  The return value of `fn` becomes
the capture; if `nothing` is returned, the capture (and its offset) are deleted.
"""
function A(patt::Pattern, fn::Function)
    PCapture(patt, Caction, AuxDict(:cap => fn))
end

A(p::Patternable, fn::Function) = A(P(p), fn)

"""
    Anow(patt::Pattern, fn::Function)

Acts as a grouping capture, applying `fn` immediately upon a match succeeding
to the captures inside `patt`, or the span of `patt` if there are no captures
within it.  The return value of `fn` becomes the capture, but if `nothing` is
returned, the entire pattern fails.
"""
function Anow(patt::Pattern, fn::Function)
    PCapture(patt, Cruntime, AuxDict(:cap => fn))
end

# Operators

const CaptureTuple = Union{Tuple{Pattern},Tuple{Pattern,Any}} # More to come

Base.:*(a::Pattern, b::Pattern) = PSeq(a, b)
Base.:*(a::Pattern, b::Symbol)  = PSeq(a, POpenCall(b))
Base.:*(a::Symbol, b::Pattern)  = PSeq(POpenCall(a), b)
Base.:*(a::Pattern, b::Union{Integer,String}) = PSeq(a, P(b))
Base.:*(a::Union{Integer,String}, b::Pattern) = PSeq(P(a), b)
Base.:*(a::CaptureTuple, b::Pattern) = C(a...) * b
Base.:*(a::Pattern, b::CaptureTuple) = a * C(b...)
Base.:*(a::Union{Integer,String}, b::CaptureTuple) = P(a) * C(b...)
Base.:*(a::CaptureTuple, b::Union{Integer,String}) = C(a...) * P(b)
Base.:*(a::CaptureTuple, b::CaptureTuple) = C(a...) * C(b...)
Base.:*(a::Vector, b::Pattern) = Cg(a) * b
Base.:*(a::Pattern, b::Vector) = a * Cg(b)


Base.:|(a::Pattern, b::Pattern) = PChoice(a, b)
Base.:|(a::Pattern, b::Symbol)  = PChoice(a, POpenCall(b))
Base.:|(a::Symbol, b::Pattern)  = PChoice(POpenCall(a), b)
Base.:|(a::Pattern, b::Union{Integer,String}) = PChoice(a, P(b))
Base.:|(a::Union{Integer,String}, b::Pattern) = PChoice(P(a), b)
Base.:|(a::CaptureTuple, b::Pattern) = C(a...) | b
Base.:|(a::Pattern, b::CaptureTuple) = a | C(b...)
Base.:|(a::Union{Integer,String}, b::CaptureTuple) = P(a) | C(b...)
Base.:|(a::CaptureTuple, b::Union{Integer,String}) = C(a...) | P(b)
Base.:|(a::CaptureTuple, b::CaptureTuple) = C(a...) | C(b...)
Base.:|(a::Vector, b::Pattern) = Cg(a) | b
Base.:|(a::Pattern, b::Vector) = a | Cg(b)

Base.:-(a::Pattern, b::Pattern) = PDiff(a, b)
Base.:-(a::Pattern, b::Union{Integer,String}) = PDiff(a, P(b))
Base.:-(a::Union{Integer,String}, b::Pattern) = PDiff(P(a), b)
Base.:-(a::Pattern, b::Symbol)  = PDiff(a, POpenCall(b))
Base.:-(a::Symbol, b::Pattern)  = PDiff(POpenCall(a), b)

Base.:/(a::Pattern, b::Function) = A(a, b)
Base.://(a::Pattern, b::Function) = Anow(a, b)

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
Base.:>>(a::Pattern, b::CaptureTuple) = a >> C(b...)
Base.:>>(a::String, b::CaptureTuple) = P(a) >> C(b...)

"""
    extrasugar()

Perform type piracy of the gravest kind, allowing symbols to be
interpreted as calls to pattern rules for all combining and
modifying forms.
"""
function extrasugar()
    @eval Base.:!(a::Symbol) = PNot(POpenCall(a))
    @eval Base.:|(a::Symbol, b::Symbol) = PChoice(POpenCall(a), POpenCall(b))
    @eval Base.:|(a::String, b::Symbol) = PChoice(P(a), POpenCall(b))
    @eval Base.:|(a::Symbol, b::String) = PChoice(POpenCall(a), P(b))
    @eval Base.:*(a::Symbol, b::Symbol) = PSeq(POpenCall(a), POpenCall(b))
    @eval Base.:*(a::String, b::Symbol) = PSeq(P(a), POpenCall(b))
    @eval Base.:*(a::Symbol, b::String) = PSeq(POpenCall(a), P(b))
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
    @eval $(@__MODULE__).:|(a::String, b::Symbol) = PChoice(P(a), POpenCall(b))
    @eval $(@__MODULE__).:|(a::Symbol, b::String) = PChoice(POpenCall(a), P(b))
    @eval $(@__MODULE__).:*(a::Symbol, b::Symbol) = PSeq(POpenCall(a), POpenCall(b))
    @eval $(@__MODULE__).:*(a::String, b::Symbol) = PSeq(P(a), POpenCall(b))
    @eval $(@__MODULE__).:*(a::Symbol, b::String) = PSeq(POpenCall(a), P(b))
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