

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

const CaptureTuple = Union{Tuple{Pattern},Tuple{Pattern,Any}} # More to come

"""
    Cg(patt::Pattern [, sym::Union{Symbol,AbstractString}])

Create a group capture, which groups all captures from P into a vector inside
the `PegMatch` object.  If `sym` is provided, the group will be found at that
key.
"""
function Cg(patt::Pattern, sym::Union{CapSym, Nothing})
    PCapture(patt, Cgroup, AuxDict(:cap => sym))
end
Cg(p::Patternable, sym::Union{CapSym, Nothing}) = Cg(P(p), sym)
Cg(p::Union{Patternable, Pattern}) = Cg(p, nothing)

"""
    Cp()

Captures the empty string in `.captures`, consuming no input.  Useful for the
side effect, of storing the corresponding offset.
"""
function Cp() #TODO implement this as a FullCapture with offset 0.
    PCapture(P(true), Cposition, AuxDict(:cap => nothing))
end

# Operators

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

"""
    @grammar(name, rules)

Syntax sugar for defining a set of rules as a single grammar. Expects a block `rules`,
each of which is a rule-pair as can be created with `<=` or `←`, with some differences:

- Rule and call symbols don't need the `:`, although this is valid.
- Strings, number, and booleans are converted to patterns, even at the head of a rule.
"""
# TODO working it out in scratch