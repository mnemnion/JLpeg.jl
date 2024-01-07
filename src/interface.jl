# Interface for JLPeg


"""
    P(p::Union{AbstractString,AbstractChar,Integer,Bool,Symbol})::Pattern

Create a [`Pattern`](@ref).

- If `p` is a `String`, this matches that string.
- If `p` is a positive `Integer`, it matches that many characters.
- If `p` is `true`, the rules succeeds, if `false`, the rule fails.
- If `p` is a `Symbol`, this represents a call to the rule with that name.
- If `p` is a negative `Integer`, matches if that many characters remain, consumes no input.
- If `p` is already a `Pattern`, it is simply returned.

# Examples

```jldoctest
julia> match(P("func"), "funci")
PegMatch(["func"])

julia> match(P(3), "three")
PegMatch(["thr"])

julia> match(P(true), "pass")
PegMatch([""])

julia> match(P(false), "fail")
PegFail("⟪f⟫ail", 1)

julia> match(P('👍'), "👍")
PegMatch(["👍"])
```
"""
function P end

const Patternable = Union{AbstractString,AbstractChar,Integer,Bool,Symbol}

"""
    Grammar(rule...)

Create a grammar from the provided rules.
"""
const Grammar = PGrammar
const Rule = PRule
const CapSym = Union{Symbol,AbstractString}
const CaptureTuple = Union{Tuple{Pattern},Tuple{Pattern,Any}}

P(s::AbstractString) = PSeq(s)
P(c::AbstractChar) = PChar(c)
P(n::Integer) = n ≥ 0 ? PAny(UInt(n)) : PAnd(PAny(UInt(-n)))
P(b::Bool) = b ? PTrue() : PFalse()
P(sym::Symbol) = POpenCall(sym)
P(p::Pattern) = p


"""
    S(s::AbstractString)

Create a Pattern matching any charcter in the string.
"""
S(s::AbstractString) = PSet(s)


"""
    R(s::AbstractString)

Create a Pattern matching every character in the range from the first to the second
character.  `s` must be two codepoints long, and the first must be lower-valued than
the second.
"""
function R(str::AbstractString)
    if length(str) > 2
        throw(PegError("Range must be two characters: $char"))
    end
    a, b = first(str), str[nextind(str, 1)]

    if a ≥ b
        throw(PegError("Range must be from low to high, got $a > $b"))
    end
    PSet((a, b))
end


"""
    R(a::AbstractChar, b::AbstractChar)

Match any character in the range `a` to `b`, inclusive.
"""
R(a::AbstractChar, b::AbstractChar) = R(a * b)

"""
    B(p::Union{Pattern,AbstractString,AbstractChar,Integer})

Match `patt` behind the current subject index. `patt` must be of fixed length.  The
most useful `B` pattern is `!B(1)`, which succeeds at the beginning of the string,
followed by `B('\n')` to match the start of a line.
"""
B(p::Union{AbstractString,AbstractChar,Integer}) = PBehind(P(p))
B(p::Pattern) = PBehind(p)
# Captures

"""
    C(patt::Pattern)

Create a capture. Matching `patt` with return the matched substring.
"""
C(patt::Pattern) = PCapture(patt, Csimple, nothing)
C(p::Patternable) = PCapture(P(p), Csimple, nothing)

"""
    C(patt::Pattern, sym::Union{Symbol,AbstractString})

Create a named capture with key :sym or "sym".
"""
function C(patt::Pattern, sym::CapSym)
    PCapture(patt, Csymbol, sym)
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
    PCapture(patt, Cgroup, sym)
end
Cg(p::Patternable, sym::Union{CapSym, Nothing}) = Cg(P(p), sym)
Cg(p::Union{Patternable, Pattern}) = Cg(p, nothing)
function Cg(p::Vector)
    if length(p) == 1
        return Cg(p[1])
    elseif length(p) == 2
        return Cg(p[1], p[2])
    else
        error("Cg(::Vector) must have a length of 1 or 2")
    end
end

"""
    Cp()

Captures the empty string in `.captures`, consuming no input.  Useful for the
side effect, of storing the corresponding offset.
"""
function Cp()
    PCapture(P(true), Cposition, nothing)
end

"""
    Cc(args...)

Constant capture. Matches the empty string and puts the values of `args` as a tuple
in that place within the `PegMatch` captures.  If `args` is a `Pair{Symbol,Any}`,
that symbol will appear as a key in the match.
"""
function Cc(args...)
    PCapture(P(true), Cconst, (args...,))
end

"""
    Cr(patt::Pattern, sym::Union{CapSym, Nothing})

Captures a `UnitRange` of matches in `patt`, optionally keyed by `sym`.
Convenient for substitutions and annotations.
"""
function Cr(patt::Pattern, sym::Union{CapSym, Nothing})
    PCapture(patt, Crange, sym)
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
    PCapture(patt, Caction, fn)
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
    PCapture(patt, Cruntime, fn)
end

"""
    T(label::Symbol)

Throw a failure labeled with `:label`.  If a rule `:label` exists, this will
be called to attempt recovery, the success or failure of the recovery rule is
then used.  Otherwise, `:label` and the position of `T(:label)` will be
attached to `PegFail` in the event that the whole match fails.
"""
function T(label::Symbol)
    PThrow(label)
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
# Conversions
Base.:|(a::PSet, b::PSet) = PSet(vcat(a.val, b.val))
Base.:|(a::PChar, b::PSet) = PSet(append!(Settable([b.val]), a.val))
Base.:|(a::PSet, b::PChar) = PSet(push!(copy(a.val), b.val))

Base.:-(a::Pattern, b::Pattern) = PDiff(a, b)
Base.:-(a::Pattern, b::Union{Integer,String}) = PDiff(a, P(b))
Base.:-(a::Union{Integer,String}, b::Pattern) = PDiff(P(a), b)
Base.:-(a::Pattern, b::Symbol)  = PDiff(a, POpenCall(b))
Base.:-(a::Symbol, b::Pattern)  = PDiff(POpenCall(a), b)


Base.:|>(a::Pattern, b::Function) = A(a, b)
Base.:>(a::Pattern, b::Function) = Anow(a, b)
Base.:%(a::Pattern, b::Symbol) = a | T(b)
# Hack that should probably be in the @grammar macro
Base.:%(a::Pattern, b::POpenCall) = a | T(b.val)

Base.:^(a::Pattern, b::Integer)  = PStar(a, b)
Base.:~(a::Pattern) = PAnd(a)
Base.:!(a::Pattern) = PNot(a)
¬(a::Pattern) = PNot(a)

<-- = ←(a::Symbol, b::Pattern) = PRule(a,b)
<-- = ←(a::Symbol, b::CaptureTuple) = PRule(a, C(b...))
<-- = ←(a::Symbol, b::Vector) = PRule(a, Cg(b))
<-- = ←(a::Symbol, b::Patternable) = PRule(a, P(b))

<-->(a::Symbol, b::Pattern) = PRule(a,C(b, a))
<-->(a::Symbol, b::CaptureTuple) = PRule(a, C(C(b...), a))
<-->(a::Symbol, b::Vector) = PRule(a, C(Cg(b),a))
<-->(a::Symbol, b::Patternable) = PRule(a, C(P(b), a))


# This little dance gets around a quirk of how negative powers
# are handled by Julia:
Base.:^(a::Tuple{Pattern, Nothing}, b::Int) = PStar(a[1], -b)
Base.inv(a::Pattern) = (a, nothing)

# Fast-forward operator
function Base.:>>(a::Pattern, b::Pattern)
    a * (!b * P(1))^0 * b
end
Base.:>>(a::Patternable, b::Pattern) = P(a) >> b
Base.:>>(a::Pattern, b::CaptureTuple) = a >> C(b...)
Base.:>>(a::Patternable, b::CaptureTuple) = P(a) >> C(b...)
Base.:>>(a::Pattern, b::Vector) = a >> Cg(b)

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

    str = replace(str, r"\\x[0-9a-fA-F]{1,2}|\\[0-7]{1,3}|\\u[0-9a-fA-F]{1,4}|\\U[0-9a-fA-F]{1,8}" =>
    s -> begin
        esc = s[2]
        base = (esc == 'u' || esc == 'U' || esc == 'x') ? 16 : 8
        Char(parse(UInt32, s[3:end], base=base))
    end)


    return str
end

"""
    P"str"

Calls P(str) on the String, in close imitation of Lua's calling convention.
"""
macro P_str(str)
    P(compile_raw_string(str))
end

"""
    R"str"

Calls R(str) on the String, in close imitation of Lua's calling convention.
"""
macro R_str(str)
    R(compile_raw_string(str))
end

"""
    S"str"

Calls S(str) on the String, in close imitation of Lua's calling convention.
"""
macro S_str(str)
    S(compile_raw_string(str))
end;