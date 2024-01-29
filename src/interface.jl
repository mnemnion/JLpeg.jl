# Interface for JLPeg


"""
    P(p::Union{AbstractString,AbstractChar,Integer,Bool,Symbol})::Pattern

Create a [`Pattern`](@ref).

- If `p` is a `String` or `Char`, this matches that string or character.
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
PegFail("fail", 1)

julia> match(P('👍'), "👍")
PegMatch(["👍"])
```
"""
function P end

const Patternable = Union{AbstractString,AbstractChar,Integer,Bool,Symbol}

"""
    Grammar(rule...)

Create a grammar from the provided [`Rules`](@ref Rule).
"""
const Grammar = PGrammar
const Rule = PRule
const CapSym = Union{Symbol,AbstractString}
const CaptureTuple = Union{Tuple{Pattern},Tuple{Pattern,Any}}

P(s::AbstractString) = length(s) == 1 ? PChar(first(s)) : PSeq(s)
P(c::AbstractChar) = PChar(c)
P(n::Integer) = n ≥ 0 ? PAny(UInt(n)) : PAnd(PAny(UInt(-n)))
P(b::Bool) = b ? PTrue() : PFalse()
P(sym::Symbol) = POpenCall(sym)
P(p::Pattern) = p


"""
    S(s::AbstractString)

Create a Pattern matching any character in the string.
"""
S(s::AbstractString) = PSet(s)


"""
    R(s::AbstractString)

Create a Pattern matching every character in the range from the first to the second
character.  `s` must be two codepoints long, and the first must be lower-valued than
the second.  Both must be valid UTF-8.
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
most useful `B` patterns are `!B(1)`, which succeeds at the beginning of the string,
and `B('\\n')|!B(1)` to match the start of a line.
"""
B(p::Union{AbstractString,AbstractChar,Integer}) = PBehind(P(p))
B(p::Pattern) = PBehind(p)
# Captures

"""
    C(patt::Pattern)

Create a capture. Matching `patt` will return the matched substring.  The sugared
form is `(patt,)`.
"""
C(patt::Pattern) = PCapture(patt, Csimple, nothing)
C(p::Patternable) = PCapture(P(p), Csimple, nothing)

"""
    C(patt::Pattern, sym::Union{Symbol,AbstractString})

Create a named capture with key :sym or "sym".  Sugared form `(patt, :sym)`.
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
key.  Sugared as `[patt]` or `[patt, :sym]`.
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
    A(patt::Pattern, λ::Function)

Acts as a grouping capture for `patt`, applying `λ` to a successful match
with the captures as arguments (not as a single Vector). If `patt` contains
no captures, the capture is the SubString.  The return value of `λ` becomes
the capture; if `nothing` is returned, the capture (and its offset) are deleted.
May be invoked as `patt <| λ`.
"""
function A(patt::Pattern, λ::Function)
    PCapture(patt, Caction, λ)
end

A(p::Patternable, λ::Function) = A(P(p), λ)

"""
    Q(patt::Pattern, λ::Function)

A `Q`uery action. `λ` will receive the `SubString` matched by `patt`, and is expected
to return a `Bool` representing the success or failure of the pattern.  This happens
during the parse, and may be used, for an example, to maintain a typedef symbol table
when parsing C, with appropriate closed-over state variables.
"""
function Q(patt::Pattern, λ::Function)
    PCapture(patt, Ctest, λ)
end

"""
    Avm!(patt::Pattern, λ::Function)

A vm Action. If `patt` succeeds, `λ` will be called on the entire `VMState`, and is
expected to return `true` or `false` to describe the success or failure of the entire
pattern.  This is an ultimate escape hatch, intended for purposes such as debugging,
or imposing user limits on stack depth. The most obvious use is early parse
termination, by setting `vm.running` to `false`.

While it may be abused to do all sorts of surprising hacks, if you think you need it
in order to parse something, you're probably wrong about that.
"""
function Avm!(patt::Pattern, λ::Function)
    PCapture(patt, Cvm, λ)
end

"""
    T(label::Symbol)

Throw a failure labeled with `:label`.  If a rule `:label` exists, this will
be called to attempt recovery, the success or failure of the recovery rule is
then used.  Otherwise, `:label` and the position of `T(:label)` will be
attached to `PegFail` in the event that the whole match fails at that point
in the string.
"""
function T(label::Symbol)
    PThrow(label)
end

"""
    M(patt::Pattern, sym::Symbol)

`M`ark the match of a pattern for later examination with [`K`](@ref).

See also [`CM`](@ref).
"""
M(patt::Pattern, sym::Symbol) = PMark(patt, sym)

"""
    CM(patt::Pattern, sym::Symbol)

Mark the match of `patt` with `sym`, while also capturing it with `sym`.

See [`M`](@ref) and [`C`](@ref) for details.
"""
CM(patt::Pattern, sym::Symbol) = C(M(patt, sym), sym)

"""
    K(patt::Pattern, sym::Symbol, [check::Union{Function,Symbol}])

Chec`K` the pattern against the previous [`Mark`](@ref JLpeg.M) with the same tag.
If `check` is not provided, it will return `true` if the SubStrings of the mark and
check are identical, otherwise it must be either a symbol representing one of the
builtins or a function with the signature `(marked::SubString,
checked::SubString)::Bool`.  The success or failure of the check is the success or
failure of the pattern.  If `patt` doesn't match, the check will not be performed.
The check will always fail if the corresponding mark is not present, except for the
builtin `:always`, which always succeeds if `patt` succeeds.

`K` may also be written in [do syntax](@extref
`Do-Block-Syntax-for-Function-Arguments`), `K(patt, :tag) do s1, s2; ... end`.

See also [`CK`](@ref)
"""
K(patt::Pattern, sym::Symbol, check::Union{Function,Symbol}) = PCheck(patt, sym, check)
K(patt::Pattern, sym::Symbol) = PCheck(patt, sym)
K(check::Function, patt::Pattern, sym::Symbol) = PCheck(patt, sym, check)

"""
    CK(patt::Pattern, sym::Symbol, check::Union{Function,Symbol})

Chec`K` the pattern against the prior [`Mark`], capturing if the check suceeds.
See [`K`](@ref) and [`C`](@ref) for details.
"""
CK(patt::Pattern, sym::Symbol, check::Union{Function,Symbol}) = C(K(patt, sym, check), sym)
CK(patt::Pattern, sym::Symbol) = CK(patt, sym, :(==))
CK(check::Function, patt::Pattern, sym::Symbol) = C(K(patt, sym, check), sym)

"""
    P"str"

Call P(str) on the String, in close imitation of Lua's calling convention.
"""
macro P_str(str)
    P(compile_raw_string(str))
end

"""
    R"str"

Create a range pattern from "str".  See [`R`](@ref).
"""
macro R_str(str)
    R(compile_raw_string(str))
end

"""
    S"str"

Create a Set pattern of the characters in "str".  See [`S`](@ref).
"""
macro S_str(str)
    S(compile_raw_string(str))
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

    str = replace(str, r"\\x[0-9a-fA-F]{1,2}|\\[0-7]{1,3}|\\u[0-9a-fA-F]{1,4}|\\U[0-9a-fA-F]{1,8}" =>
    s -> begin
        esc = s[2]
        base = (esc == 'u' || esc == 'U' || esc == 'x') ? 16 : 8
        Char(parse(UInt32, s[3:end], base=base))
    end)

    return str
end


¬(a::Pattern) = PNot(a)

<-- = ←(a::Symbol, b::Pattern) = PRule(a,b)
<-- = ←(a::Symbol, b::CaptureTuple) = PRule(a, C(b...))
<-- = ←(a::Symbol, b::Vector) = PRule(a, Cg(b))
<-- = ←(a::Symbol, b::Patternable) = PRule(a, P(b))

↔ = ⟷ = <-->(a::Symbol, b::Pattern) = PRule(a, Cg(b, a))
↔︎ = ⟷ = <-->(a::Symbol, b::CaptureTuple) = PRule(a, Cg(C(b...), a))
↔︎ = ⟷ = <-->(a::Symbol, b::Vector) = PRule(a, Cg(Cg(b),a))
↔︎ = ⟷ = <-->(a::Symbol, b::Patternable) = PRule(a, Cg(P(b), a))
<|(a::Pattern, b::Function) = A(a, b)

# Defined but not exported, for macro-availability

ε = P(true)
∅ = P(false)

"""
    JLpeg.Combinators

Contains all the combinator operators which shadow definitions in Base.  The module
redefines these symbols, providing a fallback to the Base, and are designed not to
break existing code.  The Julia compiler handles this kind of redirection well during
inference, this technique is used by several packages which specialize operators for
numerically-demanding tasks, without issue; we even transfer the Base docstrings.

Whether or not these overloads rise to the level of piracy is debatable.  That said,
we've walled them off, so that debate is not necessary.

# Use

The [`@grammar`](@ref) and [`@rule`](@ref) macros are able to use these operators, so
you may not need them.  To import them, add this to your module:

```julia
import JLpeg.Combinators: *, -, %, |, ^, ~, !, >>, inv
```
"""
module Combinators

using ..JLpeg
import ..JLpeg: CaptureTuple, Patternable, Settable,
                PSet, PSeq, POpenCall, PChoice, PDiff, PAnd, PNot, PChar, PStar

# Fallbacks to Base
# These are well-understood by the compiler and will not
# slow down your code with extra calls.  We even put the
# documentation back.

@inline
*(args::Any...) = Base.:*(args...)
@doc (@doc Base.:*) :*

@inline
|(a::Any, b::Any) = Base.:|(a, b)
@doc (@doc Base.:|) :|

@inline
-(a::Any, b::Any) = Base.:-(a, b)
-(a::Any) = Base.:-(a)
@doc (@doc Base.:-) :-

@inline
%(a::Any, b::Any) = Base.:%(a, b)
@doc (@doc Base.:%) :%

@inline
^(a::Any, b::Any) = Base.:^(a, b)
@doc (@doc Base.:^) :^

@inline
~(a::Any) = Base.:~(a)
@doc (@doc Base.:~) :~

@inline
!(a::Any) = Base.:!(a)
@doc (@doc Base.:!) :!

@inline
>>(a::Any, b::Any) = Base.:>>(a, b)
@doc (@doc Base.:>>) :>>

@inline
inv(a::Any) = Base.inv(a)
@doc (@doc Base.inv) inv

# Now a whole bunch of overloads.

*(a::Pattern, b::Pattern)            = PSeq(a, b)
*(a::Pattern, b::Pattern, c::Any...) = PSeq(a, *(b, c...))
*(a::Pattern, b::Symbol)             = PSeq(a, POpenCall(b))
*(a::Pattern, b::Symbol, c::Any...)  = PSeq(a, *(POpenCall(b), c...))
*(a::Symbol, b::Pattern)             = PSeq(POpenCall(a), b)
*(a::Symbol, b::Pattern, c::Any...)  = PSeq(POpenCall(a), *(b, c...))
*(a::Pattern, b::Union{Integer,String})            = PSeq(a, P(b))
*(a::Pattern, b::Union{Integer,String}, c::Any...) = PSeq(a, *(P(b), c...))
*(a::Union{Integer,String}, b::Pattern)            = PSeq(P(a), b)
*(a::Union{Integer,String}, b::Pattern, c::Any...) = *(PSeq(P(a), b), c...)
*(a::CaptureTuple, b::Pattern)            = C(a...) * b
*(a::CaptureTuple, b::Pattern, c::Any...) = *(C(a...), b, c...)
*(a::Pattern, b::CaptureTuple)            = a * C(b...)
*(a::Pattern, b::CaptureTuple, c::Any...) = *(a, C(b...), c...)
*(a::Union{Integer,String}, b::CaptureTuple)            = P(a) * C(b...)
*(a::Union{Integer,String}, b::CaptureTuple, c::Any...) = *(P(a), C(b...), c...)
*(a::CaptureTuple, b::Union{Integer,String})            = C(a...) * P(b)
*(a::CaptureTuple, b::Union{Integer,String}, c::Any...) = *(C(a...), P(b), c...)
*(a::CaptureTuple, b::CaptureTuple)            = C(a...) * C(b...)
*(a::CaptureTuple, b::CaptureTuple, c::Any...) = *(C(a...), C(b...), c...)
*(a::Vector, b::Pattern)            = Cg(a) * b
*(a::Vector, b::Pattern, c::Any...) = *(Cg(a), b, c...)
*(a::Pattern, b::Vector)            = a * Cg(b)
*(a::Pattern, b::Vector, c::Any...) = *(a, Cg(b), c...)

|(a::Pattern, b::Pattern) = PChoice(a, b)
|(a::Pattern, b::Symbol) = PChoice(a, POpenCall(b))
|(a::Symbol, b::Pattern) = PChoice(POpenCall(a), b)
|(a::Pattern, b::Union{Integer,String}) =  a | P(b)
|(a::Union{Integer,String}, b::Pattern) = P(a) | b
|(a::CaptureTuple, b::Pattern) = C(a...) | b
|(a::Pattern, b::CaptureTuple) = a | C(b...)
|(a::Union{Integer,String}, b::CaptureTuple) = P(a) | C(b...)
|(a::CaptureTuple, b::Union{Integer,String})  = C(a...) | P(b)
|(a::CaptureTuple, b::CaptureTuple) = C(a...) | C(b...)
|(a::Vector, b::Pattern) = Cg(a) | b
|(a::Pattern, b::Vector) = a | Cg(b)
# Conversions
|(a::PSet, b::PSet) = PSet(vcat(a.val, b.val))
|(a::PChar, b::PSet) = PSet(append!(Settable([b.val]), a.val))
|(a::PSet, b::PChar) = PSet(push!(copy(a.val), b.val))

-(a::Pattern, b::Pattern) = PDiff(a, b)
-(a::Pattern, b::Union{Integer,String}) = PDiff(a, P(b))
-(a::Union{Integer,String}, b::Pattern) = PDiff(P(a), b)
-(a::Pattern, b::Symbol) = PDiff(a, POpenCall(b))
-(a::Symbol, b::Pattern) = PDiff(POpenCall(a), b)

# Base.:(a::Pattern, b::Function) = Anow(a, b)
%(a::Pattern, b::Symbol) = a | T(b)
^(a::Pattern, b::Integer) = PStar(a, b)
function ^(a::Pattern, b::Vector{UnitRange{T}} where T <: Integer)
    isempty(b) && error("empty Vector in a^[n:m]")
    length(b) > 1 && error("a^[n:m] can take only one UnitRange, not $(length(b))")
    r = b[1]
    r.start > r.stop && error("invalid range [n:m] = [$(r.start):$(r.stop)]")
    if r.start == r.stop
        return reduce(*, Iterators.repeated(a, r.start))
    else
        return reduce(*, Iterators.repeated(a, r.start)) * a^(r.start - r.stop)
    end
end

function ^(a::Pattern, b::Vector{T} where T <: Integer)
    isempty(b) && error("empty Vector in a^[n]")
    length(b) > 1 && error("a^[n] can take only one Integer, not $(length(b))")
    return reduce(*, Iterators.repeated(a, b[1]))
end

~(a::Pattern) = PAnd(a)
!(a::Pattern) = PNot(a)

# This little dance gets around a quirk of how negative powers
# are handled by Julia:
^(a::Tuple{Pattern, Nothing}, b::Int) = PStar(a[1], -b)
inv(a::Pattern) = (a, nothing)

# Fast-forward operator
function >>(a::Pattern, b::Pattern)
    a * (!b * P(1))^0 * b
end
>>(a::Patternable, b::Pattern) = P(a) >> b
>>(a::Pattern, b::CaptureTuple) = a >> C(b...)
>>(a::CaptureTuple, b::Pattern) = C(a...) >> b
>>(a::Patternable, b::CaptureTuple) = P(a) >> C(b...)
>>(a::CaptureTuple, b::CaptureTuple) = C(a...) >> C(b...)
>>(a::CaptureTuple, b::Vector) = C(a...) >> Cg(b)
>>(a::Vector, b::CaptureTuple) = Cg(a) >> C(b...)
>>(a::Pattern, b::Vector) = a >> Cg(b)
>>(a::Vector, b::Pattern) = Cg(a) >> b

end; # module Combinators
