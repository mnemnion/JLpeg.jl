
const PegKey = Union{Symbol,AbstractString,Integer}
const PegCap = Any
const PegCapture = Vector{PegCap}
const PegOffset = Vector{Union{Integer,Vector}}

"""
    PegMatch <: AbstractMatch

A type representing a successful match of a [`Pattern`](@ref) on a string.  Typically
returned from the [`match`](@ref) function.  [`PegFail`](@ref) is the atypical return
value.

A `PegMatch` is equal (`==`) to a `Vector` if the captures are equal to the `Vector`.

Properties:

-  `subject::AbstractString`:  Stores the string matched.
-  `full::Bool`:  Whether the match is of the entire string.
-  `captures::Vector`:  Contains any captures from matching `patt` to `subject`.
    This can in principle contain anything, as captures may call functions, in which
    case the return value of that function becomes the capture.  For more
    information, consult the `JLPeg` documentation, and the docstrings for `C`, `Cg`,
    `Cc`, `A`, and `Anow`.
-  `offsets::Vector{Int}`:  Provided for compatibility with `AbstractMatch`.
   `SubString`s contain their own offsets, so this is unnecessary for normal work,
   but we generate them when `match.offsets` is used.  It then consists of the
   indices at which the outer layer of captures may be found within the subject.
- `patt::Pattern`: The pattern matched against the subject.
"""
struct PegMatch <: AbstractMatch
    subject::AbstractString
    full::Bool
    captures::PegCapture
    patt::Pattern
    PegMatch(subject::AbstractString,
             full::Bool,
             captures::PegCapture,
             patt::Pattern) = new(subject, full, captures, patt)
end

function Base.:(==)(match::PegMatch, other::AbstractVector)
    return match.captures == other
end

function Base.:(==)(other::AbstractVector, match::PegMatch)
    return other == match.captures
end

function Base.getproperty(match::PegMatch, field::Symbol)
    if field == :offsets
        offs = []
        for cap in match
            if cap isa SubString
                push!(offs, cap.offset + 1)
            elseif cap isa Vector
                push!(offs, _getidx(cap))
            elseif cap isa Integer
                push!(offs, cap)
            else
                push!(offs, nothing)
            end
        end
        return offs
    end
    return getfield(match, field)
end

function Base.propertynames(::PegMatch)
    return (fieldnames(PegMatch)..., :offsets)
end

function _getidx(cap::Vector)
    while true
        if cap[1] isa SubString
            return cap.offset + 1
        elseif cap[1] isa Vector
            cap = cap[1]
        elseif cap isa Integer
            return cap
        else
            return nothing
        end
    end
end

"""
    Base.keys(m::PegMatch)::Vector

Return a vector of keys for all captures from the pattern.  This is as similar to
`keys(::RegexMatch)` as it can be, given the greater power of PEGs to match strings,
and therefore, the greater complexity of a PegMatch.

Captures are only included if they succeed, and only the first capture of a given
name may be indexed with the corresponding symbol or string.  Therefore, the key for
the first capture of that name will be its string or symbol, and subsequent named
captures will return the index which can retrieve that capture.

This somewhat awkward interface will provide usefully regex-like behavior for
regex-like captures.
"""
function Base.keys(m::PegMatch)::Vector
    keys = []
    keyset = Set{Union{Symbol,AbstractString}}()
    for (idx, elem) in pairs(m.captures)
        if elem isa Pair
            if elem.first in keyset
                push!(keys, idx)
            else
                push!(keys, elem.first)
                push!(keyset, elem.first)
            end
        else
            push!(keys, idx)
        end
    end
    return keys
end

Base.eachindex(m::PegMatch) = eachindex(m.captures)

"""
    Base.getindex(m::PegMatch, i::PegKeys)

Return the capture at index `i`.  This may be a `String` or `Symbol` type as well
as integer.  If so, the first capture by that name, if any, is returned.
"""
function Base.getindex(m::PegMatch, i::PegKey)
    if i isa Integer
        elem = m.captures[i]
        if elem isa Pair
            if elem.second isa Vector
                return PegMatch(m.subject, false, elem.second, m.patt)
            end
            return elem.second
        else
            return elem
        end
    else
        for cap ∈ m.captures
            if cap isa Pair && cap.first == i
                if cap.second isa Vector
                    return PegMatch(m.subject, false, cap.second, m.patt)
                end
                return cap.second
            end
        end
    end
end

# some helper functions
#
# this one is for pairs, makes a pair if there isn't one
function _topair(key, value)
    if value isa Pair
        return value
    else
        return key => value
    end
end

# this is for enumerate, makes pairs into values such that
# all indices are Integer (and therefore valid)

function _frompair(key, value)
    if value isa Pair
        return key => value.second
    else
        return key => value
    end
end

"""
    Base.pairs(m::PegMatch)

Return all captures as Pairs. If the capture has a name, that name is `pair.first`.
This is only a valid index for the first such capture, if you need those, use `enumerate`.
"""
Base.pairs(m::PegMatch) = Base.Generator(_topair, range(1,length(m.captures)), m.captures)

Base.enumerate(m::PegMatch) = Base.Generator(_frompair, range(1, length(m.captures)), m.captures)

function showcaptures(io::IO, caps::Vector)
    print(io, "[")
    for (idx, cap) in enumerate(caps)
        if cap isa Pair
            show(io, cap.first)
            print(io, " => ")
            if cap.second isa Vector
                showcaptures(io, cap.second)
            else
                show(io, cap.second)
            end
        elseif cap isa Vector
            showcaptures(io, cap)
        else
            show(io, cap)
        end
        if idx < length(caps)
            print(io, ", ")
        end
    end
    print(io, "]")
end

function Base.iterate(m::PegMatch)
    iterate(m, 0)
end

function Base.iterate(m::PegMatch, i::Integer)
    i += 1
    i > length(m.captures) && return nothing
    return m[i], i
end

Base.length(m::PegMatch) = length(m.captures)

function Base.show(io::IO, ::MIME"text/plain", m::PegMatch)
    print(io, "PegMatch(")
    showcaptures(io, m.captures)
    print(io, ")")
end

"""
    PegFail

Returned on a failure to `match(patt:Pattern, subject::AbstractString)`.


# Properties

- `subject::AbstractString`:  The string that we failed to match on.
- `errpos`: The position at which the pattern ultimately failed to match.
- `label::Symbol`: Info about the failure provided by `T(::symbol)`, defaulting to
        `:default` if the pattern fails but not at a throw point, or if no throws are
        provided.
"""
struct PegFail
    subject:: String
    errpos::UInt32
    label::Symbol
end

# TODO some way to promote this to an Exception?

function Base.show(io::IO, ::MIME"text/plain", pfail::PegFail)
    print(io, "PegFail(")
    subject, errpos, label = pfail.subject, pfail.errpos, pfail.label
    # we'll need more for long strings, but as a start
    if isempty(subject)
        print(io, "\"\"")
    elseif errpos == 1
        err = subject[1:1]
        if err == " "
            reverseit = true
        else
            reverseit = false
        end
        print(io, '"')
        printstyled(io, err, color=:red, reverse=reverseit)
        print(io, subject[nextind(subject,1):end], '"')
    else # The point-of-failure is one past the character which caused that failure,
         #  so -1 to highlight it
        epos = prevind(subject, errpos)
        e1 = prevind(subject, epos)
        e2 = nextind(subject, epos)
        sub1, err, sub2 = subject[1:e1], subject[epos:epos], subject[e2:end]
        if err == " "
            reverseit = true
        else
            reverseit = false
        end
        print(io, '"', sub1)
        printstyled(io, err, color=:red, reverse=reverseit)
        print(io, sub2, '"')
    end
    print(io, ", $(Int(errpos))")
    if label != :default
        print(io, ", :$label")
    end
    print(io, ")")
end