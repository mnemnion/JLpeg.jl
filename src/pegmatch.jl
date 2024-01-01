
const PegKey = Union{Symbol, AbstractString, Integer}
const PegCap = Any
const PegCapture = Vector{PegCap}
const PegOffset = Vector{Union{Integer, Vector}}

"""
    PegMatch <: AbstractMatch

A type representing a single match to a `Pattern`.  Typically created from the `match`
function.

Fields:

    -  `subject` stores the string matched.

    -  `last` is the index of the last character matched by `patt`.  Due to the nature
        of PEGs, the match always begins at the first character, so there is little
        point in storing this information as a SubString (although the subject itself
        may be one).

    -  `captures` contains any captures from matching `patt` to `subject`.  This
        Vector can in principle contain anything, as captures may call functions,
        in which case the return value of that function becomes the capture.  For
        more information, consult the `JLPeg` documentation, and the docstrings
        for `C`, `Cg`, #Todo others

    -  `offsets` is a `Vector` of offsets matching the start of these captures, and
       `Vector`s of that vector, such that the same pattern of iterative search will
       produce the offset and its substring.

    - `patt` is the `Pattern` matched against the subject.
"""

struct PegMatch <: AbstractMatch
    subject::AbstractString
    last::Integer
    captures::PegCapture
    offsets::PegOffset
    patt::Pattern
    PegMatch(subject::AbstractString,
             last::Integer,
             captures::PegCapture,
             offsets::PegOffset,
             patt::Pattern) = new(subject, last, captures, offsets, patt)
end


function Base.keys(m::PegMatch)::Vector
    # TODO handle other key types
    return collect(eachindex(m.captures))
end

Base.eachindex(m::PegMatch) = eachindex(m.captures)


"""
    Base.getindex(m::PegMatch, i::PegKeys)

Return the capture at index `i`.  This may be a `String` or `Symbol` type as well
as integer.  If so, the first capture by that name, if any, is returned.
"""
function Base.getindex(m::PegMatch, i::PegKey)
    if i isa Integer
        return m.captures[i]
    else
        for cap in m.captures
            if cap isa Pair && cap.first == i
                return cap.second
            end
        end
    end
end


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

function Base.show(io::IO, m::PegMatch)
    print(io, "PegMatch(")
    showcaptures(io, m.captures)
    print(io, ")")
end

Base.iterate(m::PegMatch, args...) = iterate(m.captures, args...)
Base.length(m::PegMatch) = length(m.captures)
Base.eltype(::PegMatch) = Pair{PegKey, PegVal}

"""
    struct PegFail
        subject:: String
        errpos::UInt32
        label::Symbol
    end

Returned on a failure to `match(patt:Pattern, subject::AbstractString)`. `errpos` is the
position at which the pattern ultimately failed to match, `label` is info about the failure
provided by `T(::symbol)`, defaulting to `:default` if the pattern fails but not at a throw
point, or if no throws are provided.
"""
struct PegFail
    subject:: String
    errpos::UInt32
    label::Symbol
end

# TODO some way to promote this to an Exception?

function Base.show(io::IO, pfail::PegFail)
    print(io, "PegFail(")
    subject, errpos, label = pfail.subject, pfail.errpos, pfail.label
    # we'll need more for long strings, but as a start
    if errpos > sizeof(subject)
        print(io, '"' * subject * "⟪⟫" * '"')
    else
        e1 = prevind(subject, errpos)
        e2 = nextind(subject, errpos)
        sub1, err, sub2 = subject[1:e1], subject[errpos:errpos], subject[e2:end]
        print(io, '"' * sub1 * '⟪' * err * '⟫' * sub2 * '"')
    end
    print(io, ", $(Int(errpos))")
    if label != :default
        print(io, ", $label")
    end
    print(io, ")")
end