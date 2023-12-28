
const PegKey = Union{Symbol, AbstractString, Integer}
const PegVal = Union{SubString, Pair{PegKey, Any}, Vector{Any}}
const PegCapture = Vector{Union{SubString,Pair{PegKey, PegVal},Vector{Any}}}
const PegOffset = Vector{Union{Integer, Vector{Any}}}

"""
    PegMatch <: AbstractMatch

A type representing a single match to a `Pattern`.  Typically created from the `match`
function.

Fields:

    - `subject` stores the string matched.

    - `last` is the index of the last character matched by `patt`.  Due to the nature
       of PEGs, the match always begins at the first character, so there is little
       point in storing this information as a SubString (although the subject itself
       may be one).

    -  `captures` contains any captures from matching `patt` to `subject`.  A `Vector`
       whose elements are variously `SubStrings`, `Pairs`, or `Vector`s, recursively
       defined.  As PEGs are recursive in the general case, any number of e.g. named
       captures may recur, and Vector captures can group them to arbitrary depth, so
       this is necessary.

    - `offsets` is a `Vector` of offsets matching the start of these captures, and
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



function Base.show(io::IO, m::PegMatch)
    print(io, "PegMatch(")
    show(io, @views m.subject[1:m.last])
    print(io, ")")
end

Base.iterate(m::PegMatch, args...) = iterate(m.captures, args...)
Base.length(m::PegMatch) = length(m.captures)
Base.eltype(m::PegMatch) = eltype(m.captures)