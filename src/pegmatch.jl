
"""
    PegMatch <: AbstractMatch

A type representing a single match to a `Pattern`.  Typically created from the `match` function.

The `match` field stores the substring of the entire matched string.
The `captures` field stores the substrings for each capture group, indexed by number.
The `offset` field stores the index into the matched string at the start of `match`.
The `offsets` field stores the locations of the start of each capture group,
with 0 denoting a group that was not captured.

This type implements the expected interface of AbstractMatch, namely `keys`, `getindex`,
`haskey`, `iterate`, and `eltype`, has the same struct layout as a RegexMatch, and follows
the same conventions, such as `nothing` for missed captures, and denoting the offset of missed
captures with `0`.  It should therefore serve as a drop-in replacement for a `RegexMatch`
in existing code.  Note that `match` on a PEG Pattern has a much more sophisticated vocabulary of
captures, but if you stick to anonymous and String-named captures, behavior should be identical.
"""
struct PegMatch{S<:AbstractString} <: AbstractMatch
    match::SubString{S}
    captures::Vector{Union{Nothing,SubString{S}}}  # TODO probably add a PegCapture type to this union
    offset::Int
    offsets::Vector{Int}
    patt::Pattern
end

PegMatch(match::SubString{S}, captures::Vector{Union{Nothing,SubString{S}}},
           offset::Union{Int, UInt}, offsets::Vector{Int}, patt::Pattern) where {S<:AbstractString} =
    PegMatch{S}(match, captures, offset, offsets, patt)


"""
    keys(m::PegMatch)::Vector

Returns keys for `match`.  These may be indexes, `String`s, or `Symbols`, depending on
the particulars of the capture definition.
"""
function Base.keys(m::PegMatch)::Vector
    # TODO handle other key types
    return collect(eachindex(m.captures))
end

"""
    show(io::IO, m::PegMatch)

Show a `PegMatch`.
"""
function Base.show(io::IO, m::PegMatch)
    print(io, "PegMatch(")
    show(io, m.match)
    capture_keys = keys(m)
    if !isempty(capture_keys)
        print(io, ", ")
        for (i, capture_name) in enumerate(capture_keys)
            print(io, capture_name, "=")
            show(io, m.captures[i])
            if i < length(m)
                print(io, ", ")
            end
        end
    end
    print(io, ")")
end

Base.getindex(m::PegMatch, i::Integer) = m.captures[i]
# TODO handle other key types

Base.iterate(m::PegMatch, args...) = iterate(m.captures, args...)
Base.length(m::PegMatch) = length(m.captures)
Base.eltype(m::PegMatch) = eltype(m.captures)