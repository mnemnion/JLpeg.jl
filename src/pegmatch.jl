
const PegKey = Union{Symbol,AbstractString,Integer}


"""
    PegCapture <: AbstractVector

The captures (including group captures) of a [`PegMatch`](@ref). Indexes
and iterates in the same fashion.
"""
struct PegCapture{E} <: AbstractVector{E}
    captures::Vector{E}
end
PegCapture() = PegCapture(Any[])

Base.append!(m::PegCapture, items...) = append!(m.captures, items...)
Base.lastindex(m::PegCapture) = lastindex(m.captures)
Base.firstindex(m::PegCapture) = firstindex(m.captures)
Base.setindex!(m::PegCapture, val, i...) = setindex!(m.captures, val, i...)
Base.size(m::PegCapture) = size(m.captures)

function Base.getindex(m::PegCapture, i::PegKey)
    if i isa Integer
        elem = m.captures[i]
        if elem isa Pair
            return elem.second
        else
            return elem
        end
    else
        for cap ∈ m.captures
            if cap isa Pair && cap.first == i
                return cap.second
            end
        end
    end
end

function Base.haskey(m::PegCapture, i::PegKey)
    if i isa Integer
        return 0 < i ≤ length(m.captures)
    end
    for cap ∈ m.captures
        if cap isa Pair && cap.first == i
            return true
        end
    end
    return false
end

"""
    PegMatch <: AbstractMatch

A type representing a successful match of a [`Pattern`](@ref) on a string.  Typically
returned from the [`match`](@ref) function.  [`PegFail`](@ref) is the atypical return
value.

A `PegMatch` is equal (`==`) to a `Vector` if the captures are equal to the `Vector`.

For details of how to make use of a `PegMatch`, see the section "Working With
Matched Data" in the documentation.

Properties:

-  `subject::AbstractString`:  Stores the string matched.
-  `full::Bool`:  Whether the match is of the entire string.
-  `captures::PegCapture`:  Contains any captures from matching `patt` to `subject`.
    This can in principle contain anything, as captures may call functions, in which
    case the return value of that function becomes the capture.  For more
    information, consult the `JLPeg` documentation, and the docstrings for `C`, `Cg`,
    `Cc`, and `A`.
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

const PegCap = Union{PegMatch,PegCapture}

function Base.:(==)(match::PegCap, other::AbstractVector)
    return match.captures == other
end

function Base.:(==)(other::AbstractVector, match::PegCap)
    return other == match.captures
end

function Base.getproperty(match::PegMatch, field::Symbol)
    if field == :offsets
        offs = []
        for cap in match
            if cap isa SubString
                push!(offs, cap.offset + 1)
            elseif cap isa PegCapture
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

function _getidx(capture::PegCapture)
    cap = capture.captures
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

Base.haskey(m::PegMatch, i::PegKey) = haskey(m.captures, i)
Base.haskey(m::PegCap, i::Any) = false

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
function Base.keys(m::PegCap)::Vector
    if m isa PegMatch
        caps = m.captures.captures
    else
        caps = m.captures
    end
    keys = []
    keyset = Set{Union{Symbol,AbstractString}}()
    for (idx, elem) in pairs(caps)
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

Base.eachindex(m::PegCap) = eachindex(m.captures)

"""
    Base.getindex(m::PegMatch, i::PegKeys)

Return the capture at index `i`.  This may be a `String` or `Symbol` type as well
as integer.  If so, the first capture by that name, if any, is returned.
"""
function Base.getindex(m::PegMatch, i::PegKey)
    if i isa Integer
        elem = m.captures[i]
        if elem isa Pair
            return elem.second
        else
            return elem
        end
    else
        for cap ∈ m.captures.captures
            if cap isa Pair && cap.first == i
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
Base.pairs(m::PegMatch) = Base.Generator(_topair, range(1,length(m.captures)), m.captures.captures)
Base.pairs(m::PegCapture) = Base.Generator(_topair, range(1,length(m.captures)), m.captures)

# Base.enumerate(m::PegCap) = Base.Generator(_frompair, range(1, length(m.captures)), m.captures)

function showcaptures(io::IO, captures::PegCapture)
    caps = captures.captures
    print(io, "[")
    for (idx, cap) in enumerate(caps)
        if cap isa Pair
            show(io, cap.first)
            print(io, " => ")
            if cap.second isa PegCapture
                showcaptures(io, cap.second)
            else
                show(io, cap.second)
            end
        elseif cap isa PegCapture
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

function Base.iterate(m::PegCap)
    iterate(m, 0)
end

function Base.iterate(m::PegCap, i::Integer)
    i += 1
    i > length(m.captures) && return nothing
    return m[i], i
end

Base.length(m::PegCap) = length(m.captures)

function Base.show(io::IO, ::MIME"text/plain", m::PegMatch)
    print(io, "PegMatch(")
    showcaptures(io, m.captures)
    print(io, ")")
end

function Base.show(io::IO, ::MIME"text/plain", m::PegCapture)
    showcaptures(io, m)
end


# AbstractTrees interface

function children(m::PegCap)
    return m.captures
end

function print_tree(m::PegMatch; kwargs...)
    tree = makefold(m)
    print_tree(tree; maxdepth=20, kwargs...)
end

# FoldingTrees

function makefold(m::PegMatch)
    root = Node("PegMatch")
    makefold(m.captures, root)
    return root
end

function makefold(m::PegCapture, parent::Node=Node("PegCapture"))
    for (key, value) in pairs(m)
        if value isa PegCapture
            if key isa Int
                capnode = Node("⊕", parent)
            else
                capnode = Node(repr(key), parent)
            end
            makefold(value, capnode)
        else
            if key isa Int
                Node(repr(value), parent)
            else
                Node(repr(key => value), parent)
            end
        end
    end
    return parent
end

function makemenu(m::PegCap)
    return TreeMenu(makefold(m))
end

request(m::PegCap) = request(makemenu(m))

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
    errpos::Int
    label::Symbol
end

# TODO some way to promote this to an Exception?

function Base.show(io::IO, ::MIME"text/plain", pfail::PegFail)
    print(io, "PegFail(")
    subject, errpos, label = pfail.subject, pfail.errpos, pfail.label
    # TODO cut the string down to a reasonable size
    @views if isempty(subject)
        print(io, "\"\"")
    elseif errpos == 1
        err = subject[1:1]
        if err == " " || err == "\t"
            reverseit = true
        elseif err == "\n"
            err = " \n"
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
        if err == " " || err == "\t"
            reverseit = true
        elseif err == "\n"
            err = " \n"
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


struct PegReport
    matched::Bool
    heatmap::Vector{Int}
    backtracks::Int
    count::Int
    capcount::Int
    subject::AbstractString
    max::Int
end

function normalize_heatmap(vector)
    # Apply logarithmic scaling and find the maximum value
    log_vector = log.(vector .+ 1)  # Adding 1 to avoid log(0)
    max_log_value = maximum(log_vector)

    # Normalize the logarithmic values to be between 0 and 15
    normalized_values = Int.(round.(15 .* (log_vector ./ max_log_value)))

    return normalized_values
end

heatmap_colors = [
    "\033[0;37m",  # White
    "\033[1;97m",   # Brighter White (bold)
    "\033[0;34m",  # Blue
    "\033[1;34m",  # Light Blue
    "\033[0;36m",  # Cyan

    "\033[1;36m",  # Light Cyan
    "\033[0;32m",  # Green
    "\033[1;32m",  # Light Green
    "\033[0;33m",  # Yellow
    "\033[1;93m",  # Brighter Yellow (approximating Orange)

    "\033[38;5;214m",  # Light Yellow
    "\033[38;5;208m",  # Orange
    "\033[0;31m",  # Red
    "\033[1;31m",  # Light Red
    "\033[0;35m",  # Magenta (approximating Orange)
    "\033[1;35m",  # Light Magenta (approximating Orange)
]

function show_heatmap_string(io::IO, s::AbstractString, heat::Vector{Int})
    last = -1
    for (idx, char) in zip(eachindex(s), s)
        temp = heat[idx]
        if temp != last
            print(io, heatmap_colors[temp])
            last = temp
        end
        print(io, char)
    end
    print(io, "\033[0m")
end

function generate_key_for_heatmap(vector)
    # Apply logarithmic scaling and find the maximum value
    log_vector = log.(vector .+ 1)  # Adding 1 to avoid log(0)
    max_log_value = maximum(log_vector)

    # Calculate the bin ranges in the logarithmic scale
    bin_ranges = [exp(range * max_log_value / 15) - 1 for range in 0:14]

    # Initialize the key with an empty vector of UnitRange
    key = UnitRange{Int}[]

    # Set the first range to start from 1
    push!(key, 1:Int(round(bin_ranges[2])))

    # Calculate the ranges for the rest of the bins
    for i in 2:length(bin_ranges)-1
        lower_bound = Int(round(bin_ranges[i])) + 1
        upper_bound = Int(round(bin_ranges[i+1]))
        push!(key, lower_bound:upper_bound)
    end

    # Ensure the last range captures the maximum value in the vector
    last_lower_bound = Int(round(bin_ranges[end])) + 1
    push!(key, last_lower_bound:maximum(vector))

    return key
end

function with_commas(num::Integer)
    str = string(num)
    return replace(str, r"(?<=[0-9])(?=(?:[0-9]{3})+(?![0-9]))" => ",")
end

function Base.show(io::IO, ::MIME"text/plain", pr::PegReport)
    println(io, "matched? ", pr.matched)
    if !pr.matched
        println("% match ", pr.max, "/", sizeof(pr.subject))
    end
    count = with_commas(pr.count)
    println(io, "count: ", count)
    println(io, "backtracks: ", pr.backtracks)
    println(io, "capture stack height: ", pr.capcount)
    println(io, "log scale: ", minimum(pr.heatmap), "-", maximum(pr.heatmap), '\n')
    heatmap_key = generate_key_for_heatmap(pr.heatmap)
    for (c, key) in zip(heatmap_colors, heatmap_key)
        print(io, c, key, "\033[0m|")
    end
    print(io, "\n\n")
    show_heatmap_string(io, pr.subject, normalize_heatmap(pr.heatmap))
end
