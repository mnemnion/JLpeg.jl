# Working With Matched Data

```@meta
CurrentModule = JLpeg
DocTestSetup = quote
    using JLpeg
    import JLpeg.Combinators: *, -, %, |, ^, ~, !, >>, >:, inv
end
```

[`PegMatch`](@ref) implements the interface of AbstractMatch, and as such, it is
intentionally structured to be similar to [`RegexMatch`](@extref `Base.RegexMatch`)
from the standard library.  PEGs are a far richer and more sophisticated tool than
regexen, however: a named capture might appear many times, captures can be grouped,
those groups may have groups, with captures, having names, and so on.

Our intention is that simple matching will behave in a familiar way, with additional
methods provided for more complex scenarios.  Let's consider a simple rule with some
captures.

```jldoctest baddate
julia> @rule :baddate ← (R"09"^[4], :year) * "-" * (R"09"^[2:2],) * "-" * (R"09"^[2], :day);

julia> date = match(baddate, "2024-01-10")
PegMatch([:year => "2024", "01", :day => "10"])
```

The rule name is because this is certainly not how you should parse a date!  Note the
two equivalent ways of specifying a definite number of repetitions, `[2]` is
preferred.

Let's illustrate how to work with this.

```jldoctest baddate
julia> date == [:year => "2024", "01", :day => "10"]
true

julia> [:year => "2024", "01", :day => "10"] == date
true
```

A [`PegMatch`](@ref) is [`==`](@extref `Base.:==`) to a [`Vector`](@extref
`Base.AbstractVector`) with the same contents.  However, note that a `PegMatch` uses
default hash equality:

```jldoctest baddate
julia> hash(date) == hash([:year => "2024", "01", :day => "10"])
false
```

This is somewhat at variance with [doctrine](@extref `Base.hash`), but we feel it's
the correct choice here.

Next, let's look at iteration and indexing.

```jldoctest baddate
julia> date[:day]
"10"

julia> date[3]
"10"

julia> keys(date)
3-element Vector{Any}:
  :year
 2
  :day

julia> collect(date)
3-element Vector{Any}:
 "2024"
 "01"
 "10"

julia> collect(eachindex(date))
3-element Vector{Int64}:
 1
 2
 3

julia> collect(pairs(date))
3-element Vector{Pair{A, SubString{String}} where A}:
 :year => "2024"
     2 => "01"
  :day => "10"

julia> collect(enumerate(date))
3-element Vector{Tuple{Int64, Any}}:
 (1, "2024")
 (2, "01")
 (3, "10")
```

Default iteration will get you the matches, [`pairs`](@extref `Base.pairs`) uses the
name of the capture when there is one, if a capture has a name, that can be used to
index it, or the position in the Vector.

So far so good, what happens if a named capture matches more than once?

```jldoctest
julia> @rule :abcs ← ((R"az"^1, :abc) | "123")^1;

julia> letters = match(abcs, "abc123def123ghi123")
PegMatch([:abc => "abc", :abc => "def", :abc => "ghi"])

julia> letters[:abc]
"abc"

julia> keys(letters)
3-element Vector{Any}:
  :abc
 2
 3

julia> collect(pairs(letters))
3-element Vector{Pair{Symbol, SubString{String}}}:
 :abc => "abc"
 :abc => "def"
 :abc => "ghi"
```

As you can see, the _first_ match with that name is the indexable one, and therefore,
is the only time `:abc` appears in `keys`, while all matches have their name in
`pairs`, or, if anonymous, their index.
