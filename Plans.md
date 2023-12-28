# Planning for jLPEG

  The intention here is to translate [LPEG](https://github.com/sqmedeiros/lpeglabel), specifically the lpeglabel fork, into idiomatic Julia, and expand it with the various bells and whistles which I've introduced, or plan to, into PEGs generally.

## Primary Sources

In addition to LPEG itself (linked above) we have:

- [LPEG Paper](https://www.inf.puc-rio.br/~roberto/docs/peg.pdf) paper describing the implementation of LPEG.


## Prior Art

- [parsimonious.jl](https://github.com/gitfoxi/Parsimonious.jl): A port of a Python library of the same name.  This takes strings and outputs ParseTrees, which will be of some use.  No idea about the internals yet.

- [PEG.jl](https://github.com/wdebeaum/PEG.jl): "Define a Parsing Expression Grammar via a macro and abuse of Julia syntax", a promising quote.  Not how I'm intending to implement this but we'll see what happens. Also no idea about internals.

- [PEGParser.jl](https://github.com/abeschneider/PEGParser.jl): uses Packrat, has a useful-looking `@grammar` macro.  Other than "it's packrat", no idea how it works yet.


## Tools

- [Match.jl](https://juliaservices.github.io/Match.jl/stable/): a macro for match/case style statements, which is an approach to the VM proper.


## Implementation

Wherein I note how I'm building the thing.

### Remaining

- [ ] Handle the other PStar cases

- [X] P(-n) for n bytes remaining

- [X] And and Not predicates

- [ ] Multibyte sets and chars

- [ ] B(patt)

- [ ] Captures

- [ ] TestPatt optimizations

- [ ] Tail call elimination

- [ ] Set span optimization


## Notes on OG Lpeg

The OG code contains the basic algorithms we need, but they're expressed in a way which makes a close translation useless. For one thing they heavily manipulate the Lua VM, and for another, it's written in C, with all that implies.

The flow in the OG begins at the bottom of `lpltree.c`, where the Lua interface is created and registered, and works its way backward from the perspective I need to take to translate it.

The interface is various ways of creating patterns, the details of which are largely useless for us, since it's all about pulling the relevant information out of Lua into C.  This involves creating two structs, `Pattern`s and `TTree`s, where a Pattern is a container for both a Tree and its bytecode.  Trees are tagged with an enum saying what variety of tree we're dealing with, a second enum indicating the variety of capture (if it is one), either zero, one, or two children, and sometimes a counter.

Trees are built into more complex patterns through combinator rules, which become parent to the existing patterns, and as grammars, where they eventually get checked and resolved in various ways.

`lp_match` causes the tree to be compiled, and these instructions are passed to the VM for match proper, which runs the bytecode.

## Strategy

The distinction between Pattern and TTree isn't something we really need, I don't
think.  The distinction is basically between the encapsulation of Lua-side userdata,
code, and a TTree, where the latter is used to do manipulation of the pattern stuff
without affecting the Lua VM.  We can just have Patterns as containers that also do
the Tree-like things.

## Implementation Notes and Details

Current work is on captures, and the early foray has convinced me that the Julian
solution will differ considerably both from the Lua (in Julia, not _everything_ is a
table/Dict) and from how the regex package implements AbstractMatch. So this is notes on how I want captures to work, and we'll work back from there.

Syntax: mostly, I like the "your tuple is a capture" syntax, and will add "your vector is a group of captures" to that.

```julia
( :a  ←  P"123" * ("abc",) | [("qwerty", :qwerty) | :b ],
  :b  ←  (("qwertz", :qwertz) | ("azerty", :azerty))^1 )
```

I don't hate it.  Syntax-wise, I'm a bit torn between adding a macro for the Snobol
operator overload style to pour a bit more sugar in the teacup, and just getting on
with porting my existing PEG format to JLPeg, that being the QED and all.

Anyway. The data structure, and therefore, what we even capture to begin with.

```julia
const PegKey = Union{Symbol, AbstractString, Integer}
const PegCapture = Vector{Union{SubString,Tuple{PegKey,PegCapture},PegCapture}}
const PegOffset = Vector{Union{Integer, PegOffset}}
```

is this terrible? it's a plist pretty much. kinda terrible? but iterable, allows duplicate named matches, and it reflects the actual structure of what a recursive pattern matcher produces, which is a big plus.

```julia
struct PegMatch <: AbstractMatch
   pattern::Pattern
   captures::PegCapture
   offsets::PegOffset
end>
```

Anyway enough about that for a sec, the important part is keeping track of all this capture business, which we're going to do by having the pattern hoist a Dict keyed by literal Capture instructions so we can successfully do all the capture stuff to the captures. They're tiny, it'll be fine, even the one with the offset fits in a machine word, which is where the code was going to put it, on god, no cap, fr fr.
