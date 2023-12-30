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

- [#] Multibyte sets and chars

  - [X] Implement multibyte sets
  - [ ] Fix bug with emoji 🫠

- [ ] B(patt)

- [X] Captures

- [ ] Mark / Check
- [ ] detect "loop may accept empty string" such as `a = (!S"'")^0`

- [ ] TestPatt optimizations

- [ ] Tail call elimination

- [ ] Set span optimization

Somewhere to put random implementation notes: at some point I intend to add
serializing of bytecode, just a (versioned!!) minimal string form which allows a
zero-logic construction of a Grammar.  That would be a good time to remov NoOps,
which get left around by various optimizations. There's a reason every instruction
set in existence has them, but it's aesthetically unsatisfying to leave them in.

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

## Back references: Mark and Check

This is the separate mechanism I want to add based on the code I hacked together for
bridge.  A Mark wraps the sequence in Choice/Commit, but this one is MarkCommit: we
pop the choice frame and combine it with the MarkCommit, and this goes on its own stack.
There's also a CheckCommit, every Mark/Check pair has its own name in the Pattern but we
convert these to a counter since the VM doesn't need them. The Check looks backward for
the first Mark, if it's within the first, say, eight frames (usually it's on top) we
remove it, if not, we slip in a hold frame, and one of the canonical mark/check actions
is performed, namely: are they the same substring, is one longer than the other, or
equal length without regard to contents.  There's also a runtime mark for taking an
Action on both captures, the result of which succeeds or fails the pattern.

I _could_ implement this as captures but there are good reasons not to, this decomplects
what is after all a pattern-matching operation from capturing, Mark/Check can cross rule
and capture boundaries freely, which is the important thing from my perspective.

## Dialects

Making progress on captures, want to get these thoughts out of my head to return to later.

JLPeg will offer dialects for strings which compile to patterns, at least these:

- `re`:  Implementation of Lpeg `re` module

- `peg`: Implementation of Sam Atman style PEG grammars

- `dialect`: For specifying dialects. I simply suspect this will be useful.

- `regex`: I don't see any reason we can't interpret PCRE regex format on the VM.

- `canonical`  A string form any dialect may be transformed to, and any
               Pattern printed as. Needs to be feature complete, rather than
               pleasant to look at or write, but must be legible. This is
               necessary for, among other things:

### Grammar Transforming Dialects

I dunno how this works actually, but the example I have in mind transforms grammars
into highlighters, so it's a DSL to specify StyledStrings annotations to perform on a
recognized grammar.

### More Dialect Notes

So there's [ReplMaker](https://juliahub.com/ui/Packages/General/ReplMaker) for
constructing custom repl modes, which we can use for dialect and grammar builders

### Expr parser

This is something I wanted to do in Lua, Julia having a canonical Expr format makes
the idea bearable, but this is a separate VM, or program really, which reparses
changes to a parser Expr to see when they remain valid to the specified Grammar.
Crucial for getting rid of Treesitter.
