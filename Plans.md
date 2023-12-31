# Planning for jLPEG

   The intention here is to translate [LPEG](https://github.com/sqmedeiros/lpeglabel),
specifically the lpeglabel fork, into idiomatic Julia, and expand it with the
various bells and whistles which I've introduced, or plan to, into PEGs generally.

## Implementation

Wherein I note how I'm building the thing.

### Remaining

The hitlist:

- [#]  Captures
  - [X]  Cc
  - [?]  Refactor Cg to use tuples, not Vectors.
  - [ ]  `:key => patt` symbol-captures pattern
  - [ ]  Capture questions
    - [X]  Shorthand: `:a <--> patt` for `:a <-- [patt, :a]`.  I like this a lot.
           Ends up being equivalent to suppressed and included rules in the PEG
           format.  Needs to be a group capture though, to get the recursive
           shape correct.
    - [?]  Expression caps can just be |> `toexpr`, right? Although what's the
           symbol.  I think having a gazillion capture types is too LPeg for JLpeg...
    - [Y]  Captures in fact should just be Csimple and Csymbol, the rest are
           conceptually Actions according to the distinction I've drawn.
    - [ ]  Maybe `patt <| λ` is best, I don't want the difference between them to be a
           matter of dropping a single character.  The mnemonic here is that the return
           value of `λ` can fail the match, so it's "piped back in", sort of.
    - [ ]  The more I think about it, a lot of what I thought should go into captures
           is already provided by SubStrings.  I'm increasingly convinced that our
           matches don't need the offsets at all, those are literally present in the
           SubString, I can always get rid of that code if I decide I don't want it.
           It would be nice to turn it into a property instead of a field though.
    - [ ]  I do have a use for range captures, even though that's basically what a
           SubString is.  They can be token captures, we don't want to see them when
           walking a parse tree or iterating, but it's important to know where they
           are and categorize them for tasks like syntax highlighting and formatting.
           The LPeg tree format I was using was a right pain for that.
  - [ ]  `Anow`, `patt > λ`
    - [ ]  Refactor `aftermatch` to get a function which can enact captures across
           part of the  stack
    - [ ]  The rest should be fairly simple
  - [ ]  "deferred action" form `patt |> :func` || `patt > :func`.  This one will be rather
         complex to get right, but we get one critical and one nice thing out of it: assigning
         several actions to a single grammar, and compile-time compiling grammars then load-time
         providing the Actions.
- [ ]  Printing stuff
  - [ ]  `clipstringat(str, i, len=20)` returns a NamedTuple tuple with the char at i,
         up to two substrings surrounding it, and booleans telling whether or not we
         truncated the string in the front or the back.  There are always substrings,
         but if `i` is the first or last character, one of them will be empty.
  - [ ]  Refactor color printing to use `printstyled`.
- [x]  Macros
  - [X]  Get rid of the clunky tuple forms of `@grammar` and `@rule` by getting the
         escaping rules for the macro correct.
- [ ]  Documenter stuff
  - [X]  Order the pages correctly
  - [ ]  Docstrings for private module names in Internals
  - [ ]  Add a "comparisons.md" page for in-depth comparison of PEGs to other systems.
  - [ ]  Fix `PegFail` so the fail is visible (again).
- [ ]  [Mark / Check](#mark-and-check-back-references)
- [ ]  Detect "loop may accept empty string" such as `a = (!S"'")^0`
- [ ]  Optimizations from The Book (paper and/or lpeg C code):
  - [ ]  Add `getfirst`: used to optimize Seq
    - [ ]  `needfollow`: used in `getfirst`
  - [ ]  TestPatt / headfail optimizations
  - [X]  Tail-call elimination
  - [ ]  Intermediate jump elimination
  - [X]  Set ISpan optimization
    - [X]  Even better: Since Set has `:l`, we just make it `LeadSetInst(vec, 0)`.
  - [?]  fixed-length detection
  - [ ]  full-capture optimization (bytecode)
  - [ ]  disjoint-PChoice optimization
  - [ ]  Capture-closing optimization (vm)
- [X]  All `CaptureInst`s same struct w. distinct Pattern subtype
- [ ]  Proposed optimizations not found in LPeg
  - [ ]  Immutable vector Instructions using the `getindex` from BitPermutations.jl
  - [ ]  MultiSetTest [conversion](#multiset-test-conversion)
  - [X]  Parameteric `IChar` specialized to `Char` (which is what I care about)
  - [ ]  Fail optimization: only update the register once when returning from calls.
         this one should be deferred until we have real profiling on the hot loop.
         Technically this _is_ found in LPeg...
  - [ ]  Inlining: short rules with no captures, "short" is like, 5 instructions? 10?
  - [ ]  CaptureCommitInst: it's a commit which create a full capture from its paired Choice.
         Very nice for capture-heavy workflows, like parsing full grammars (where we very often
         want a full rule).
  - [ ]  `(a / b / c)* -> (a* / b* / c*)*` should give better performance on common patterns
         like whitespace, where the {\t\n } is very frequent and the comment part is not, lets
         us use ISpan for a leading set.  Note: there's some bytecode trickery needed here to
         make sure it doesn't infinite loop.
  - [ ]  Follow sets for TestSet and Span.  These have the additional advantage that they can
         jump immediately if they fail (for TestSet) or recurse to the start instruction after
         a match (for a SpanSet).
  - [X]  Headfailing TestSet groups: If we have a group of multibyte sets we want to test,
         we can compress the head (lead) bytes into a single ASCII-style lead test set, by
         masking the high bit off.  This lets us single-test fail out of some very large
         ranges indeed, such as Chinese.
  - [ ]  Left recursion.  Strangely, [this paper](https://arxiv.org/pdf/1207.0443.pdf)
         describes a method for implementing left recursion, by Roberto's team (!),
         describing how to do it for the LPeg VM (!!), but LPeg has not been extended to allow
         this.  I'd like to ask the Lua list why before trying it myself...
    - [This repo](https://github.com/sacek/LPeg) is an LPeg fork with left recursion
       added, I can diff this with its source (LPeg 1.0.0) and learn things.
  - [ ]  Choice-sequence prefix matching.  This is something regex automatons do
         automatically, which would be nice for us to have.  Basically, it work with
         head sequences (including sets, and maybe predicates, but not repetition)
         such that choices which share a prefix are collapsed into a single choice
         sequence, this limits backtracking.  It ties into the next one:
  - [ ]  Choice shadow detection.  This is just a nice thing I'd like to do for my
         users.  In many circumstances the compiler could detect when an earlier ordered choice means that a later choice can't be matched, this is always a bug and should be brought to the user's attention.  The easy cases all involve fixed-length later choices, but
         a certain amount of detection can be performed with predicates and repetition in the mi as well.  For literal sequences it's as simple as applying the earlier rule to the string form of the later rule and seeing if there's a match.
- [ ]  Fragment parser (see [section](#fragment-parser))
- [ ]  String [generators](#string-generation)
- [ ]  Add beginning index or `UnitRange` as optional third argument for `match`.
       The way the VM is structured we don't even need to make a SubString, we cache
       the last index string set the subject pointer to 1, so both of those are mutable
       things.  Note that the VM works correctly on SubStrings already, although we should
       special-case them because this implies that all the indexing is going through an offset
       we only need to calculate once.
- [ ]  Suspendable VM [discussion](#suspend-the-vm)
- [ ]  Pure [Code Bumming](#optimal-vm) (need to be able to check if it even matters)
  - [ ]  StaticArray for prepared programs.
    - [ ]  This might have to be custom because of indexing, it's likely to be a dip
           down into C.  Lpeg labels are jiggered based on some instructions (entirely charsets I believe) being extra-length, when I print pcode there are gaps in the numbers
  - [ ]  Optimal VM dispatch, the current system is aggressively not-type-stable.
           accordingly.  It's a major operation from my perspective, we'd need to obtain a pointer to the Vector somehow and correct all the instruction labels, but probably
           worth the most speedup after a type-stable dispatch.
  - [ ]  SetInst may not need a `:l`.
  - [X]  Struct packing
  - [ ]  I'm surely losing cycles converting non-ASCII Chars into UInt32, should add
         Char1Inst..Char4Inst.  With NotChar and TestChar, that's 12 instructions, which is
         fine I think.
- [ ] `AbstractPattern` methods
  - [ ] count(patt::Pattern, s::AbstractString, overlap::Boolean=false)
  - [ ] findall: I think this just returns the .offsets vector of the match
- [ ]  [PDiff](#pdiff)
- [ ] - [X] Done:
  - [X] Handle the other PStar cases
  - [X] `P(-n)` for n bytes remaining
  - [X] And and Not predicates
  - [X] Check lpeg code for #P(whatever) inside a call, does it use BackCommit?
  - [X] Interface stuff
    - [X] The grammar munges every possible string into P"string", symbols too.
          We need to fix that, possibly by rewriting as a prewalk, not postwalk?
    - [X] `←` needs to take the capture forms on the right hand side
  - [X] `B(patt)` (prerequisite: determining fixed-length patterns)
  - [X] `T(:label)` somewhat hefty VM refactor here
    - [X] `ThrowInst`
    - [X] `ThrowRecInst`
  - [X] `PegFail` object with test conversion
    - [X] default label is `:default`
  - [X] Serializing and loading grammars
    - [X] Serializer for PRule, PGrammar
    - [X]  `@rule!` and `@grammar!` macros
      - [X]  `@rule!`
      - [X]  `@grammar!`
  - [X]  [Compiler Rewrite](#compiler-rewrite)
    - [X]  Hoisting
  - [X]  Add a bunch more MultiSet tests (emoji in particular)
    - [X]  Add **moar** Set tests!
  - [X]  Refactor `.final` field into `.l` labeled jump
  - [X]  Add `IByteInst`, refactor `IMultiSet` for new bytecode
  - [X]  It turns out that a simple complement of an ASCII PSet isn't valid, because
         that doesn't match a Unicode character, and it would need to. "not this set"
         and "not this character" are very common in the ASCII range in practical
         patterns, to the point that it might be worth adding extra opcodes for them.
         These will simply return `true` if the pattern doesn't match and `false` if it
         does, so `!S"123"` will correctly match multibyte characters as well.
         These are **predicates** so they do not advance the subject pointer.
    - [X]  NotChar
    - [X]  NotSet
  - [X]  Handle PChoice consolidation
    - [X]  Make PRange and PSet both PSet
    - [X]  Compile time doesn't matter nearly so much as VM time, but it's still nice
           to avoid extra allocation, especially for something like ".val" fields which
           stick around forever.  So we could make PSet store a
           `Vector{Option{Char,Pair{Char,Char}}}`, get rid of separate PRange which
           only complicates the code, and JIT the bitvector in prefix!, apply the byte,
           and call it golden.
    - [X]  Specialize `|` for combinations of `PSet` and `PChar` to automagically
           consolidate into one `PSet`.
    - [X]  Add the headfail instruction, that should go fairly smoothly I think, it's just:
      - [X]  Count the keys in the prefix map, if there are more than say five:
      - [X]  Calculate a LeadMultiVec, which fail-jumps without unwinding the stack
  - [X]  Multibyte sets and chars
    - [X]  Implement multibyte sets
    - [X]  Fix bug with emoji 🫠.  Appears to work?  This problem may reappear though.
    - [X]  Multiset Rewrite
    - [X]  Clean Up

### PDiff

Make a special `PSetDiff` which has a second Settable value, such that those
characters and ranges are excluded from the construct.  This is just to defer
the expensive operation until we compile, it should return itself as a PSet.

Which I don't have to do myself!  How luxurious.  There's a
[UnitRangesSortedSet](https://juliahub.com/ui/Packages/General/UnitRangesSortedSets)
which will do the needful.

### Capture closing

I knew there was a reason I might want to cache the capture stack...

This optimization changes `CapEntry` back to holding the values of the instruction
separately, and adds a capture register holding the values of the top frame.  When we
get a `CloseCaptureInst`, we mutate the register to hold a FullCaptureEntry with the
calculated offset. And this simplifies the `aftermatch` code somewhat because there
will only be `FullCapture`s, if we find a not-`FullCapture` there's a mistake somewhere.

This would also mean not having to synthesize a `FullCaptureInst` in the following,
although I doubt very much that this would generate different machine code, but
maybe: the compiler pays a lot of attention to method dispatch.

### CaptureCommitInst

We can use this whenever a `PCapture` is enclosing a `PChoice`, but it's a slightly
tricky optimization to get right.  What we do is go through the copied bytecode and
replace every Commit (not Partial Commit) _which belongs to the `PChoice`_ with a
`CaptureCommitInst`.  This is best accomplished by re-synthesizing the choice code
from parts.

`onCaptureCommit` looks like this:

```julia
@inline
function onCaptureCommit(inst::LabelInst, vm::VMState)
    _, s, _ = popframe!(vm)
    off = vm.s - s
    fullinst = FullCaptureInst(inst.kind, off)
    pushcap!(vm, fullinst)
    vm.i += inst.l
    return true
end
```

This saves us a frame in both the VM stack and (more importantly) the capture stack,
in a common workload.

### Relabeling Bytecode

At some point I intend to add serializing of bytecode, just a (versioned!!) minimal
string form which allows a zero-logic construction of a Grammar.  That would be a
good time to remove NoOps, which get left around by various optimizations. There's a
reason every instruction set in existence has them, but it's aesthetically
unsatisfying to leave them in.

It isn't the only or even the best reason to want to relabel the bytecode, which can
be done in one pass as follows: We initialize an `Option{Int16,missing}` Vector the
size of the program, and a running count during the pass of how many bytes we've
added or removed, such that after a full pass, that Vector tells us: how much each
still-present instruction has moved, and whether a given offset still points at a
valid location.  There are passes where it's valid for jumps to point at a
now-missing location, but we always want to know that.  Because the count Vector and
the operation on it are both dense, we can reuse it for as many passes as we'd like.

Easy test is to generate a random number and take a program we have nice tests for
(such as `re`, soon), scatter a bunch of OpNoOps in it, correct, remove a bunch,
correct, and so on, testing each time.

#### Sparse Vectors

Are going to be the efficient way to relabel. They store any nonzero value and any
other index returns 0, so we can simply store a 1 for any added value and a -1 for a
deleted one, then relabel by calculating the sum between the offset and its jump.
Easy peasy.

### Fragment Parser

This is something I was working on doing at the code-generation level with LPeg,
which was/is the wrong way to go.  Note that this name probably belongs to embedding a
grammar in another grammar, but let's leave that aside for now.

I'm referring to a grammar which will first try to match the start rule, then if that
fails, try to match any of the subrules, in "sight order", which is the order
subrules are encountered from the start rule.

This is _very easily done_ in bytecode!  We're currently doing the standard thing of
starting with a call at [1] to the program body at [3], with [2] an [End].  But we
can replace that with [Choice, Call, Commit, Program], where Choice jumps beyond the
"end" of the program and Commit goes home.  The 'post program' is just a big ol'
choice statement trying all the rest of the rules in sight order.

This gives us a fragment parser: something which (in Lua terms) tries to make a
`:program`, then a `:shebang`, then a `:block`, a `:statement`, etc, all the way down
to making an `:add` out of `+`.  Very cheap!

#### A Wrinkle

Is whitespace really. What we want is the _longest_ fragment we can match, so that
e.g. ` :symbol` matches both.  But this is easy: we make the postscript one big
patt^0 with all the choices.  Literally it's just an additional rule `|(P(:a)...)^0`
after the end.  This can even give only grammatically-valid results if we insist on
matching a full string.

### Mark and Check: Back References

This is the separate mechanism I want to add based on the code I hacked together for
bridge.  A Mark wraps the sequence in Choice/Commit, but this one is MarkCommit: we
pop the choice frame and combine it with the MarkCommit, and this goes on its own stack.
There's also a CheckCommit, every Mark/Check pair has its own name in the Pattern but we
convert these to a counter since the VM doesn't need them. The Check looks backward for
the first Mark, if it's within the first, say, eight frames (usually it's on top) we
remove it, if not, we slip in a hold frame, and one of the canonical mark/check actions
is performed, namely: are they the same substring, is one longer than the other, or
equal length without regard to contents.  There's also a runtime mark for taking an
Action on both captures.  The result of any of which succeeds or fails the pattern.

I _could_ implement this as captures but there are good reasons not to, this decomplects
what is after all a pattern-matching operation from capturing, Mark/Check can cross rule
and capture boundaries freely, which is the important thing from my perspective.

### Dialects

Making progress on captures, want to get these thoughts out of my head to return to later.

JLPeg will offer dialects for strings which compile to patterns, at least these:

- `re`:  Implementation of Lpeg `re` module

- `peg`: Implementation of Sam Atman style PEG grammars

- `dialect`: For specifying dialects. I simply suspect this will be useful

- `regex`: I don't see any reason we can't interpret PCRE regex format on the VM

- `canonical`  A string form any dialect may be transformed to, and any
               Pattern printed as. Needs to be feature complete, rather than
               pleasant to look at or write, but must be legible. This is
               necessary for, among other things:

#### Grammar Transforming Dialects

I dunno how this works actually, but the example I have in mind transforms grammars
into highlighters, so it's a DSL to specify StyledStrings annotations to perform on a
recognized grammar.

### Expr parser

This is something I wanted to do in Lua, Julia having a canonical Expr format makes
the idea bearable, but this is a separate VM, or program really, which reparses
changes to a parsed Expr to see when they remain valid to the specified Grammar.
Crucial for getting rid of Treesitter.

This doesn't have to be such a huge change, with the fragment parser we can do it
with strings: walk up the Expr tree, match the substring (conceptually that is, not a
SubString in this case),

### Suspend the VM

I've been thinking about parsing streams, and this would require a change to the
semantics of hitting the end of input. Our "forward" patterns all check for end of
input and return `false`, but we could have a version of the VM that checks for this
condition on fails and suspends parsing, such that it can be renewed on more input or
told that there is no more, at which point a final pass/fail judgement is issued.

Also useful for e.g. iterating over a large XML or JSON file: deep recursion is
unusual, but the structure means that any interruption before the final closing
element would fail the match in an ordinary PEG parser.

### String generation

This has been on the long-term plan for a long time, and debugging the MultiSets got
me thinking about it again.  It's very simple: a different VM which interprets the
opcodes as rules for generating a string.

Easy: Chars, Sets, repetition, these are all done with a random number generator.
Choices should maintain a modulus and produce a different choice each time they're
visited, recursive self-calls are treated as a sort of repetition (which of course
they are).  I think we can ignore throws which don't have a recovery rule, and any
sequence before one (this is very uncommon anyway).

The only tricky part is predicates, and it isn't that tricky, just brute-force it:
for `!`, generate a token, test it, continue, for `~`, use the lookahead as the
generator, then apply the second rule.

I just think it would be funny to build a cheesy math parser in the demo, and then
instead of parsing an expression, generate one. There are actually useful things to
do with it as well but those are mostly debugging aids, especially if trying to make
a conformant parser for a language which already has one.

In fact, I'm going to start with a complete Set generator which just goes in order.
This is the easiest way to demonstrate that the Unicode sets aren't producing any
garbage or characters that obviously don't belong to the set (which, as it turns out,
they were).

- [#]  Generate Primitives
  - [X]  Generate PSets
    - [X]  Full generator
    - [ ]  Random-character generator
  - [ ]  Generate PChar
  - [ ]  Generate PAny(1)
    - [ ]  Random version should produce Unicode characters, some restricted ranges.
    - [ ]  Careful with this on the instruction level, many/most AnyInst(1) will be
           following a TestSet or TestChar, which of course we must generate instead.
- [ ] Generate repetition
  - [ ]  "full reps" can expand every member within a set (when allowed, aka n ≥ 0)
  - [ ]  It would be fun to special case S"AZ"^+n, S"az"^+n, and variations, such
         that in a grammar they give back a word from lorem ipsum.

### MultiSet to MultiTestSet Conversion

Is easy: we have OpFail everywhere it can fail other than the vectorized
instructions: we replace the end vectors with TestMultiVec (doesn't exist yet but
obvious opcode is obvious) and swap the OpFails with jumps to the next Choice, put an
`IAny (1)` at the end, done.

We're probably going to have to assume that MultiSet codes aren't disjoint with other
choices though, although.... with the PDiff thing we'll have a way of differing two
PSets by value, we don't have to examine the bytecode directly.

### Notes on Full Parsing

`match` is all well and good, but I'm going to want a separate system for parsing, one which returns something more useful than a `PegMatch`.

What I have so far: the basic mechanism in bridge was decent, the main lack was a way
to get back leaf nodes which were elided by suppression.  The proposal is to store
meaningful tokens as SubString pairs with the symbol as key, and "shadow tokens" as
UnitRange, with a (separate?) dictionary of offsets to obtain the rule name when needed.

Ordinary iterators and tree walkers will ignore these, but in contexts where they're
useful (syntax highlighting in particular), they're there.

### Optimal VM

For Ultimate Performance, we're going to have to do some C-isms.  This is after all
the inlining and casting is out of the way, it's about code layout.

The size of instructions ranges from 1 byte to 32, now that I packed in the structs.
All the instructions except for the ones with Int128 vectors will fit in two words,
those take four, the vector being itself two words.  The way to go is to pack all the
skinny instructions into two words, with padding such that the `.op` field is the
last byte, and then put the Set bitvecs as a fake instruction after each opcoded
instruction.  Fake in the sense that reading the .op field (which the struct will of
course not have) would be undefined behavior.

With that layout I can probably convince Julia to lay out program vectors
contiguously and directly, because they'll have a consistent size.  The offsets will
work natively because the stride length will be known.  I'll need to do some fuckery
to get the opcode byte out on dispatch, because we want to cast instructions to their
type using that byte, and if we try to read it from a field, Julia will have to look
up the type for us, so we're back to type instability.

All of this is at the end of the working document and will stay there because I don't
even have profiling set up and it's asinine to optimize on this level until I've put
in all the other optimizations and features, have wide-ranging tests, and so on.

I needed to get this out of my head so I can get on with all that.
