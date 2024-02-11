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
  - [ ]  Capture questions
    - [X]  Shorthand: `:a <--> patt` for `:a <-- [patt, :a]`.  I like this a lot.
           Ends up being equivalent to suppressed and included rules in the PEG
           format.  Needs to be a group capture though, to get the recursive
           shape correct.
    - [?]  Expression caps can just be <| `toexpr`, right? Although what's the
           symbol.  I think having a gazillion capture types is too LPeg for JLpeg...
    - [Y]  Captures in fact should just be Csimple and Csymbol, the rest are
           conceptually Actions according to the distinction I've drawn.
    - [Y]  Maybe `patt <| λ` is best, I don't want the difference between them to be a
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
  - [X]  `Anow`, `patt >: λ` (a version of this was implemented as `Q`).
    - [ ]  I have to decide what `Anow` is even for, is what it amounts to.  In LPeg it's an
           all-purpose escape hatch that lets you do almost anything.  We have the check
           mechanism, which covers the (only) use shown in the LPeg manual, matching long
           strings. I'm starting to think we just don't need it.
    - [X]  I think the answer here is we have an action `Avm!(λ)`, which calls `λ(vm)` and
           expects a true or false return value.  This is Ultimate Power without adding much
           program complexity, and I can imagine a few uses for it, debugging being the main
           one, but it serves as an all-purpose escape hatch for weird hacks.
    - [X]  `patt >: λ`?  I think this is the best one actually
    - [ ]  Refactor `aftermatch` to get a function which can enact captures across
           part of the  stack
    - [ ]  The big question here is what the function signature and return values
           look like.  The equivalent in LPeg, `Cmt`, is [rather
           complex](https://www.inf.puc-rio.br/~roberto/lpeg/lpeg.html#matchtime). Julia isn't
           Lua, we can't really use the program stack to store captures, and without `Anow`,
           the captures themselves aren't created until after matchtime.  And we don't have
           true multiple return values, we can fake it with a tuple but we have to, y'know,
           fake it with a tuple.
  - [ ]  "deferred action" form `patt <| :func`.  This one will be rather complex to
         get right, but we get some nice things out of it: assigning several actions to a
         single grammar, compile-time compiling grammars then load-time providing
         the Actions, serializing grammars without pickling functions, providing
         expanded Unicode definitions, there are a few options.
- [ ]  Add a `U8` pattern for matching an arbitrary byte.  Among other things, this
       will let me special-case the character `\0`, so I can use it to signal unused
       bytes in the unrolled `CharInst` I'm planning to add.  Fetching a character
       involves validating it as `UTF-8` and bitshifting the bytes into a 32 bit word,
       which is activity I don't need in order to match.
- [#]  Complete Mark and Check
  - [X]  Add the remaining builtins
  - [X]  Support function checks
  - [ ]  Implement indentation parsing. We may need something which checks and then
         marks, this would make sense considering we're keeping track of indentation.
         Call that `KM`, I guess.  And `CKM` I suppose?  I can synthesize this with
         `~K(p, :p)`, `K("", :p, :close)`, `M(p, :p)`, so we can see if it's useful enough
         to include a more performant shorthand.

         This is a documentation thing as much as anything. Mark/Check is a novel mechanism
         and it deserves some detail about how to use it.  I also want to figure out how this
         can be used for indentation-sensitive syntax, for my own nefarious reasons...
- [ ]  Printing stuff
  - [ ]  `clipstringat(str, i, len=20)` returns a NamedTuple tuple with the char at i,
         up to two substrings surrounding it, and booleans telling whether or not we
         truncated the string in the front or the back.  There are always substrings,
         but if `i` is the first or last character, one of them will be empty.
  - [ ]  Refactor color printing to use `printstyled`.
  - [X]  Highlight PegFail on a space with background red.
- [X]  "Modularize" the @grammar code so that users don't get unexpected results if they
       define, say, `compile!` (humorous example) and try to use it as a variable name.
- [ ]  Documenter stuff
  - [X]  Order the pages correctly
  - [ ]  Docstrings for private module names in Internals
  - [ ]  Add a "comparisons.md" page for in-depth comparison of PEGs to other systems.
- [X]  [Mark / Check](#mark-and-check-back-references)
  - [X]  `MC(patt, :sym)`: Mark `patt` with `:sym` and capture with the same name.
  - [X]  By analogy `KC(patt, :sym)` for checking-with-capture.
- [ ]  Detect "loop may accept empty string" such as `a = (!S"'")^0`.  Left recursion
       may obviate this.
- [ ]  PegMatch should implement the [AbstractTrees][Trees] interface.
  - [X]  Also we can virtualize .offsets into a property and JIT it when requested.
  - [ ]  `PegMatch` groups should be a `PegCapture` (no longer a full PegMatch), which
         subtypes `AbstractVector`, and is just a wrapper which provides the appropriate
         `keys` and `getindex` methods.
- [#]  Optimizations from [The Book](#headfail-notes) (paper and/or lpeg C code):
  - [X]  Instrument `lpeglabel` to report optimizations of interest, to get usable
         test cases and gain clarity on some thorny parts.  I tried the latest LPeg
         but was unable to get it to load without SIGKILL.  Rifle is fine.
  - [ ]  `set_disjoint` I can and should switch to UnitRangeSets, makes this easy.
  - [ ]  Add `getfirst`: used to optimize a bunch of patterns
  - [ ]  `needfollow`: used in coding Seqs (add last, computing the followset is harmless)
  - [ ]  TestPatt / headfail optimizations
  - [X]  Tail-call elimination
  - [ ]  Intermediate jump elimination.  The compiler has been println'ed to tell me when
         it's in a position to perform these optimizations, so far, no pattern would use them,
         and adding untested code paths is a bad idea at this point in the story.
  - [X]  Set ISpan optimization
    - [X]  Even better: Since Set has `:l`, we just make it `LeadSetInst(vec, 0)`.
  - [X]  fixed-length detection
  - [X]  full-capture optimization (bytecode)
  - [ ]  disjoint-PChoice optimization
  - [X]  Capture-closing optimization (vm)
- [#]  Proposed optimizations not found in LPeg
  - [X]  Immutable vector Instructions using the `getindex` from BitPermutations.jl
  - [ ]  MultiSetTest [conversion](#multiset-test-conversion)
  - [X]  Parameteric `IChar` specialized to `Char` (which is what I care about)
  - [ ]  Capture lowering: in a Choice, if each branch is fixedlen, we can lower the
         capture into the choice, and use a FullCapture to pick up the capture.
         There's some heuristic here, because even if it's only a few which are
         fixedlen, it still might be worthwhile to move the capture in.  This will
         have some subtle interactions with the code for e.g. headfails, but then again,
         a leading TestSet is a great opportunity to not even open a capture unless the
         test passes.
  - [ ]  Inlining: short rules with no captures, "short" is like, 5 instructions? 10?
    - [ ]  Definitely good for predicates, disinclined to do this for ordinary rules.
           With predicates we can remove captures and turn throws into simple fails.
           I think we have to keep the in-pred throw codes though, we can't in the
           general case inline a rule, they can be recursive, or long enough that we
           don't want to.  It would be possible to create a full copy without
           captures and throws but this is not an optimization imho.
  - [ ]  [CaptureCommitInst](CaptureCommitInst): it's a commit which creates a full
         capture from its paired Choice.  Very nice for capture-heavy workflows, like
         parsing full grammars (where we very often want a full rule).
  - [ ]  `(a / b / c)* -> (a+ / b+ / c+)*` should give better performance on common
         patterns like whitespace, where the `{\t\n }` is very frequent and the comment
         part is not, lets us use ISpan for a leading set.  I think this transformation is
         always valid, it's basic loop unrolling, and we can combine it with headfail.
  - [ ]  [MultiSets for TestSet and Span](#multitestsets).  These have the additional
         advantage that they can jump immediately if they fail (for TestSet) or
         recurse to the start instruction after a match (for a SpanSet).
  - [X]  Headfailing TestSet groups: If we have a group of multibyte sets we want to test,
         we can compress the head (lead) bytes into a single ASCII-style lead test set, by
         masking the high bit off.  This lets us single-test fail out of some very large
         ranges indeed, such as Chinese.
  - [ ]  Left recursion.  Strangely, [this paper](https://arxiv.org/pdf/1207.0443.pdf)
         describes a method for implementing left recursion, by Roberto's team (!),
         describing how to do it for the LPeg VM (!!), but LPeg has not been extended to allow
         this.  I'd like to ask the Lua list why before trying it myself...
    - [This repo](https://github.com/sacek/LPeg) is an LPeg fork with left recursion
       added, I can diff this with its source (LPeg 1.0.0) and learn things. Here's
       how to [do a file diff](https://vscode.one/diff-vscode/) in VSCode.
  - [ ]  Choice-sequence [prefix matching](#prefix-matching).  This is something
         regex automatons do automatically, which would be nice for us to have.
         Basically, it work with head sequences (including sets, and maybe
         predicates, but not repetition) such that choices which share a prefix are
         collapsed into a single choice sequence, this limits backtracking.  It ties
         into the next one:
  - [ ]  Choice shadow detection.  This is just a nice thing I'd like to do for my
         users.  In many circumstances the compiler could detect when an earlier
         ordered choice means that a later choice can't be matched, this is always a
         bug and should be brought to the user's attention.  The easy cases all
         involve fixed-length later choices, but a certain amount of detection can be
         performed with predicates and repetition in the mi as well.  For literal
         sequences it's as simple as applying the earlier rule to the string form of
         the later rule and seeing if there's a match.
- [ ]  Fragment parser (see [section](#fragment-parser))
- [ ]  String [generators](#string-generation)
- [ ]  Add beginning index or `UnitRange` as optional third argument for `match`.
       The way the VM is structured we don't even need to make a SubString, we cache
       the last index string and set the subject pointer to 1, so the fact that
       currently these are always the beginning and end of the string is just a
       special case.  Note that the VM works correctly on SubStrings already,
       although we should special-case them because this implies that all the
       indexing is going through an offset we only need to calculate once.
- [ ]  Suspendable VM [discussion](#suspend-the-vm)
- [ ]  Pure [Code Bumming](#optimal-vm) (need to be able to check if it even matters)
  - [ ]  Vector in contiguous memory for prepared programs.
    - [ ]  This might have to be custom because of indexing, it's likely to be a dip
           down into C.  Lpeg labels are jiggered based on some instructions
           (entirely charsets I believe) being extra-length, when I print pcode there
           are gaps in the numbers accordingly.  It's a major operation from my
           perspective, we'd need to obtain a pointer to the Vector somehow and
           correct all the instruction labels, but probably worth the most speedup
           after a type-stable dispatch.
  - [ ]  Optimal VM dispatch, the current system is aggressively not-type-stable.
  - [X]  Struct packing
    - [ ]  Eventually this needs to include explicit padding which puts `.op` in the 15th
           byte, this involves splitting big vector instructions into a second instruction
           which doesn't have a meaningful opcode.  Caveat decoder!
  - [ ]  Use Stack from DataStructures.jl for the instruction/capture/mark stacks.
         Proposed block size of 512 for VM and Caps and 256 for marks, which should stay
         much smaller than that in a well-functioning program.  I don't want to add this
         until we have code for turning a Pattern into a "matcher" so it can reuse VMStates.
  - [ ]  I'm surely losing cycles converting non-ASCII Chars into UInt32, should add
         Char1Inst..Char4Inst.  With NotChar and TestChar, that's 12 instructions, which is
         fine I think.  Note: this might not even be true, Chars are said to be stored in an
         "optimized form" which probably means they're raw UTF-8 unless/until converted into
         an Int.
  - [ ]  Fail optimization: only update the register once when returning from calls.
         this one should be deferred until we have real profiling on the hot loop.
         Technically this _is_ found in LPeg...
- [ ] `AbstractPattern` methods
  - [ ] count(patt::Pattern, s::AbstractString, overlap::Boolean=false)
  - [ ] findall: I think this just returns the .offsets vector of the match
- [X]  [PDiff](#pdiff)
- [ ]  [Generators](#string-generate)
- [X] Done:
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
  - [X]  `patt^[n:m]` for fixed repetition.  The `Vector` is a nice way of avoiding precedence
         problems, probably better than requiring `patt^(n:m)`.  This simply decomposes into
         `patt(n times) * patt^(n - m)`, which we can do in the constructor.
  - [x]  Macros
    - [X]  Get rid of the clunky tuple forms of `@grammar` and `@rule` by getting the
           escaping rules for the macro correct.
  - [X]  All `CaptureInst`s same struct w. distinct Pattern subtype
  - [X]  Un-pirate: shadow operators with a definition which falls back to Base,
         don't extend Base for operators directly.
    - [X]  Separate module JLpeg.Combinators for exporting the operators directly.
           They'll still work in `@rule` and `@grammar`.

[Trees]: https://github.com/JuliaCollections/AbstractTrees.jl

### Compiling Redux

The peephole optimizations invalidate the assumption that a code can simply be lifted
up by the enclosing pattern and always be valid.

The (one may hope, final) form is thus:

- [ ]  A `build` (note, no ! now) method, which may be passed an instruction vector.  This
       appends the code to that vector and returns it.

- [ ]  A `compile!` method, which `build`s subsidiary patterns, but this time, into
       its own `.code` field.  This is idempotent, so we return the pattern immediately
       if the `.code` vector isn't empty.  This also does peephole optimizing on the final
       product, and this (and only this) adds `OpEnd`.  No `prepare!`, no `prepared!` field.

### Headfail Notes

The remaining "classical" optimizations all cluster around headfailing: that is,
letting a complex pattern fail on the first test, in particular before pushing a
Choice frame (or Capture, or often both).  This is important for real-world patterns,
especially in using patterns for regex-type tasks.

The lpeg code is difficult to port because it has that classic C pattern where mutation
happens by passing a pointer and return values are just integers which are used in the
logic.  There's also a lot of... laconic use of Boolean masks and the whole thing is
further complicated by the Instruction type being a union, which is only sometimes what
we call an "Instruction", labels are a second "instruction" and charsets take up a lot of
space.

Last but not least, lpeg stores patterns as a binary tree, this is particularly significant
for seqs and choices, which we unroll.

TL;DR the way forward is to identify the **function** of the various optimizations, and their
results, and only the implement them in the JLpeg context.

#### On Choice

The algorithm does one section of a choice at a time, making sure that a) the choice
is headfailing and b) the choice has a firstset which is disjoint from all the
others.  If only a) is true, we get a TestSet/TestChar, then a Choice, an Any (to
soak up our match), the body, the Commit. If a) and b) are true, we get the testchar
with a jump to the next option, because we don't have to backtrack: once we're in the
money, we have to succeed or fail.

Note (this being the thing I finally figured out) that since choice is ordered, we
only have to check each subsequent choice against the remaining headsets. So if '1'
is the leadset for choices 1 and 3, 1 has to be choice/commit, but 3 can be test/jump.

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

Update: I think this one is useless. If there are no intervening captures, the VM
will close it, if there are, we can't really use this without backtracking at some
point.  Although if we're doing [Stay Winning][#stay-winning] anyhow, that separates
capturing from building the final capture, so backtracking a group capture is no big
deal: the point of this sort of optimization is to make the pattern cheap when it
fails, the work involved in inserting a virtual opencapture is modest most of the
time.  There are also simple captures (think variable names) which could benefit
somewhat from this instruction.  Although how many? This is relatively seldom
covering all branches of a choice, it's mostly a single choice, and the VM will collapse
the capture anyway since it isn't nested.

We can use this whenever a `PCapture` is enclosing a `PChoice`, but it's a slightly
tricky optimization to get right.  What we do is go through the copied bytecode and
replace every Commit (not Partial Commit) _which belongs to the `PChoice`_ with a
`CaptureCommitInst`.  This is best accomplished by re-synthesizing the choice code
from parts.  The last patttern in a PChoice doesn't get choice/commit bracketed,
if it's fixedlen we can use a full capture, if not we wrap it in an open/close.

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

This saves us a frame in both the VM stack and the capture stack, in a common
workload.

Note: We can't apply this optimization if there are nested captures and the
CloseCapture is of group type.  There are ways to do it but, as far as I've been able
to figure, they involve fiddling around with the capture list after match, in a way
which has got to be slower than just allowing a bracketed capture.  The VM ends up
collapsing open/close pairs which are touching anyway, so the capture list should end
up being entirely full captures except for groups, which are inherently nested
structures.

### Prefix Matching

This is an optimization performed on choices, which can be used (perhaps further
analysis is needed) to detect bad choice ordering, and is a necessary analysis for
[stay winning](#stay-winning).

- Precepts:
  - PChar, PTrue, PFalse, PSet, PThrow, match literally
  - PAny matches up to the shortest `n` value
  - PStar matches if the body is the same and n is the same
  - PAnd, PNot, match if: same predicate, both bodies prefix-match
  - PChoice: matches up to the longest prefix of the choices (recursive)
    - A PChoice directly within another would be flattened during construction, but
      e.g. predicate choices can be lifted: `!("abc" | "abd")` can be `!("ab" ("c" |
      "d"))`.
  - PCall: matches if the rule is the same
    - If only one is a PCall, we can try prefix-matching the referenced rule, but
      this has too much potential for surprising behavior, I'd say.  I'm reluctant to
      inline rules for the same reason.
  - PCapture: matches if same kind, same tag, and same body

I note there's basically no leeway at all, which makes sense, the different patterns
exist because there are a finite number of element of recognition, and if any such
differ, they match a different string, except for.... prefix and suffix matching!

When prefix matching is completed, we may attempt suffix matching on the remaining
choice bodies. It works the same way but backward.

This will give us much better patterns in JLpegUnicode, where there are a lot of
prefix- and suffix-matched instructions in the gnarly combining forms.

If an earlier rule matches a later rule completely, we've found a bug: the later rule
can never be reached, due to ordered choice. The rules for shadowing are somewhat
different, for example, patt^1 shadows patt^0, a superset shadows a subset, the
principle is that a pattern shadows another pattern if it can always match on the
same string as the second pattern, even if the second pattern would match longer or
just differently for the rest of it.  Something like `"a" / "aa"` is the classic
example.  This is somewhat related to headfails and disjoint sets in fact, but overlapping
heads are ok as long as the first rule has anything distinct which it *must* match.

Where it gets complicated is rules like `"a" ("b" | :c | "d") / "ac"`, where `:c` matches
a literal `"c"` in its prefix.  When the time comes we'll need a bunch of badly-structured
grammars like this, with captures and marks and all the bells and whistles.

Any time we prefix-match an entire earlier pattern with a later one, we've got a
shadow, but to reiterate, there are more conditions where this arises.

#### What Can We Do With This

There are four states a Choice can be in with the full analysis.

1. It cannot be prefix or suffix matched, no rules shadow, etc. No change.
2. We can successfully merge prefixes or suffixes.  We do so.
3. There is an earlier choice which prevents the execution of a later choice from
   ever happening. We report this as a bug.
4. There are common pathways through the prefix, but earlier rules match longer
   strings than later ones.  This is where we apply [Stay Winning](#stay-winning).

Detection is actually the tricky part here, we need to look through multiple rules to
see what's going on.  Something tells me this is solved with a custom data structure,
like a Trie, but where adding a rule as a choice applies the required comparison
rules and maintains references into the original Pattern, prevents runaway recursion,
and so on.

### Relabeling Bytecode

Status: this is almost entirely pointless. `OpNoOp` shows up almost entirely
(entirely?) in places which the code can't reach anyway.

At some point I intend to add serializing of bytecode, just a (versioned!!) minimal
string form which allows a zero-logic construction of a Grammar.  Precompilation
admittedly makes this less compelling... That would be a good time to remove NoOps,
which get left around by various optimizations. There's a reason every instruction
set in existence has them, but it's aesthetically unsatisfying to leave them in.

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
e.g. `:symbol` matches both.  But this is easy: we make the postscript one big
`patt` with all the choices, and a `!P(1)` at the end of each.  Not much more than
just `|(P(:a)...)^0` as a postscript.

We might in fact want to do this in a more sophisticated way, we can sort the rules
so that longer patterns match before shorter ones, giving a fragment parser which
yields the most coherent fragment.  We can in fact do this, it's the same algorithm
we need for [Stay Winning](#stay-winning).

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

This is fairly easy to do: we're in a `while` loop, so just `break` on the instruction.
Then check `vm.running`, if it's still true, follow the `onsuspend` codepath.

Proposal is

### String generation

This has been on the long-term plan for a long time, and debugging the MultiSets got
me thinking about it again.  It's very simple: a different VM which interprets the
opcodes as rules for generating a string.

I think I'll hate it if I try to interpret the bytecode itself, so we're talking
about a second compiler and a looser VM where "instructions" are mutable so they can
hold state, and the code is flattened but not unrolled into commits etc.  No need to
overthink the format at this stage, just note that it isn't a direct Pattern interpreter
or a VM bytecode interpreter, it uses its own format which it derives from Patterns

Easy: Chars, Sets, repetition, these are all done with a random number generator.
Choices should maintain a modulus and produce a different choice each time they're
visited, recursive self-calls are treated as a sort of repetition (which of course
they are).  I think we can ignore throws which don't have a recovery rule, and any
sequence before one (this is very uncommon anyway).

One tricky part is predicates, and it isn't that tricky, just brute-force it:
for `!`, generate a token, test it, continue, for `~`, use the lookahead as the
generator, then apply the second rule.

Another, trickier one, is `Q`, and `K` with a lambda, those can call for
backtracking.  I might insist on extra data being provided for these, captures
weren't intended to be executed (no need) but it's tractable to provide input to a
Check (or, with more work, a Q) which it will accept, and restrict emission of that
pattern to acceptable strings (no need at that point to include the checks, marks, or
action). We can work backward on the builtins, since it's easy enough to synthesize a
string which will always succeed (though we have to then _match_ that string on the
pattern which would generate it: if this fails, we've found a rule which can't
succeed, since the checking postcondition can never arise if the pattern precondition
never matches).  The idea is to be able to generate valid strings which another
parser will accept, so it's important to support this kind of out-of-band business,
even if it calls for extra work on the part of the user.  Checks and queries based
on a custom function are enough of an edge case to handle last, if at all, the perfect
shouldn't be the enemy of the good here.  They're almost certainly impossible to
satisfy with perfect generality.

I just think it would be funny to build a cheesy math parser in the demo, and then
instead of parsing an expression, generate one. There are actually useful things to
do with it as well but those are mostly debugging aids, especially if trying to make
a conformant parser for a language which already has one.

In fact, I'm going to start with a complete Set generator which just goes in order.
This is the easiest way to demonstrate that the Unicode sets aren't producing any
garbage or characters that obviously don't belong to the set (which, as it turns out,
they were).

#### Structure

We want an abstract type `PatternGenerator` and subtypes: `GSet` for `PSet`, and so
on. `generator(p::Pattern)` returns the entire pattern converted to a generator,
which can be called with an optional randomness seed and IObuffer.

#### Tasklist

- [#]  Generate Primitives
  - [X]  Generate PSets
    - [X]  Full generator
    - [ ]  Random-character generator
  - [ ]  Generate PChar
  - [ ]  Generate PAny(1)
    - [ ]  Random version should produce Unicode characters, some restricted ranges.
    - [ ]  Careful with this on the instruction level, many/most AnyInst(1) will be
           following a TestSet or TestChar, which of course we must generate instead.
           This is a good reason to be creating separate generators, rather than a
           different sort of bytecode interpreter. There are several of those.
- [ ] Generate repetition
  - [ ]  "full reps" can expand every member within a set (when allowed, aka n ≥ 0)
  - [ ]  It would be fun to special case S"AZ"^+n, S"az"^+n, and variations, such
         that in a grammar they give back a word from lorem ipsum.

### MultiTestSets

Is easy: we have OpFail everywhere it can fail other than the vectorized
instructions: we replace the end vectors with TestMultiVec (doesn't exist yet but
obvious opcode is obvious) and swap the OpFails with jumps to the next Choice, put an
`IAny (1)` at the end, done. MultiSpan is the same but the end is a jump back to the
LeadSet(s).

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

I think I'll find that making an Expr is a special case that doesn't make a lot of
sense if I'm not parsing Julia, which of course, I do intend to do.

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

#### Links

Looks like [StaticTools.jl](https://docs.juliahub.com/General/StaticTools/stable/)
has the basics for allocating a dense Vector on the heap. From there it's a matter of
making the instructions the same size, locating `.op` in a consistent location, and
writing an unsafe decoder in the VM, which reads the exact byte of .op and casts the
initial pointer to the correct type.

The core of what needs doing is to pre-allocate a Vector of a single concrete
Instruction type (I intend to make one special for this purpose) of 16 words, with an
.op field, then iterate the Instruction Vector and use `reinterpret` to cast
everything to that instruction type.  Then in the dispatch loop, indexing gets the
correct bits, with `.op` in the right place, and we use that information to
re-reinterpet the instruction into the correct shape, thus dispatching to the correct
method in a type-stable way.

This will give nonsense results if accidentally called on a "long vector" but correct
code will never do this.

There's some finesse in there which I'm missing, but this is the basic idea.  It may
even make sense to take over manual memory management of the VM stacks, but that
seems less likely to yield greater performance than finally shaking off the boxed
instructions and runtime type decoding.

Taking a close look at the instructions, I can fit anything which isn't a Vector into
one word, which has nice properties, to put it mildly.  It will take a bit of finesse
to handle the ASCII vector tests, which are two words wide, but there are a few
approaches which will serve.  One is to treat the mask like two masks and just
remove a high bit from 0x40-0x7F, picking the vector accordingly.

Compiling is the actually challenging part here, I think this is solved by breaking
Set instructions down into however many parts are needed, rather than doing something
fancy to keep track of which indices are multi-instruction.

### Stay Winning

I want to figure out how to implement the Stay Winning algorithm with the bytecode
VM.

Example code:

```julia
    :expr         ←  :alt | :seq | :element
    :alt          ←  [(:seq | :element) * :S * (S"|/" * :S * (:seq | :element))^1, :alt]
    :seq          ←  [:element * (:S * :element * :S)^1, :seq]
```

Stay Winning means we always keep a `:seq` or `:element`.

The main semantic change is one added instruction, the complexity is in handling
captures.  A `WinCommitInst` first does an ordinary Commit, popping its own choice
frame, but before it jumps to the label, it updates the new top frame `.ts` and
`.tc`, and uses a second label to update `.ti` as well (this will include marks and
left recursion, once those are added). LabelInst, which CommitInst is a member of,
have a free 16 bit slot (and an 8 bit slot for that matter), and since this would be
coded as an offset-to-the-offset, this is more than enough.  I can imagine JLpeg
programs with a total length of more than 65536 instructions, but not one where a
single update during a prefix-matched parse changes the offset by more than 32767 or
-32786 instructions.

That way, if the longer rule fails, it keeps the advanced subject pointer and cap
stack, and just jumps where e.g. `seq` would have gone anyway.

This will make peepholing interesting though, because we'll want to optimize the jump
on a WinCommitInst based on where it sends the Choice it's paired with, which is
surely deducible from the bytecode but not trivially.

Question to answer: am I going to be able to guarantee that the choice frames are all
adjacent to each other, with no intervening call frames?  That is definitely not a
property which we have now.  But I think it falls out of the prefix matching.

Consider: with `:alt, :seq, :element`, we currently push the `:alt` choice frame in
`:expr`, then call `:alt`, push the `:seq` choice frame, call `:element`, and
backtrack over those rules several times over if we don't get a "|".  With the prefix
matching, we push `:alt, :seq, :element` choice frames, then call to `:element`.
`:element` failing has to fail all those, but if `:element` succeeds, we drop the
call frame on `return`, drop the `:element` choice frame on WinCommit, which updates
the `:seq` frame to point at the element-win condition. `:seq` has to fail twice,
succeeding updates the `:alt` jump to point at the `:seq` win, and `:alt` succeeding
is just an ordinary `:commit`, with the unconditional jump to an ordinary CloseCapture.

If we can avoid intervening call frames (looks like it) we don't have to go looking
for the choice frame we're modifying, it's in the register.  That would be ideal.

#### Captures

We need to be careful with captures though. In the above code, as currently
implemented, failing to find a `S"|/"` will unwind the capture stack, dropping the
captured :seq, which is what we want to avoid.  So it isn't legal to update the
`:alt` choice frame with a new capture height, because that will strand an opencapture.

[CaptureCommits](#capturecommitinst) don't help here, because they add a frame to the
capture height.

But, we could add a special kind of fail instruction. Every failable pattern comes
with a test form, the fail could have the tag of the capture to slip out of the stack
during unwind.  I don't like this on first glance: it complicates unwinding, not
necessarily making it slower, but now we have two ways to change the `lcap`, not one,
and the height of the capture frame is stored in any choice frame, so we'd have to
reason fairly carefully about whether and how this is legal. Intuitively it's
acceptable, because it doesn't change any values below the one we're removing.  This
would also complicate coding of differentiating patterns, which can include lookahead.

I earlier had notes about leaving a stranded frame and marking that as a possibility,
but that would just end up being backtracking with extra steps, or allocation of
spurious vectors which have to be spilled.

What should work better: a special closing instruction telling us not to include the
grouping thus established. That lets us process in linear time, and we retain the
nested-capture property.

I think this ends up being pretty cheap in practice. We open a group, a lot of the
time it's empty, the rest of the time we just append it to the existing group.  We
can even have a spare-parts stack to stick the emptied Vector for a later group
capture, that spares some of the allocation pressure, and it's not even branching:
it's just `cap = pop!(groupstack); append!(captures, cap); empty!(cap); push!(cap,
groupbuffer)`.  That's gotta be cheaper than backtracking, especially with some of
the bad patterns which this memorizes against, valid Lua Lvalue chains being the
example which first got me thinking about this algorithm.

Or even better: we simply create the captures on their own Vector, then slice it up
at the end. We end up with one stack of data, and another stack of Tuples with index
references into the data and what to do with it. For fake groups the answer is
nothing. No extra group stack either, we push open captures as a tuple of the capture
and its index into the group vector, and when we get a fake close, we can O(1)
 replace it with a sentinel value. Two passes always sounds like more work, I have to
remind myself no, it's the same amount of work being done in a different order. It
isn't the sort of thing I'd do if it wasn't useful, there are some weird bits, we
still need the operation stac

Cheap to code, for the VM, too. A WinCommit points the Choice at a new label, we just
make that location our special CloseCapture (a `LidCaptureInst`, let's say). It has
its own opcode and we can therefore include the final jump in the instruction.

Clearly the Stay Winning optimization is going to be the last thing we add.  I'll
want to benchmark it, for one thing.

Another thought here: We have UInt16 worth of unused instruction space in
CaptureInst, one larry, if you'll indulge my whimsy.  That allows for a special tag
which opens a capture which can be closed in several ways: we use the larry to carry
a second tag which is used to retrieve the action of closing.  This is promising I
think, because we don't need special jump instructions to pseudo-close the captures.
As an illustration, with `:seq, :alt, :element`, we open it with a unique tag, and
use WinCommitInst as before to target unique captures, all of which close the open
capture, but with differing larry-tags.  We can even rewrite this into a FullCapture
if the open and close end up next to each other, just use the larry tag instead of the
normal tag to write the full capture.  Yeah this is obviously the way to go, and one of
the tags (it should be the longest) can be a normal close.  That only helps because it
means we don't have to make up a virtual tag, instead using a different opcode for the
CloseCaptureInsts where the larry-tag applies. A `TaggedCloseCaptureInst`.

Can't really beat that, can we. No bloat on the capture stack at all, no significant
changes to the capture algorithm, just adding a conditional branch for
`TaggedCloseCaptureInst` and a different dispatch for the full-capture optimization when
pussing a `TaggedCloseCaptureInst` onto the capstack.

...This only works if the captures aren't nested, which they are more-or-less by
definition.  There may be a few cases where this isn't true, but not enough to make
it worth it.  Bugger all.  I suppose the original solution wasn't so bad...
