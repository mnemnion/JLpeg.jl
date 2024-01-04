# Planning for jLPEG

   The intention here is to translate [LPEG](https://github.com/sqmedeiros/lpeglabel),
specifically the lpeglabel fork, into idiomatic Julia, and expand it with the
various bells and whistles which I've introduced, or plan to, into PEGs generally.

## Implementation

Wherein I note how I'm building the thing.

### Remaining

The hitlist:

- [X]  Multibyte sets and chars
  - [X]  Implement multibyte sets
  - [X]  Fix bug with emoji 🫠.  Appears to work?  This problem may reappear though.
  - [ ]  [Multiset Rewrite](#multiset-rewrite)
- [#]  Captures
  - [X] Cc
  - [ ] `Ce`, `=>`
  - [ ] `Anow`, `>`
  - [ ] `Cf`, `./`
  - [ ]  "deferred action" form `patt |> :func` || `patt > :func`.  This one will be rather
         complex to get right, but we get one critical and one nice thing out of it: assigning
         several actions to a single grammar, and compile-time compiling grammars then load-time
         providing the Actions.
- [ ]  [Mark / Check](#mark-and-check-back-references)
- [ ]  Detect "loop may accept empty string" such as `a = (!S"'")^0`
- [ ]  Optimizations from The Book (paper and/or lpeg C code):
  - [ ] TestPatt / headfail optimizations
  - [ ] Tail-call elimination
  - [ ] Set ISpan optimization
  - [?] fixed-length detection
  - [ ] full-capture optimization (bytecode)
  - [ ] disjoint-PChoice optimization
  - [ ] Capture-closing optimization (vm)
- [X]  All `CaptureInst`s same struct w. distinct Pattern subtype
- [ ]  Proposed optimizations not found in LPeg
  - [ ]  Fail optimization: only update the register once when returning from calls.
         this one should be deferred until we have real profiling on the hot loop.
         Technically this _is_ found in LPeg...
  - [ ]  Inlining: short rules with no captures, "short" is like, 5 instructions? 10?
  - [ ]  CaptureCommitInst: it's a commit which create a full capture from its paired Choice.
         Very nice for capture-heavy workflows, like parsing full grammars (where we very often
         want a full rule).
  - [ ]  `(a / b / c)* -> (a* / b* / c*)*` should give better performance on common patterns
         like whitespace, where the {\t\n } is very frequent and the comment part is not, lets
         use use ISpan for a leading set.
  - [ ]  Follow sets for TestSet and Span.  These have the additional advantage that they can
         jump immediately if they fail (for TestSet) or recurse to the start instruction after
         a match (for a SpanSet).
  - [ ]  Headfailing TestSet groups: If we have a group of multibyte sets we want to test,
         we can compress the head (lead) bytes into a single ASCII-style lead test set, by
         masking the high bit off.  This lets us single-test fail out of some very large
         ranges indeed, such as Chinese.
- [ ]  Multigrammars (see [section](#multigrammar))
- [ ] `AbstractPattern` methods
  - [ ] count(patt::Pattern, s::AbstractString, overlap::Boolean=false)
  - [ ] findall: I think this just returns the .offsets vector of the match
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

### Multiset Rewrite

Now that I have significant progress on [JLpegUnicode](httk://add.repo.soon), it's
time to make MultiSets optimal.  Optimal within the framework of the VM, that is,
this might be somewhat slower than a perfect NFA, but then again, maybe not.  I refuse to add a regex special-case to the VM.

The first thing is to replace "final" with an instruction label, so that any match
can skip the rest of the MultiSet in a single jump, this lets us do a simple loop for
the rest of the tests.

Next, we need a `ExtTestSetInst` instruction.  This fails if the high bit is zero, if
not, we mask it off and test the byte against a BitVec, telling us whether our
extended char is by definition not in our set.

Test instructions all have a label, when this is done *all* SetInst will have a label
(and no `.final` field).

This alone would give relatively good performance, but since we have labels in Sets now, we can do better than this, by breaking 3 and 4 character sets down into head-matching leads with a jump to the tail match.  So an IByteInst which matches against the headbyte and jumps to the MultiSetInst, which is now just a lead byte and a Bitvector64.

So S"😾👍" (we don't do headfails for less than, let's say, four multibyte chars) looks like this:

```txt
01 IByteInst 0xf0 :05
02 IByteInst 0x9f :05
03 IMultiSet lead:0x98 vec:{0xbe} :05
04 IMultiSet lead:0x98 vec:{0x8d} :05
05 [next instruction]
```

Or, come to think of it, maybe `IMultiSet` is just a :vec and an :l and we use
`IByteInst` with a label to consolidate.  I like that more.  (Now I'm getting
distracted thinking about how Vectors get laid out in memory and how to eke out that
performance. Later!)

So a complex byteset (Unicode category, as the motivating example) is laid out like this (I'm assuming three bytes for simplicity, the pattern generalizes to two or four)

```txt
    ExtTestSetInst (testing if byte 1 ) :A (this can fail)
    SetInst :END (If it contains ASCII, also can fail)
A:  IByteInst 0x[A] :B
    IByteInst 0x[B] :C
    ...
B:  IByteInst 0x[C] :D
    ...
C:  IByteInst 0x[D] :E
    ...
    IFail
D:  IMultiSet [:vec] :END
    ...
E:  IMultiSet [:vec] :END
    ...
    IFail
END:  (rest of program goes here)
```

This layout respects the convention "succeed, go to label" and in fact, we shouldn't
need to special case any of these instructions with `followset!` or anything like it,
we just put an `IFail` at `:END`.  That's much nicer, innit.

That said, the advantages of running a single Set as one dispatch are compelling.  We
don't have to unwind the subject pointer, `vm.s` never points at an invalid character
(correct code doesn't need that property, to be fair), we only need to extract each
byte once (we know when an `IByteInst` has matched and can retrieve the next byte
before we jump-to-label), a matching `IMultiSet` advances the subject pointer before
jumping to :END.  Each `IMultiSet` either succeeds or fails, so we just need one `IFail`
between the `IByteInst` and the `IMultiSet`.

The bear of it will be consolidating `PChoice` instructions into a single mega-set,
but the use of `IByteInst` actually makes this easier.  The first `IByteInst`
contains the offset for any second byte, which has the offset for any third byte,
which points to the MultiSets.  Even mixed-byte-length combination is (relatively)
easy, because by definition the head-bytes are disjoint, so we can still put all the
MultiSets at the end.

An IChoice has potentially dozens, maybe hundreds, of Sets (Unicode, remember), and
we want to be able to fixup the code in a single pass, so how's this for an
algorithm: we go through each MegaSet, and make a Dict where each jumpable
instruction (that isn't to the end) is the key, and each landable instruction is its
value.  Then we shuffle all instructions into proper order, with a second Dict
showing where we put everything, then do a fixup pass where we relabel all the jumps
with their new Index.  For consistency we key all instructions, the ones which need
to jump to the new OpEnd get a special HoldInst as a value.

Algorithm then:

- Collect all ASCII, 1byte, 2byte, 3byte, and MultiSets, Dict them appropriately
- Compute the new ASCII test, append
- Compute the new headfail for the 1bytes
- Append all 1bytes, then 2bytes, then 3bytes, then the IMultisets, and OpEnd
- Fixup pass to replace all instructions with their relabeled version.

#### MultiSet Checklist

This calls for a certain order of operations!

- [ ] Add a bunch more MultiSet tests (emoji in particular)
- [ ] Refactor `.final` field into `.l` labeled jump
- [ ] Add `IByteInst`, refactor `IMultiSet` for new bytecode

### Throw notes

Currently adding throws to JLpeg, which are the basic mechanism to build good error reporting
and recovery. Observations from the code:

Adds three new instructions: `IPredChoice`, `IThrow`, and `IThrowRec`.

`IPredChoice` isn't directly related to throws, it's so error reporting works correctly (?). It's coded by (for us) `PAnd` and `PNot`, and acts just like a choice in ordinary unwinds.

`IThrow` and `IThrowRec` differ based on whether the grammar has a recovery rule with the label name; the latter has an offset to the recovery instruction (other differences TBD).

`lpeglabel` adds two booleans, `labenv` and `predchoice`, to each stack frame, and an `insidepred` de-facto register (local variable inside the dispatch loop, so same thing).

The essential distinction is that `labenv` is `true` anytime we have any sort of PredChoice on the stack, and `predchoice` itself means we're inside an actual predicate choice (and not an ordinary choice, within a predicate choice or not).  These are both `false` in call frames.

This is so that throws can unwind through ordinary Choice frames, which I should be able to do
just by checking the opcode of the frame's instruction pointer, which I like more.

So that means we add a `p` Bool to each StackFrame, a `tp` stack register, an `inpred` VM register, and we unwind predicates on throw the (relatively) expensive way, by checking the instruction pointer each time we see an .s.

#### [X] PThrow Checklist

- [X]  Re-code `PAnd` and `PNot` to use `IPredChoice`
- [X]  Add `p` to stack frames, `inpred` and `tp` registers, proper updating in:
       `onChoice`, `onPredChoice`, and `failinst`.
- [#]  Code ThrowInst and ThrowRecInst, starting (ofc) with `PThrow`.
  - [X]  Good time to re-code the compiler to stop trying to float auxiliaries and just
         pass in a `:throws` and `:caps` Dict during `prepare!`, good preparation for
         better compiling of Grammars.
  - [X]  Code ThrowInst
  - [X]  Code ThrowRecInst

### Grammar Compile Rewrite

Time to tackle codegen in an order which lets us determine what's going on with rules
at a time when it's useful to the calling rule.

#### [X] Rule Traversal

This should begin by replacing all POpenCall with PCall, only then generating code.
What this means is we pass along the rulemap to the first rule, then go visiting
subrules: if it's terminal, we compile it, if we see it twice, it's recursive: if
that's left recursion, bail out, otherwise we tag as such (and therefore variable
length by definition).

Whenever we reach the end of a visited rule, we know if it's recursive, and all
ultimately-terminal calls (at whatever degree of remove) are compiled / we know
what we need to know, so we can compile that rule, and when we reach the end of the
start rule, we're done, and ready to hoist and link.

We use `:aux` to store everything we determine about rules, nullable, nofail,
fixedlen, terminal, visited, anything else we really need to know.

#### Compiler Rewrite

I'm not satisfied with the flow of the current compiler, and I don't entirely understand
why.  It might not even be about that, might just need to be more willing to lift the
sub-codes of, particularly, `PChoice`, to rewrite them under some circumstances.

Two things and I might not need both:

- [?]  Pass in the parent in `compile` to `_compile`, to condition compiling on parental
       context.
- [?]  Add a `hoist!(parent::Pattern, child::Pattern)` method, which by default returns
       the `.code`, but which can contextually rewrite it or copy it based on expectation
       of the parental unit.  I like this better actually, full dispatch is one of the
       gems of Julia and it is rather distinct behavior from compiling itself.  The
       contract is that the parent (by being what it is) informs the child if a copy
       or copy-and-rewrite is needed.

I like `hoist!` better.  A pattern doesn't need its parent to compile, this makes
"rewrite in context" its own operation, which I expect will be easier to follow.  It
lets us have specialization in both directions, such as an enclosing rule which
always needs a copy, if such exist.

### Capture closing

I knew there was a reason I might want to cache the capture stack...

This optimization changes `CapEntry` back to holding the values of the instruction
separately, and adds a capture register holding the values of the top frame.  When we
get a CloseCaptureInst, we mutate the register to hold a FullCaptureEntry with the
calculated offset. And this simplifies the `aftermatch` code somewhat because there
will only be FullCaptures, if we find a not-FullCapture there's a mistake somewhere.

This would also mean not having to synthesize a FullCaptureInst in the following,
although I doubt very much that this would generate different machine code, but
maybe: the compiler pays a lot of attention to method dispatch.

### CaptureCommitInst

We can use this whenever a PCapture is enclosing a PChoice, but it's a slightly tricky
optimization to get right.  What we do is go through the copied bytecode and replace
every Commit (not Partial Commit) _which belongs to the PChoice_ with a
CaptureCommit.  We can accomplish this by using a counter of PChoice instructions in
the run, and if that counter is greater than 0 then when we encounter ICommit we
decrement the counter rather than swapping in a CaptureCommit.

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
good time to remov NoOps, which get left around by various optimizations. There's a
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

### Multigrammar

This is something I was working on doing at the code-generation level with LPeg,
which was/is the wrong way to go.  Note that this name probably belongs to embedding a
grammar in another grammar, but let's leave that aside for now.

I'm referring to a grammar which will first try to match the start rule, then if that
fails, try to match any of the subrules, in "sight order", which is the order
subrules are encountered from the start rule.

This is *very easily done* in bytecode!  We're currently doing the standard thing of
starting with a call at [1] to the program body at [3], with [2] an [End].  But we
can replace that with [Choice, Call, Commit, Program], where Choice jumps beyond the
"end" of the program and Commit goes home.  The 'post program' is just a big ol'
choice statement trying all the rest of the rules in sight order.

This gives us a fragment parser: something which (in Lua terms) tries to make a
`:program`, then a `:shebang`, then a `:block`, a `:statement`, etc, all the way down
to making an `:add` out of `+`.  Very cheap!

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
Action on both captures, the result of which succeeds or fails the pattern.

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
changes to a parser Expr to see when they remain valid to the specified Grammar.
Crucial for getting rid of Treesitter.
