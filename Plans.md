# Planning for jLPEG

   The intention here is to translate [LPEG](https://github.com/sqmedeiros/lpeglabel),
specifically the lpeglabel fork, into idiomatic Julia, and expand it with the
various bells and whistles which I've introduced, or plan to, into PEGs generally.

## Implementation

Wherein I note how I'm building the thing.

### Remaining

This list could be a lot longer!

- [X] Handle the other PStar cases
- [X] `P(-n)` for n bytes remaining
- [X] And and Not predicates
- [X] Check lpeg code for #P(whatever) inside a call, does it use BackCommit?
- [#] Multibyte sets and chars
  - [X] Implement multibyte sets
  - [ ] Fix bug with emoji 🫠
- [X] Interface stuff
  - [X] The grammar munges every possible string into P"string", symbols too.
        We need to fix that, possibly by rewriting as a prewalk, not postwalk?
  - [X] `←` needs to take the capture forms on the right hand side
- [X] `B(patt)` (prerequisite: determining fixed-length patterns)
- [X] `T(:label)` somewhat hefty VM refactor here
  - [X] `PegFail` object with test conversion
    - [X] default label is `:default`
- [#] Captures
  - [ ] Cc
  - [ ] Ce, =>
- [ ] Mark / Check
- [ ] detect "loop may accept empty string" such as `a = (!S"'")^0`
- [ ] Optimizations from The Book (paper and/or lpeg C code):
  - [ ] TestPatt / headfail optimizations
  - [ ] Tail-call elimination
  - [ ] Set ISpan optimization
  - [?] fixed-length detection
  - [ ] full-capture optimization (bytecode)
  - [ ] disjoint-PChoice optimization
  - [ ] Capture-closing optimization (vm)
- [ ] All `CaptureInst`s same struct w. distinct Pattern subtype
- [ ] fail optimization: only update the register once when returning from calls
- [ ] Serializing and loading grammars
  - [ ] Deserializing constructors for PRule and PGrammar, `@rule!` and `@grammar!` macros
- [ ] CaptureCommitInst: it's a commit which create a full capture from its paired Choice.
- [ ] AbstractPattern methods
  - [ ] count(patt::Pattern, s::AbstractString, overlap::Boolean=false)
  - [ ] findall: I think this just returns the .offsets vector of the match

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

#### PThrow Checklist

- [X]  Re-code `PAnd` and `PNot` to use `IPredChoice`
- [X]  Add `p` to stack frames, `inpred` and `tp` registers, proper updating in:
       `onChoice`, `onPredChoice`, and `failinst`.
- [#]  Code ThrowInst and ThrowRecInst, starting (ofc) with `PThrow`.
  - [X]  Good time to re-code the compiler to stop trying to float auxiliaries and just
         pass in a `:throws` and `:caps` Dict during `prepare!`, good preparation for
         better compiling of Grammars.
  - [X]  Code ThrowInst
  - [ ]  Code ThrowRecInst

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

#### [ ] Compiling

I'm trying to figure out here what the operations for compiling even are.

My basic annoyance with the current compiler is that it prewalks the code. Advantage: didn't have to sprinkle "compile!" everywhere.  Disadvantages: numerous.

What I want is to dump that, and pass through a `link` Dict.

Things to determine/do on recursive walk:

- [X] Sight order: which rules are seen first, the code will be laid out accordingly
- [X] Which rules are recursive
- [ ] `:hascaps` (need for FullCapture optimization)
- [ ] Compile everything which doesn't have a rule call.

 [ ] Inlining? Should we? What circumstances?
 [ ] Tail-call elimination

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

### Serializing

Compiling is expensive, potentially very expensive once we start doing cool stuff.
And we make no use of the Pattern at all, except the top one as a container for
`aux`, after we create it. So I want to add a serialize-to-string function which is
as fast to load as computerly possible, some ideas: Capital letters are enums (Opcode
and CapKind), lowercase letters are short labels, - means what it always does, `n`
comes before `l`, jumps further than +-26 are hex as `0ff`, always lowercase because
we expect an opcode next, always at least two, if an n is encoded as hex, the label
is always encoded as hex.  I believe that's unambiguous and forward-only, and only
leaves out sets, which we already know how to serialize with show in a basically
optimal way, although we'll  make some minor changes in the interest of forward-only
unambigious optimality: starts with `{`, characters are in order and concatenated,
ranges start with `|` and cover the next two characters.  That leaves finality, which
comes before the `vec` and is `!` for false and `+` for true. The program section
ends with `.`, followed by a Julia serialization of the Dict.  We'll need to figure
out how to handle functions, the answer is **not** using eval on live Julia code, in
the Dict we encode them as `missing` and there's some technique to reload them at runtime.

### Back references: Mark and Check

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

- `dialect`: For specifying dialects. I simply suspect this will be useful

- `regex`: I don't see any reason we can't interpret PCRE regex format on the VM

- `canonical`  A string form any dialect may be transformed to, and any
               Pattern printed as. Needs to be feature complete, rather than
               pleasant to look at or write, but must be legible. This is
               necessary for, among other things:

### Grammar Transforming Dialects

I dunno how this works actually, but the example I have in mind transforms grammars
into highlighters, so it's a DSL to specify StyledStrings annotations to perform on a
recognized grammar.

### Expr parser

This is something I wanted to do in Lua, Julia having a canonical Expr format makes
the idea bearable, but this is a separate VM, or program really, which reparses
changes to a parser Expr to see when they remain valid to the specified Grammar.
Crucial for getting rid of Treesitter.
