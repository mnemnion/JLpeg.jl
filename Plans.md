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
- [#] Multibyte sets and chars
  - [X] Implement multibyte sets
  - [ ] Fix bug with emoji 🫠
- [ ] `B(patt)` (prerequisite: determining fixed-length patterns)
- [ ] `T(:label)` somewhat hefty VM refactor here.
  - [ ] `PegFail` object with test conversion.
    - [ ] default label is `:default`.
- [X] Captures
- [ ] Mark / Check
- [ ] detect "loop may accept empty string" such as `a = (!S"'")^0`
- [ ] Optimizations from The Book (paper and/or lpeg C code):
  - [ ] TestPatt optimizations
  - [ ] Tail call elimination
  - [ ] Set span optimization
  - [ ] Capture-closing optimization
- [ ] Serializing and loading grammars
- [ ] CaptureCommitInst: it's a commit which create a full capture from its paired Choice.

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

#### BIG PROBLEM (solvable)

With doing this, is that we use a Dict keyed on the actual CloseInstruction to find
capture actions.  We can solve this and get something better in the process.

The basic idea is that `prepare` looks through the pattern code for closing captures
(ICloseCapture and IFullCapture), all capture-type instructions now have a `.tag`
field which is initialized to `0`.  All such instructions are monotonically replaced
with the next tag in order, and a Vector is prepared containing the reference values
from the `:caps` dict, added to `aux`. This simplifies serialization as well because
we now have a Vector of cap actions instead of a Dict. PCompiled rules are still
portable provided that hoisting reconstructs the `:caps` dict.

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
