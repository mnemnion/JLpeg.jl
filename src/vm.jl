# JLpeg virtual machine

"A frame of the instruction stack."
struct StackFrame
    i::Int32   # Instruction pointer
    s::UInt32  # String index
    c::UInt32  # Capture level
    p::Bool    # Predicate status
end

"An entry in the capture stack"
struct CapEntry
    s::UInt32      # String index
    inst::CaptureInst
    CapEntry(s::UInt32, inst::CaptureInst) = new(s, inst)
end

"""
    VMState(patt::Pattern, string::AbstractString)

Contains the state of a match on `subject` by `program`.

# Fields

Other than the `subject` and `program`, we have:

- `top`: the byte length of `subject`
- `i`: Instruction counter
- `s`: Subject pointer
- `ti`, `ts`, `tc`, `tp`: Stack top registers
- `t_on`: flag for nonempty stack
- `inpred`: `Bool` is `true` inside predicates (`PAnd`, `PNot`)
- `sfar`: furthest subject pointer we've failed at, for error reporting
- `failtag`: tag for a labeled failure
- `stack`: Contains stack frames for calls and backtracks
- `cap`: A stack for captures
- `running`: `Bool` which is `true` when the VM is executing
- `matched`: true if we matched the program on the subject

##  Implementation

Classic dispatch-driven VM with a program counter, opcodes, stack
frames.  I borrowed a page from Forth and made the top of the
stack a register, allowing stack frames to be immutable while still
permitting the PartialCommit optimization.
"""
mutable struct VMState
   subject::AbstractString # The subject we're parsing
   program::IVector  # Our program
   patt::Pattern     # The pattern it's derived from
   top::UInt32       # Byte length of subject string
   # Registers
   i::Int32          # Instruction Counter
   s::UInt32         # Subject pointer
   ti::Int32         # Stack top instruction register
   ts::UInt32        # Stack top subject register
   tc::UInt32        # Stack top capture level register
   tp::Bool          # Stack top predicate register
   t_on::Bool        # Is there a frame on the stack?
   inpred::Bool      # Are we inside a predicate?
   sfar::UInt32      # Farthest subject pointer we've failed at
   failtag::UInt16   # Labeled failure tag
   stack::Vector{StackFrame}  # Stack of Instruction offsets
   cap::Vector{CapEntry}
   running::Bool
   matched::Bool
   function VMState(patt::Pattern, subject::AbstractString)
      program = prepare!(patt).code
      stack = sizehint!(Vector{StackFrame}(undef, 0), 64)
      cap   = Vector{CapEntry}(undef, 0)
      top = ncodeunits(subject)
      return new(subject, program, patt, top, 1, 1, 0, 0, 0, false, false, false, 1, 0, stack, cap, false, false)
   end
end

# ## VM Actions
#
# All functions which mutate a running VM state other than instruction dispatch.

@inline
"Push a full frame onto the stack."
function pushframe!(vm::VMState, i::Int32, s::UInt32)
    if !vm.t_on
        vm.ti, vm.ts, vm.tc, vm.tp = i, s, lcap(vm), vm.inpred
        vm.t_on = true
    else
        frame = StackFrame(vm.ti, vm.ts, vm.tc, vm.tp)
        vm.ti, vm.ts, vm.tc, vm.tp = i, s, lcap(vm), vm.inpred
        push!(vm.stack, frame)
    end
end


@inline
"Push a call onto the stack."
function pushcall!(vm::VMState)
    if !vm.t_on
       vm.ti = vm.i + 1
       vm.t_on = true
    else
        frame = StackFrame(vm.ti, vm.ts, vm.tc, vm.tp)
        vm.ti, vm.ts, vm.tc, vm.tp = vm.i + 1, 0, 0, false
        push!(vm.stack, frame)
    end
end

@inline
"Pop a stack frame. Returns a tuple (i, s, c, p)"
function popframe!(vm::VMState)::Tuple{Union{Int32,Nothing},UInt32,UInt32,Bool}
    if !vm.t_on
        return nothing, UInt32(0), UInt32(0), false
    end
    if isempty(vm.stack)
        vm.t_on = false
        return vm.ti, vm.ts, vm.tc, vm.tp
    end
    frame = pop!(vm.stack)
    _ti, _ts, _tc, _tp = vm.ti, vm.ts, vm.tc, vm.tp
    vm.ti, vm.ts, vm.tc, vm.tp = frame.i, frame.s, frame.c, frame.p
    return _ti, _ts, _tc, _tp
end

@inline
"Update the top stack frame."
function updatetop_s!(vm::VMState)
    vm.ts = vm.s
    vm.tc = lcap(vm)
end

@inline
"Length/height of the capture stack."
function lcap(vm::VMState)
    return UInt16(length(vm.cap))
end

@inline
"Trim the capture stack height to `c`."
function trimcap!(vm::VMState, c::UInt32)
    while lcap(vm) > c
        pop!(vm.cap)
    end
    return
end

@inline
"Push a CapEntry."
function pushcap!(vm::VMState, inst::Instruction)
   push!(vm.cap, CapEntry(vm.s, inst))
end

@inline
"Return the char at `vm.s`."
function thischar(vm::VMState)
    if vm.s > vm.top
        return nothing
    end
    vm.subject[vm.s]
end

@inline
"Return the byte at `vm.s`."
function thisbyte(vm::VMState)
    if vm.s > vm.top
        nothing
    else
        codeunit(vm.subject, vm.s)
    end
end

@inline
"""
    updatesfar!(vm)

Updates the farthest-fail register if greater than before.
Also sets the fail tag to 0, since throws set sfar directly,
and this is the condition of all other failures
"""
function updatesfar!(vm)
    vm.failtag = 0
    if vm.s > vm.sfar
        vm.sfar = vm.s
    end
end

@inline
"Unwind the stacks on a match failure"
function failmatch!(vm::VMState)
    if !vm.t_on
        vm.running = false
        vm.matched = false
        return
    end
    i, s, c, p = popframe!(vm)
    while s == 0 # return from calls
        i, s, c, p = popframe!(vm)
        if i === nothing break end
    end # until we find a choice frame or exhaust the stack
    if i === nothing
        vm.running = false
        vm.matched = false
    else
        vm.s = s::UInt32
        vm.i = i
        vm.inpred = p
        trimcap!(vm, c::UInt32)
    end
end

# ## VM core and instructions
#
# The core execution loop and all dispatched instructions.

"""
    runvm!(vm::VMState)::Bool

Run a vm. A classic instruction-dispatch loop, relying on Julia's excellent
method specialization to provide speed.  Once I have some representative patterns
and sufficiently weighty test data, I may try swapping in a Vector of function
pointers to see if the code generated from this approach is, in fact, optimal.
"""
function runvm!(vm::VMState)::Bool
    vm.running = true
    while vm.running
        inst::Instruction = @inbounds vm.program[vm.i]
        # print(vm_to_str(vm))
        if !onInst(inst, vm)::Bool
            failmatch!(vm)
        end
    end
    vm.matched
end


"""
    onInst(inst::Instruction, vm::VMState)::Bool

Dispatch an instruction by structure, with match statements by opcode for
further dispatch. Returns `false` if the instruction fails, otherwise `true`,
in which case the dispatch function is expected to have altered the VM state.
"""
function onInst(inst::Instruction, ::VMState)::Bool
    error("unrecognized instruction $inst")
    false
end

function onInst(inst::HoldInst, ::VMState)::Bool
    @error "HoldInstruction left in code, op $(inst.op)"
    false
end

"onAny"
function onInst(any::AnyInst, vm::VMState)::Bool
    if vm.s > vm.top
        updatesfar!(vm)
        return false
    end
    idx = vm.s
    for i in any.n:-1:1
        idx = nextind(vm.subject, idx)
        if idx > vm.top && i > 1
            updatesfar!(vm)
            return false
        end
    end
    vm.s = idx
    vm.i += 1
    return true
end

"onChar"
function onInst(inst::CharInst, vm::VMState)::Bool
    this = thischar(vm)
    if this === nothing
        updatesfar!(vm)
        return false
    end
    if this == inst.c
        vm.i += 1
        vm.s = nextind(vm.subject, vm.s)
        return true
    else
        updatesfar!(vm)
        return false
    end
end

"onTestChar"
function onInst(inst::TestCharInst, vm::VMState)::Bool
    this = thischar(vm)
    if this == inst.c
        vm.i += 1
        return true
    else  # this includes the this === nothing case
        vm.i += inst.l
        return true  # Not an unwinding fail
    end
end

"onNotChar"
function onInst(inst::NotCharInst, vm::VMState)::Bool
    this = thischar(vm)
    if this == inst.c
        updatesfar!(vm)
        return false
    else  # this includes the this === nothing case
        vm.i += 1
        return true  # Not an unwinding fail
    end
end

"onBehind"
function onInst(inst::BehindInst, vm::VMState)::Bool
    s = vm.s
    for _ = inst.n:-1:1
        s = prevind(vm.subject, s)
        if s == 0
            break
        end
    end
    if s == 0
        updatesfar!(vm)
        return false
    else
        vm.s = s
        vm.i += 1
        return true
    end
end

"onSet"
function onInst(inst::SetInst, vm::VMState)::Bool
    match = false
    byte = thisbyte(vm)
    if byte !== nothing
        if byte < 0x80 && inst[byte + 1]
            vm.s += 1
            match = true
        end
    end
    if !match
        updatesfar!(vm)
        vm.i += 1
        return inst.op == ISet ? false : inst.op == ILeadSet ? true : error("weird opcode in onSet"); false
    else
        vm.i += inst.l
        return true
    end
end

"onTestSet"
function onInst(inst::TestSetInst, vm::VMState)::Bool
    # TODO add, test, then refactor with thisbyte(vm)
    this = thischar(vm)
    if this === nothing
        vm.i += 1
        return true
    end
    code = UInt32(this)
    if code < 128 && @inbounds inst[code + 1]
        vm.i += 1
        return true
    else
        vm.i += inst.l
        return true  # Still not an unwinding fail
    end
end

"onNotSet"
function onInst(inst::NotSetInst, vm::VMState)::Bool
    nomatch = true
    this = thischar(vm)
    if this !== nothing
        code = UInt32(this)
        if code < 128 && inst[code + 1]
            nomatch = false
        end
    end
    if !nomatch
        updatesfar!(vm)
        vm.i += 1    # NOTE this depends on only two kinds of SetInst!
        return false
    else
        vm.i += inst.l
        return true
    end
end

"onByte"
function onInst(inst::ByteInst, vm::VMState)::Bool
    byte = thisbyte(vm)
    if byte === nothing
        updatesfar!(vm)
        return false
    end
    if byte == inst.b
        vm.i += inst.l
        vm.s += 1
        return true
    else
        vm.i += 1
        return true
    end
end

"onMultiVec"
function onInst(inst::MultiVecInst, vm::VMState)::Bool
    if vm.s ≤ vm.top
        byte = thisbyte(vm)::UInt8
        # check in valid continuation byte range
        if 0b10000000 ≤ byte ≤ 0b10111111
            # Mask high bit, + 1 for Julia indexing
            if @inbounds inst[(byte & 0b00111111) + UInt8(1)]
                vm.i += inst.l
                vm.s += 1
                return true
            end
        end
    end
    updatesfar!(vm)
    return false
end

"onLeadMulti"
function onInst(inst::LeadMultiInst, vm::VMState)::Bool
    byte = thisbyte(vm)
    if byte === nothing
        updatesfar!(vm)
        return false
    end
    # Must check for 0b11xxxxxx or false positives from malformed data
    if (byte & 0xc0 == 0xc0) && @inbounds inst[(byte & 0b00111111) + UInt8(1)]
        vm.i += 1
        return true
    else  # goto fail
        vm.i += inst.l
        return true
    end
end

"onCapture"
function onInst(inst::CaptureInst, vm::VMState)::Bool
    pushcap!(vm, inst)
    vm.i += 1
    return true
end

"onChoice"
function onInst(inst::ChoiceInst, vm::VMState)
    pushframe!(vm, vm.i + inst.l, vm.s)
    vm.i += 1
    return true
end

"onThrow"
function onInst(inst::ThrowInst, vm::VMState)
    if !vm.inpred
        vm.failtag = inst.tag
        vm.sfar = vm.s
        return false
    else
        return unwindpred!(vm)
    end
end

"onThrowRec"
function onInst(inst::ThrowRecInst, vm::VMState)
    if !vm.inpred
        vm.failtag = inst.tag
        pushcall!(vm)
        vm.i += inst.l
        return true
    else
        return unwindpred!(vm)
    end
end

@inline
"Unwind a throw inside a predicate"
function unwindpred!(vm::VMState)
    # Unwind until above predicate
    # Which we can check with the stack instruction register,
    # Conveniently enough
    while vm.program[vm.ti].op ≠ IPredChoice
        popframe!(vm)
    end # until we've left the predicate on the stack
    vm.failtag = 0
    vm.sfar = vm.s
    return false  # This handles .inpred (we could be in more than one)
end

function onInst(inst::LabelInst, vm::VMState)
    if inst.op == ICommit            return onCommit(inst, vm)
    elseif inst.op == IJump          return onJump(inst, vm)
    elseif inst.op == ICall          return onCall(inst, vm)
    elseif inst.op == IPredChoice    return onPredChoice(inst, vm)
    elseif inst.op == IPartialCommit return onPartialCommit(inst, vm)
    elseif inst.op == IBackCommit    return onBackCommit(inst, vm)
    end
end

function onInst(inst::MereInst, vm::VMState)
    if inst.op == INoOp
        vm.i += 1; return true
    elseif inst.op == IEnd       return onEnd(vm)
    elseif inst.op == IReturn    return onReturn(vm)
    elseif inst.op == IFail      return onFail(vm)
    elseif inst.op == IFailTwice return onFailTwice(vm)
    end
end

@inline
function onCommit(inst::LabelInst, vm::VMState)
    popframe!(vm)
    vm.i += inst.l
    return true
end

@inline
function onJump(inst::LabelInst, vm::VMState)
    vm.i += inst.l
    return true
end

@inline
function onCall(inst::LabelInst, vm::VMState)
    pushcall!(vm)
    vm.i += inst.l
    return true
end

@inline
function onPredChoice(inst::LabelInst, vm::VMState)
    pushframe!(vm, vm.i + inst.l, vm.s)
    vm.inpred = true
    vm.i += 1
    return true
end

@inline
function onEnd(vm::VMState)
    @assert !vm.t_on "hit end instruction with $(length(vm.stack) + 1) on stack:\n$(vm_to_str(vm))"
    vm.running = false
    vm.matched = true
    return true
end

@inline
function onReturn(vm::VMState)
    i, s = popframe!(vm)
    @assert s == 0 "rule left a choice frame on the stack:\n$(repr(vm))"
    vm.i = i
    return true
end

@inline
function onPartialCommit(inst::LabelInst, vm::VMState)
    updatetop_s!(vm)
    vm.i += inst.l
    return true
end

@inline
function onBackCommit(inst::LabelInst, vm::VMState)
    _, s, c, p = popframe!(vm)
    vm.i += inst.l
    vm.s = s
    vm.inpred = p
    trimcap!(vm, c)
    return true
end

@inline
function onFail(vm::VMState)
    updatesfar!(vm)
    return false
end

@inline
function onFailTwice(vm::VMState)
    popframe!(vm)
    updatesfar!(vm)
    return false
end

"""
    oncapmatch(vm::VMState)

Process the capture list and return what we find.
"""
function aftermatch(vm::VMState)::PegMatch
    function _substr(s, f)
        # s should always be a valid index, f might not be,
        # and is in all cases one past what we need
        f1 = prevind(vm.subject, f)
        return @views (vm.subject[s:f1])
    end
    last = prevind(vm.subject, vm.s)
    captures = PegCapture()
    offsets = PegOffset()
    patt = vm.patt
    # there may not be any captures, in which case the whole
    # matched string is the capture:
    if lcap(vm) == 0
        push!(captures, @views vm.subject[1:last])
        push!(offsets, 1)
        return PegMatch(vm.subject, last, captures, offsets, patt)
    end
    # Otherwise:
    # To handle nested captures of all sorts we use a stack
    capstack = []
    # And another stack for grouping captures
    groupstack = []
    capsdict = patt.aux[:caps]
    for i in 1:lcap(vm)
        cap = vm.cap[i]
        if cap.inst.op == IOpenCapture
            push!(capstack, cap)
            if cap.inst.kind == Cgroup
                # Push our current captures and offsets onto the group stack
                push!(groupstack, (captures, offsets))
                captures, offsets = PegCapture(), PegOffset()
            end
            continue
        end
        if cap.inst.op == IFullCapture
            # Make a synthetic back capture, reusing this Instruction
            # The only distinct value of bcap we use is .s,
            # Which we calculate thus:
            bcap = CapEntry(cap.s + cap.inst.l, cap.inst)
        elseif cap.inst.op == ICloseCapture
            bcap = pop!(capstack)
        end
        ikey = cap.inst
        key = capsdict[cap.inst.tag]
        if bcap.inst.kind == ikey.kind
            # TODO if there are non-capturing captures (possible), we
            # check for those here.  I think the rule is a capture gets an offset.
            push!(offsets, Int(bcap.s))
            if ikey.kind == Csimple
                push!(captures, _substr(bcap.s, cap.s))
            elseif ikey.kind == Csymbol
                sub = _substr(bcap.s, cap.s)
                push!(captures, key => sub)
            elseif ikey.kind == Cgroup
                # grab the outer captures and offsets
                caps, offs = pop!(groupstack)
                if isempty(captures)  # the group is the capture
                    push!(captures, _substr(bcap.s, cap.s))
                    push!(offsets, bcap.s)
                    if key !== nothing
                        push!(caps, key => captures)
                    else
                        push!(caps, captures)
                    end
                    push!(offs, offsets)
                    captures, offsets = caps, offs
                    continue
                end
                if key !== nothing
                    push!(caps, key => captures)
                else
                    push!(caps, captures)
                end
                push!(offs, offsets)
                captures, offsets = caps, offs
            elseif ikey.kind == Cposition
                push!(captures, _substr(bcap.s, cap.s))
            elseif ikey.kind == Crange
                if key !== nothing
                    push!(captures, key => [bcap.s:prevind(vm.subject, cap.s)])
                else
                    push!(captures, [bcap.s:prevind(vm.subject, cap.s)])
                end
            elseif ikey.kind == Cconst
                push!(captures, key)
            elseif ikey.kind == Caction
                λ = key::Function
                # The Action either created the group, or it *is* the group
                if ikey.op == IFullCapture || isempty(captures)
                    arg = _substr(bcap.s, cap.s)
                    if isempty(captures) && isempty(groupstack)
                        push!(captures, λ(arg))
                    elseif !isempty(captures)
                        # A FullCapture inside another group to be closed later
                        push!(captures, λ(arg))
                    elseif isempty(captures) && !isempty(groupstack)
                        # We had to use an OpenCap Action, captures is discarded
                        # but we need the offset we just put in it
                        off = offsets[end]
                        captures, offsets = pop!(groupstack)
                        push!(offsets, off)
                        push!(captures, λ(arg))
                    end
                else  # all of captures is our arguments
                    args = captures
                    off = offsets[1]
                    if isempty(groupstack)
                        captures, offsets = PegCapture(), PegOffset()
                    else
                        captures, offsets = pop!(groupstack)
                    end
                    push!(offsets, off)
                    push!(captures, λ(args...))
                end
                # remove `nothing` matches
                if captures[end] === nothing
                    pop!(captures)
                    pop!(offsets)
                end
            else
                @warn "doesn't handle the case of $(ikey.kind) yet!"
                # Keep the offsets correct:
                push!(captures, :__not_found_capture__ => "")
            end
        else
            error("Unbalanced caps begin $(bcap.inst.kind) end $(cap.inst.kind)")
        end
    end
    if !isempty(capstack)
        @warn "left entries on the capture stack: $(capstack)"
    end
    return PegMatch(vm.subject, last, captures, offsets, patt)
end

function afterfail(vm::VMState)::PegFail
    if vm.failtag == 0
        return PegFail(vm.subject, vm.sfar, :default)
    end
    throws = vm.patt.aux[:throws]
    return PegFail(vm.subject, vm.sfar, throws[vm.failtag])
end

## Core Method extensions


"""
    match(patt::Pattern, subject::AbstractString)::Union{PegMatch, PegFail}

Match `patt` to `subject`, returning a `PegMatch` implementing the expected interface
for its supertype `AbstractMatch`, or a `PegFail` with useful information about the
failure.
"""
function Base.match(patt::Pattern, subject::AbstractString)::Union{PegMatch, PegFail}
    vm = VMState(patt, subject)
    runvm!(vm) ? aftermatch(vm) : afterfail(vm)
end


"""
    findfirst(patt::Pattern, string::AbstractString)::Union{Integer, Nothing}

Find the first match of `patt` in `string`. Returns the index at the *end* of the match,
such that string[1:findfirst(patt)] will show the substring.
"""
function Base.findfirst(patt::Pattern, string::AbstractString)::Union{Integer, Nothing}
    vm = VMState(patt, string)
    runvm!(vm) ? vm.s - 1 : nothing
end

"""
    occursin(needle::Pattern, haystack::AbstractString)

Check if `needle` matches in `haystack`.  PEG patterns, unlike regex, must match
from the first character in the string; to convert a pattern `p` to match anywhere,
use `psearch = "" >> p`.
"""
function Base.occursin(needle::Pattern, haystack::AbstractString)
    findfirst(needle, haystack) !== nothing ? true : false
end

