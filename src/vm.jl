# JLpeg virtual machine

"A frame of the instruction stack."
struct StackFrame
    i::Int32   # Instruction pointer
    s::UInt32  # String index
    c::UInt32  # Capture level
    m::UInt16  # Mark level
               # One byte left. Left recursion?
    p::Bool    # Predicate status
end

"An entry in the capture stack"
struct CapFrame
    s::UInt32      # String index
    inst::CaptureInst
    CapFrame(s::UInt32, inst::CaptureInst) = new(s, inst)
end

"A frame of the mark stack"
struct MarkFrame
    s::UInt32
    off::UInt16
    tag::UInt16
end

"""
    VMState(patt::Pattern, subject::AbstractString)

Contains the state of a match on `subject` by `program`.

# Fields

Other than the `subject`, `program`, and `patt`, we have:

- `top`: the byte length of `subject`
- `i`: Instruction counter
- `s`: Subject pointer
- `ti`, `ts`, `tc`, `tm`, `tp`: Stack top registers
- `t_on`: flag for nonempty stack
- `running`: `Bool` which is `true` when the VM is executing
- `matched`: true if we matched the program on the subject
- `inpred`: `Bool` is `true` inside predicates (`PAnd`, `PNot`)
- `sfar`: furthest subject pointer we've failed at, for error reporting
- `failtag`: tag for a labeled failure
- `stack`: Contains stack frames for calls and backtracks
- `cap`: A stack for captures
- `mark`: A stack for marks

##  Implementation

Classic dispatch-driven VM with a program counter, opcodes, stack
frames.  I borrowed a page from Forth and made the top of the
stack a register, allowing stack frames to be immutable while still
permitting the PartialCommit optimization.
"""
mutable struct VMState{S <: AbstractString}
   subject::S        # The subject we're parsing
   program::IVector  # Our program
   patt::Pattern     # The pattern it's derived from
   top::UInt32       # Byte length of subject string
   # Registers
   i::Int32          # Instruction Counter
   s::UInt32         # Subject pointer
   ti::Int32         # Stack top instruction register
   ts::UInt32        # Stack top subject register
   tc::UInt32        # Stack top capture level register
   tm::UInt16        # Stack top mark level register
   tp::Bool          # Stack top predicate register
   # State
   running::Bool     # Is the VM running?
   matched::Bool     # Has the pattern matched the subject?
   t_on::Bool        # Is there a frame on the stack?
   inpred::Bool      # Are we inside a predicate?
   mo::UInt32        # Mark opening subject pointer
   sfar::UInt32      # Farthest subject pointer we've failed at
   failtag::UInt16   # Labeled failure tag
   # Stacks
   stack::Vector{StackFrame}  # Stack of Instruction offsets
   cap::Vector{CapFrame}  # Capture stack
   mark::Vector{MarkFrame} # Mark stack
   function VMState(patt::Pattern, subject::S, s::Integer, top::Integer) where {S <: AbstractString}
      program = compile!(patt).code
      stack = sizehint!(Vector{StackFrame}(undef, 0), 64)
      cap   = Vector{CapFrame}()
      mark  = Vector{MarkFrame}()
      return new{S}(subject, program, patt, top, 1, s, 0, 0, 0, 0, false, false, false, false, false, 0, 1, 0, stack, cap, mark)
   end
   function VMState(patt::Pattern, subject::S) where {S <: AbstractString}
        top = ncodeunits(subject)
        VMState(patt, subject, 1, top)
    end
end

# ## VM Actions
#
# All functions which mutate a running VM state other than instruction dispatch.

@inline
"Push a full frame onto the stack."
function pushframe!(vm::VMState, i::Int32, s::UInt32)
    if !vm.t_on
        vm.ti, vm.ts, vm.tc, vm.tm, vm.tp = i, s, lcap(vm), mcap(vm), vm.inpred
        vm.t_on = true
    else
        frame = StackFrame(vm.ti, vm.ts, vm.tc, vm.tm, vm.tp)
        vm.ti, vm.ts, vm.tc, vm.tm, vm.tp = i, s, lcap(vm), mcap(vm), vm.inpred
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
        frame = StackFrame(vm.ti, vm.ts, vm.tc, vm.tm, vm.tp)
        vm.ti, vm.ts, vm.tc, vm.tm, vm.tp = vm.i + 1, 0, 0, 0, false
        push!(vm.stack, frame)
    end
end

@inline
"Pop a stack frame. Returns a tuple (i, s, c, p)"
function popframe!(vm::VMState)::Tuple{Union{Int32,Nothing},UInt32,UInt32,UInt16,Bool}
    if !vm.t_on
        return nothing, 0x00000000, 0x00000000, 0x0000, false
    end
    if isempty(vm.stack)
        vm.t_on = false
        return vm.ti, vm.ts, vm.tc, vm.tm, vm.tp
    end
    frame = pop!(vm.stack)
    _ti, _ts, _tc, _tm, _tp = vm.ti, vm.ts, vm.tc, vm.tm, vm.tp
    vm.ti, vm.ts, vm.tc, vm.tm, vm.tp = frame.i, frame.s, frame.c, frame.m, frame.p
    return _ti, _ts, _tc, _tm, _tp
end

@inline
"Drop a call stack frame."
function dropframe!(vm::VMState)::Bool
    if !vm.t_on
        return false
    end
    if isempty(vm.stack)
        vm.t_on = false
        return true
    end
    frame = pop!(vm.stack)
    vm.ti, vm.ts, vm.tc, vm.tm, vm.tp = frame.i, frame.s, frame.c, frame.m, frame.p
    return true
end

@inline
"Update the top stack frame."
function updatetop_s!(vm::VMState)
    vm.ts = vm.s
    vm.tc = lcap(vm)
end

@inline
"Height of the capture stack."
function lcap(vm::VMState)::UInt16
    return length(vm.cap)
end

@inline
"Height of the mark stack"
function mcap(vm::VMState)::UInt16
    return length(vm.mark)
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
"Trim the mark stack height to `m`"
function trimmark!(vm::VMState, m::UInt16)
    while mcap(vm) > m
        pop!(vm.mark)
    end
    return
end

@inline
"Push a CapFrame."
function pushcap!(vm::VMState, inst::Instruction)
    if inst.op == ICloseCapture
        top = last(vm.cap)
        if top.inst.op == IOpenCapture
            @assert inst.tag == top.inst.tag "mismatched tags in capture push"
            vm.cap[end] = CapFrame(vm.s, FullCaptureInst(inst.kind, vm.s - top.s, inst.tag))
        else
            push!(vm.cap, CapFrame(vm.s, inst))
        end
    else
        push!(vm.cap, CapFrame(vm.s, inst))
    end
end

@inline
"Push a MarkFrame."
function pushmark!(vm::VMState, off::UInt16, inst::Instruction)
    push!(vm.mark, MarkFrame(vm.mo, off, inst.tag))
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

If this is the farthest point at which we've failed, update the farthest-fail
point, and set the failtag to 0.
"""
function updatesfar!(vm)
    if vm.s > vm.sfar
        vm.failtag = 0
        vm.sfar = vm.s
    end
end

@inline
"Unwind the stacks on a match failure"
function failmatch!(vm::VMState)
    while vm.ts == 0 && dropframe!(vm)
        # pop until we have a subject pointer, or exhaust stack
    end
    if !vm.t_on
        vm.running = false
        vm.matched = false
    else
        i, s, c, m, p = popframe!(vm)
        vm.s = s::UInt32
        vm.i = i
        vm.inpred = p
        trimcap!(vm, c::UInt32)
        trimmark!(vm, m::UInt16)
    end
end

@inline
"Unwind a throw inside a predicate"
function unwindpred!(vm::VMState)
    # Unwind until above predicate
    # Which we can check with the stack instruction register,
    # Conveniently enough
    while vm.program[vm.ti].op ≠ IPredChoice
        dropframe!(vm)
    end # until we've left the predicate on the stack
    vm.failtag = 0
    vm.sfar = vm.s
    return false  # This handles .inpred (we could be in more than one)
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
    vm.i += 1
    if any.n == 1
        vm.s = nextind(vm.subject, vm.s)
        return true
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
    return true
end

"onChar"
function onInst(inst::CharInst, vm::VMState)::Bool
    s, match = @inline nchars(inst, vm)
    if match
        vm.i += 1
        vm.s = s
        return true
    else
        updatesfar!(vm)
        return false
    end
end

"onTestChar"
function onInst(inst::TestCharInst, vm::VMState)::Bool
    _, match = @inline nchars(inst, vm)
    if match
        vm.i += 1
        return true
    else
        vm.i += inst.l
        return true  # Not an unwinding fail
    end
end

"onNotChar"
function onInst(inst::NotCharInst, vm::VMState)::Bool
    _, match = @inline nchars(inst, vm)
    if match
        updatesfar!(vm)
        return false
    else  # predicate, doesn't advance subject pointer
        vm.i += 1
        return true
    end
end

@inline
function nchars(inst::Instruction, vm::VMState)::Tuple{UInt32,Bool}
    s = vm.s
    top = vm.top
    n = inst.nchar
    if s + n - 1 > top
        return s, false
    end
    subject = vm.subject
    this = codeunit(subject, s)
    match = inst.one === this
    !match && return s, false
    if n === 0x02
        s += 1
        match &= inst.two === codeunit(subject, s)
    elseif n === 0x03
        s += 1
        match &= inst.two === codeunit(subject, s)
        s += 1
        match &= inst.three === codeunit(subject, s)
    elseif n === 0x04
        s += 1
        match &= inst.two === codeunit(subject, s)
        s += 1
        match &= inst.three === codeunit(subject, s)
        s += 1
        match &= inst.four === codeunit(subject, s)
    end
    return s + 1, match
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
        if byte < 0x40 && vm.program[vm.i+1][byte + 1]
            vm.s += 1
            match = true
        elseif 0x40 ≤ byte < 0x80 && vm.program[vm.i+2][(byte & 0b00111111) + 1]
            vm.s += 1
            match = true
        end
    end
    if !match
        if inst.op == ISet
            updatesfar!(vm)
            return false
        elseif inst.op == ILeadSet
            vm.i += 3
            return true
        else
            error("unrecognized set opcode $(inst.op)")
            return false
        end
    else
        vm.i += inst.l
        return true
    end
end

"onTestSet"
function onInst(inst::TestSetInst, vm::VMState)::Bool
    match = false
    byte = thisbyte(vm)
    if byte !== nothing
        if byte < 0x40 && vm.program[vm.i+1][byte + 1]
            vm.s += 1
            match = true
        elseif 0x40 ≤ byte < 0x80 && vm.program[vm.i+2][(byte & 0b00111111) + 1]
            vm.s += 1
            match = true
        end
    end
    if !match
        vm.i += 3
        return true
    else
        vm.i += inst.l
        return true
    end
end

"onNotSet"
function onInst(inst::NotSetInst, vm::VMState)::Bool
    nomatch = true
    byte = thisbyte(vm)
    if byte !== nothing
        if byte < 0x40 && vm.program[vm.i+1][byte + 1]
            nomatch = false
        elseif 0x40 ≤ byte < 0x80 && vm.program[vm.i+2][(byte & 0b00111111) + 1]
            nomatch = false
        end
    end
    if !nomatch
        updatesfar!(vm)
        vm.i += 3
        return false
    else
        vm.i += inst.l
        return true
    end
end

"onLeadByte"
function onInst(inst::LeadByteInst, vm::VMState)::Bool
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
        mask = vm.program[vm.i + 1]
        # check in valid continuation byte range
        if 0b10000000 ≤ byte ≤ 0b10111111
            # Mask high bit, + 1 for Julia indexing
            if @inbounds mask[(byte & 0b00111111) + UInt8(1)]
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
    mask = vm.program[vm.i + 1]
    # Must check for 0b11xxxxxx or false positives from malformed data
    if (byte & 0xc0 == 0xc0) && @inbounds mask[(byte & 0b00111111) + UInt8(1)]
        vm.i += 2
        return true
    else  # goto fail
        vm.i += inst.l
        return true
    end
end

"onCapture"
function onInst(inst::CaptureInst, vm::VMState)::Bool
    vm.i += 1
    if inst.op == ICloseRunTime
        return onCloseRunTime(inst, vm)
    end
    pushcap!(vm, inst)
    return true
end

function onCloseRunTime(inst::CaptureInst, vm::VMState)::Bool
    if vm.cap[end].inst.tag ≠ inst.tag
        throw(PegError("query actions may not contain captures"))
    end
    open = pop!(vm.cap)
    λ = vm.patt.aux[:caps][inst.tag]
    ret::Bool = false
    if inst.kind == Cvm
        ret = λ(vm)
        return ret
    elseif inst.kind == Ctest
        span = @views vm.subject[open.s:vm.s-1]
        ret = λ(span)
        return ret
    else
        error("unsupported runtime action kind: $(inst.kind)")
        return ret
    end
end

"onOpenMark"
function onInst(::OpenMarkInst, vm::VMState)::Bool
    vm.mo = vm.s
    vm.i += 1
    return true
end

"onCloseMark"
function onInst(inst::CloseMarkInst, vm::VMState)::Bool
   off = UInt16(vm.s - vm.mo)
   pushmark!(vm, off, inst)
   vm.i += 1
   return true
end

"onCheckMark"
function onInst(inst::CheckMarkInst, vm::VMState)::Bool
    vm.i += 1  # we advance unconditionally in this instruction
    idx = findlast(vm.mark) do i
        i.tag == inst.tag
    end
    if inst.check == 0x0004  # :always
        if idx !== nothing
            deleteat!(vm.mark, idx)
        end
        return true
    end
    # All other checks fail if there is no mark:
    if idx === nothing
        updatesfar!(vm)
        return false
    end
    # Built-ins
    mark = vm.mark[idx]
    start1, stop1 = mark.s, mark.s + mark.off - 1
    start2, stop2 = vm.mo, vm.s - 1
    matched::Bool = false
    if inst.check == 0x0001  # aka :(==)
        if stop1 - start1 == stop2 - start2
            Δ = start2 - start1
            matched = true
            for i = start1:stop1
                if codeunit(vm.subject, i) ≠ codeunit(vm.subject, i + Δ)
                    matched = false
                    break
                end
            end
        end
    # :length
    elseif inst.check == 0x0002 && stop1 - start1 == stop2 - start2
        matched = true
    # :close
    elseif inst.check == 0x0003
        matched = true
    # :gt (handled 0x0004 at top)
    elseif inst.check == 0x0005 && stop2 - start2 > stop1 - start1
        matched = true
    # :lt
    elseif inst.check == 0x0006 && stop2 - start2 < stop1 - start1
        matched = true
    # :gte
    elseif inst.check == 0x0007 && stop2 - start2 ≥ stop1 - start1
        matched = true
    # :lte
    elseif inst.check == 0x0008 && stop2 - start2 ≤ stop1 - start1
        matched = true
    elseif inst.check ≥ 0x0009  # user-provided function
        λ = vm.patt.aux[:checks][inst.check]
        sub1 = @views vm.subject[start1:stop1]
        sub2 = @views vm.subject[start2:stop2]
        matched = λ(sub1, sub2)
    end

    if matched
        if !vm.inpred
            deleteat!(vm.mark, idx)
        end
    else
        updatesfar!(vm)
    end
    return matched
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
    _, s, c, m, p = popframe!(vm)
    vm.i += inst.l
    vm.s = s
    vm.inpred = p
    trimcap!(vm, c)
    trimmark!(vm, m)
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

"Catch OpenCallInst"
function onInst(::OpenCallInst, ::VMState)
    throw(PegError("Undefined rule while matching Pattern"))
    return false
end

"""
    oncapmatch(vm::VMState, start=1)::PegMatch

Process the capture list and return what we find.
"""
function aftermatch(vm::VMState, start::Integer=1)::PegMatch
    function _substr(s, f)
        # s should always be a valid index, f might not be,
        # and is in all cases one past what we need
        f1 = prevind(vm.subject, f)
        return @views (vm.subject[s:f1])
    end
    if vm.s == vm.top + 1
        full = true
    else
        full = false
    end
    captures = PegCapture()
    patt = vm.patt
    # there may not be any captures, in which case the whole
    # matched string is the capture:
    if lcap(vm) == 0
        push!(captures, @views vm.subject[start:prevind(vm.subject, vm.s)])
        return PegMatch(vm.subject, full, captures, patt)
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
                # Push our current captures onto the group stack
                push!(groupstack, captures)
                captures = PegCapture()
            end
            continue
        end
        if cap.inst.op == IFullCapture
            # Make a synthetic back capture, reusing this Instruction
            # The only distinct value of bcap we use is .s,
            # Which we calculate thus:
            bcap = CapFrame(cap.s - cap.inst.n, cap.inst)
            if cap.inst.kind == Cgroup # We'll need a cap vector
                push!(groupstack, captures)
                captures = PegCapture()
            end
        elseif cap.inst.op == ICloseCapture
            bcap = pop!(capstack)
            if bcap.inst.tag ≠ cap.inst.tag
                open, close = capsdict[bcap.inst.tag], capsdict[bcap.inst.tag]
                otag, ctag = Int(bcap.inst.tag), Int(cap.inst.tag)
                @warn "mismatched tags: open: $open #$otag, close $close #$ctag"
            end
        end
        ikey = cap.inst
        key = capsdict[cap.inst.tag]
        if bcap.inst.kind == ikey.kind
            if ikey.kind == Csimple
                push!(captures, _substr(bcap.s, cap.s))
            elseif ikey.kind == Csymbol
                sub = _substr(bcap.s, cap.s)
                push!(captures, key => sub)
            elseif ikey.kind == Cgroup
                # grab the outer captures and offsets
                caps = pop!(groupstack)
                if isempty(captures)  # the group is the capture
                    push!(captures, _substr(bcap.s, cap.s))
                    if key !== nothing
                        push!(caps, key => captures)
                    else
                        push!(caps, captures)
                    end
                    captures = caps
                    continue
                end
                if key !== nothing
                    push!(caps, key => captures)
                else
                    push!(caps, captures)
                end
                captures = caps
            elseif ikey.kind == Cposition
                # Note: the original intention was to capture an empty SubString,
                # Those are unfortunately broken (they set the offset to 0) so
                # I'm going to use a numbered position capture for now and
                # maybe write a whole package which does this one thing correctly
                push!(captures, Int(bcap.s))
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
                        captures = pop!(groupstack)
                        push!(captures, λ(arg))
                    end
                else  # all of captures is our arguments
                    args = captures
                    if isempty(groupstack)
                        captures = PegCapture()
                    else
                        captures = pop!(groupstack)
                    end
                    push!(captures, λ(args...))
                end
                # remove `nothing` matches
                if captures[end] === nothing
                    pop!(captures)
                end
            else
                @warn "doesn't handle the case of $(ikey.kind) yet!"
            end
        else
            error("Unbalanced caps begin $(bcap.inst.kind) end $(cap.inst.kind)")
        end
    end
    if !isempty(capstack)
        amount = length(capstack) == 1 ? "entry" : "entries"
        @warn "left $(length(capstack)) $amount on the capture stack: $(capstack)"
    end
    return PegMatch(vm.subject, full, captures, patt)
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
function Base.match(patt::Pattern, subject::AbstractString, bounds::UnitRange{<:Integer})::Union{PegMatch, PegFail}
    vm = VMState(patt, subject, bounds.start, bounds.stop)
    runvm!(vm) ? aftermatch(vm, bounds.start) : afterfail(vm)
end
function Base.match(patt::Pattern, subject::AbstractString, start::Integer)::Union{PegMatch, PegFail}
    vm = VMState(patt, subject, start, ncodeunits(subject))
    runvm!(vm) ? aftermatch(vm, start) : afterfail(vm)
end

"""
    findfirst(patt::Pattern, string::AbstractString)::Union{Integer, Nothing}

Find the first match of `patt` in `string`. Returns the index at the *end* of the match,
such that `@views string[1:findfirst(patt)]` is the match.
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
function Base.occursin(needle::Pattern, haystack::AbstractString)::Bool
    findfirst(needle, haystack) !== nothing ? true : false
end

