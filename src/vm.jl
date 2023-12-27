# JLpeg virtual machine

include("compile.jl")
include("pegmatch.jl")

struct StackFrame
    i::Int32   # Instruction pointer
    s::UInt32  # String index
    c::Int     # Capture level
end

"An entry in the capture stack"
struct CapEntry
    i::Int32       # Instruction pointer
    s::UInt32      # String index
    off::Int32     # Offset (for full captures)
    op::Opcode     # What variety of capture is this
    kind::CapKind  # Sort of capture
    CapEntry(i::Int32, s::UInt32, off::Int32, op::Opcode, kind::CapKind) = new(i, s, off, op, kind)
    CapEntry(i::Int32, s::UInt32, op::Opcode, kind::CapKind) = new(i, s, 0, op, kind)
end

"""
    VMState(patt::Pattern, string::AbstractString)

Contains the state of a match on `subject` by `program`.

# Fields

    Other than the `subject` and `program`, we have:

- top: the byte length of `subject`
- i: Instruction counter
- s: Subject pointer
- ti, ts, tc: Stack top registers
- t_on: flag for nonempty stack
- stack: Contains stack frames for calls and backtracks
- cap: A stack for captures
- running: a boolean which is true when the VM is executing
- matched: true if we matched the program on the subject.

# Implementation

Classic dispatch-driven VM with a program counter, opcodes, stack
frames.  I borrowed a page from Forth and made the top of the
stack a register, allowing stack frames to be immutable while still
permitting the PartialCommit optimization.
"""
mutable struct VMState
   subject::AbstractString # The subject we're parsing
   program::IVector # Our program
   patt::Pattern # The pattern it's derived from
   top::UInt32  # Byte length of subject string
   # Registers
   i::Int32         # Instruction Counter
   s::UInt32        # Subject pointer
   ti::Int32        # Stack top instruction register
   ts::UInt32       # Stack top subject register
   tc::UInt32       # Stack top capture level register
   t_on::Bool       # Is there a frame on the stack?
   stack::Vector{StackFrame}  # Stack of Instruction offsets
   cap::Vector{CapEntry}
   running::Bool
   matched::Bool
   function VMState(patt::Pattern, subject::AbstractString)
      program = compile!(patt).code
      stack = Vector{StackFrame}(undef, 0)
      cap   = Vector{CapEntry}(undef, 0)
      top = ncodeunits(subject)
      return new(subject, program, patt, top, 1, 1, 0, 0, 0, false, stack, cap, false, false)
   end
end

CapEntry(vm::VMState, inst::OpenCaptureInst) = CapEntry(vm.i, vm.s, Int32(0), IOpenCapture, inst.kind)
CapEntry(vm::VMState, inst::CloseCaptureInst) = CapEntry(vm.i, vm.s - Int32(1), Int32(0), ICloseCapture, inst.kind)
CapEntry(vm::VMState, inst::FullCaptureInst) = CapEntry(vm.i, vm.s, inst.off, IFullCapture, inst.kind)

# ## VM Actions
#
# All functions which mutate a running VM state other than instruction dispatch.

@inline
"Push a full frame onto the stack."
function pushframe!(vm::VMState, i::Int32, s::UInt32)
    if !vm.t_on
        vm.ti, vm.ts, vm.tc = i, s, lcap(vm)
        vm.t_on = true
    else
        frame = StackFrame(vm.ti, vm.ts, vm.tc)
        vm.ti, vm.ts, vm.tc = i, s, lcap(vm)
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
        frame = StackFrame(vm.ti, vm.ts, vm.tc)
        vm.ti, vm.ts, vm.tc = vm.i + 1, 0, 0
        push!(vm.stack, frame)
    end
end

@inline
"Pop a stack frame. Returns a tuple (i, s, c)"
function popframe!(vm::VMState)
    if !vm.t_on
        return (nothing, nothing, nothing)
    end
    if isempty(vm.stack)
        vm.t_on = false
        return vm.ti, vm.ts, vm.tc
    end
    frame = pop!(vm.stack)
    _ti, _ts, _tc = vm.ti, vm.ts, vm.tc
    vm.ti, vm.ts = frame.i, frame.s
    return _ti, _ts, _tc
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
    return length(vm.cap)
end

@inline
"Trim the capture stack height to `c``."
function trimcap!(vm::VMState, c::UInt32)
    while lcap(vm) > c
        pop!(vm.cap)
    end
    return
end

@inline
"Push a CapEntry."
function pushcap!(vm::VMState, inst::Instruction)
   push!(vm.cap, CapEntry(vm, inst))
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
"Unwind the stacks on a match failure"
function failmatch!(vm::VMState)
    if !vm.t_on
        vm.running = false
        vm.matched = false
        return
    end
    i, s, c = popframe!(vm)
    while s == 0 # return from calls
        i, s, c = popframe!(vm)
        if i === nothing break end
    end # until we find a choice frame or exhaust the stack
    if i === nothing
        vm.running = false
        vm.matched = false
    else
        vm.s = s
        vm.i = i
        trimcap!(vm, c)
    end
end


"""
    followSet(inst::Instruction, match::Bool, vm::VMState)::Bool

Follow a set of instructions forming a single logical set instruction.
Return the success of any of these instructions, or `false`.
"""
function followSet(inst::Instruction, match::Bool, vm::VMState)::Bool
    if !inst.final
        vm.i += 1
        inst = vm.program[vm.i]
        if match
            return followSet(inst, match, vm)
        elseif onInst(inst, vm)
            match = true
        end
    else  # if subject advanced, it happened on match
        vm.i += 1
    end
    return match
end


# ## VM core and instructions
#
# The core execution loop and all dispatched instructions.

"""
    runvm!(vm::VMState)::Nothing

Run a vm. A classic instruction-dispatch loop, relying on Julia's excellent
method specialization to provide speed.  Once I have some representative patterns
and sufficiently weighty test data, I may try swapping in a Vector of function
pointers to see if the code generated from this approach is, in fact, optimal.
"""
function runvm!(vm::VMState)::Nothing
    vm.running = true
    while vm.running
        @debug short_vm(vm)
        # print(vm_to_str(vm))
        if vm.i > length(vm.program)
            vm.running = false
            continue
        end
        inst = vm.program[vm.i]
        if !onInst(inst, vm)
            failmatch!(vm)
        end
    end
end


"""
    onInst(inst::Instruction, vm::VMState)::Bool

Dispatch an instruction by structure, with match statements by opcode for
further dispatch. Returns `false` if the instruction fails, otherwise `true`,
in which case the dispatch function is expected to have altered the VM state.
"""
function onInst(inst::Instruction, vm::VMState)::Bool
    @error "unrecognized instruction $inst"
    false
end

"onAny"
function onInst(any::AnyInst, vm::VMState)::Bool
    idx = vm.s
    for i in any.n:-1:1
        idx = nextind(vm.subject, idx)
        if idx > vm.top && i > 1
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
        return false
    end
    if this == inst.c
        vm.i += 1
        vm.s = nextind(vm.subject, vm.s)
        return true
    else
        return false
    end
end

"onSet"
function onInst(inst::SetInst, vm::VMState)::Bool
    match = false
    this = thischar(vm)
    if this !== nothing
        code = UInt32(this)
        if code < 128 && inst.vec[code + 1]
            vm.s = nextind(vm.subject, vm.s)
            match = true
        end
    end
    followSet(inst, match, vm)
end

function onInst(inst::MultiSetInst, vm::VMState)::Bool
    # Check if there's room to match
    match = false
    this = thischar(vm)
    if this !== nothing
        lead = inst.lead
        width, bytes = encode_utf8(this)
        # Correct sort of codepoint?
        if width == sizeof(lead) + 1
            headmatch = true
            for (idx, byte) in enumerate(lead)
                headmatch = headmatch && bytes[idx] == byte
            end
            if headmatch
                # Mask off the high bit, always 1 for follow char
                # Add 1 for Julia indexing
                comp = (bytes[end] & 0b01111111) + UInt8(1)
                if inst.vec[comp]
                    match = true
                end
            end
        end
        if match
            vm.s += width
        end
    end
    followSet(inst, match, vm)
end

"onTestChar"
function onInst(inst::TestCharInst, vm::VMState)::Bool
    this = thischar(vm)
    if this == inst.c
        vm.i += 1
        return true
    else
        vm.i += inst.l
        return true  # Not an unwinding fail
    end
end

"onTestSet"
function onInst(inst::TestSetInst, vm::VMState)::Bool
    this = thischar(vm)
    if this === nothing
        return false
    end
    code = UInt32(this)
    if code < 128 && inst.vec[code + 1]
        vm.i += 1
        return true
    else
        vm.i += inst.l
        return true  # Still not an unwinding fail
    end
end

"onOpenCapture"
function onInst(inst::OpenCaptureInst, vm::VMState)::Bool
    pushcap!(vm, inst)
    vm.i += 1
    return true
end

"onCloseCapture"
function onInst(inst::CloseCaptureInst, vm::VMState)::Bool
    pushcap!(vm, inst)
    vm.i +=1
    return true
end

"onFullCapture"
function onInst(inst::FullCaptureInst, vm::VMState)::Bool
    pushcap!(vm, inst)
    vm.i += 1
    return true
end


"onChoice"
function onInst(inst::ChoiceInst, vm::VMState)
    pushframe!(vm, vm.i + inst.l, vm.s + inst.n)
    vm.i += 1
    return true
end

function onInst(inst::LabelInst, vm::VMState)
    if inst.op == ICommit            return onCommit(inst, vm)
    elseif inst.op == IJump          return onJump(inst, vm)
    elseif inst.op == ICall          return onCall(inst, vm)
    elseif inst.op == IPartialCommit return onPartialCommit(inst, vm)
    elseif inst.op == IBackCommit    return onBackCommit(inst, vm)
    end
end

function onInst(inst::MereInst, vm::VMState)
     if inst.op == IEnd           return onEnd(vm)
     elseif inst.op == IReturn    return onReturn(vm)
     elseif inst.op == IFail      return false
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
    if !vm.t_on
        return false
    end
    i, s = popframe!(vm)
    while s == 0 # return from calls
        i, s = popframe!(vm)
        if i === nothing break end
    end # until we find a choice frame or exhaust the stack
    if i === nothing
        return false
    else
        vm.i += inst.l
        vm.s = s
        return true
    end
end

@inline
function onFailTwice(vm::VMState)
    popframe!(vm)
    return false
end

"""
    oncapmatch(vm::VMState)

Process the capture list and return what we find.
"""
function oncapmatch(vm::VMState)::PegMatch
    # Iterate C style so I can try optimizing use of the captable
    # For now, we just handle simple captures.
    S = typeof(vm.subject)
    match = SubString{S}(vm.subject, 1, vm.s-1)
    s1, s2 = nothing, nothing
    offset = 1
    captures = Vector{Union{SubString{S},Nothing}}()
    offsets = Vector{Int}()
    for i in 1:lcap(vm)
        cap = vm.cap[i]
        if cap.kind == Csimple
            if cap.op == IOpenCapture
                s1 = cap.s
            elseif cap.op == ICloseCapture
                s2 = cap.s
                if isnothing(s1)
                    error("Found a close capture without an open")
                end
                push!(offsets, Int(s1))
                push!(captures, vm.subject[s1:s2])
            end
        else
            @warn "No actions for $(cap.kind) yet!"
        end
    end

    return PegMatch(match, captures, offset, offsets, vm.patt)
end

## Core Method extensions


"""
    match(patt::Pattern, subject::AbstractString)::Union{PegMatch, Nothing}

Match `patt` to `subject`, returning a `PegMatch` implementing the expected interface
for its supertype `AbstractMatch`, or `nothing` if the match fails.
"""
function Base.match(patt::Pattern, subject::AbstractString)::Union{PegMatch, Nothing}
    vm = VMState(patt, subject)
    runvm!(vm)
    if vm.matched
        return oncapmatch(vm)
    else
        return nothing
    end
end


"""
    findfirst(patt::Pattern, string::AbstractString)::Union{Integer, Nothing}

Find the first match of `patt` in `string`. Returns the index at the *end* of the match,
such that string[1:findfirst(patt)] will show the substring.
"""
function Base.findfirst(patt::Pattern, string::AbstractString)::Union{Integer, Nothing}
    vm = VMState(patt, string)
    runvm!(vm)
    if vm.matched
        return vm.s - 1
    else
        return nothing
    end
end

include("printing.jl")
