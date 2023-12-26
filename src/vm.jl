# JLpeg virtual machine

include("compile.jl")

struct StackFrame 
    i::Int32   # Instruction pointer
    s::UInt32  # String index
end

CallFrame(i::Int32) = StackFrame(i, 0)
CallFrame(i::UInt32) = StackFrame(Int32(i), 0)

# TODO What's a CapEntry?
struct CapEntry
    
end

"""
    VMState(subject::String, program::IVector)

Contains the state of a match on `subject` by `program`.

## Fields

    Other than the `subject` and `program`, we have:
- top: the byte length of `subject`
- i: Instruction counter 
- i: Subject pointer 
- stack: Contains stack frames for calls and backtracks 
- cap: A stack for captures
- running: a boolean which is true when the VM is executing
- matched: true if we matched the program on the subject.

## Implementation 

I'm currently using functions to manipulate the stack, because it 
may be a good optimization to cache the top of the stack in registers,
in which case I won't have manipulations scattered throughout the program.
"""
mutable struct VMState
   subject::AbstractString # The subject we're parsing
   program::IVector # Our program
   top::UInt32  # Byte length of subject string
   # Registers
   i::Int32         # Instruction Counter 
   s::UInt32        # Subject pointer
   ti::Int32        # Stack top instruction register
   ts::UInt32       # Stack top subject register
   t_on::Bool       # Is there a frame on the stack?
   stack::Vector{StackFrame}  # Stack of Instruction offsets
   cap::Vector{CapEntry}
   running::Bool
   matched::Bool
   function VMState(s::AbstractString, p::IVector)
      stack = Vector{StackFrame}(undef, 0)
      cap   = Vector{CapEntry}(undef, 0)
      top = ncodeunits(s)
      return new(s, p, top, 1, 1, 0, 0, false, stack, cap, false, false)
   end
end

@inline
function thischar(vm::VMState)
    if vm.s > vm.top
        return nothing
    end 
    vm.subject[vm.s]
end

@inline
function pushframe!(vm::VMState, i::Int32, s::UInt32)
    if !vm.t_on 
        vm.ti, vm.ts = i, s
        vm.t_on = true
    else
        frame = StackFrame(vm.ti, vm.ts)
        vm.ti, vm.ts = i, s 
        push!(vm.stack, frame)
    end
end

@inline
function pushcall!(vm::VMState)
    if !vm.t_on 
       vm.ti = vm.i + 1
       vm.t_on = true 
    else 
        frame = StackFrame(vm.ti, vm.ts)
        vm.ti, vm.ts = vm.i + 1, 0
        push!(vm.stack, frame)
    end
end

@inline
function popframe!(vm::VMState)
    if !vm.t_on 
        return (nothing, nothing)
    end
    if isempty(vm.stack)
        vm.t_on = false
        return vm.ti, vm.ts 
    end
    frame = pop!(vm.stack)
    _ti, _ts = vm.ti, vm.ts 
    vm.ti, vm.ts = frame.i, frame.s 
    return _ti, _ts
end

@inline
function updatetop_s!(vm::VMState)
    vm.ts = vm.s
end

@inline
function failmatch(vm::VMState)
    if !vm.t_on
        vm.running = false
        vm.matched = false
        return
    end
    i, s = popframe!(vm)
    while s == 0 # return from calls
        i, s = popframe!(vm)
        if i === nothing break end
    end # until we find a choice frame or exhaust the stack
    if i === nothing 
        vm.running = false
        vm.matched = false
    else 
        vm.s = s
        vm.i = i
    end
end

"""
    match(program::IVector, subject::AbstractString)

Match `program` to `subject`, returning the farthest match index.
"""
function Base.match(program::IVector, subject::AbstractString)::Union{UInt32, Nothing}
    vm = VMState(subject, program)
    vm.running = true
    while vm.running
        # print(short_vm(vm))
        # print(vm_to_str(vm))
        if vm.i > length(vm.program)
            vm.running = false
            continue
        end
        inst = vm.program[vm.i]
        if !onInst(inst, vm)
            failmatch(vm)
        end
    end
    if vm.matched
        return vm.s 
    else
        return nothing
    end
end

"""
    match(patt::Pattern, subject::AbstractString)

Match `patt` to `subject`, returning the farthest match index
"""
function Base.match(patt::Pattern, subject::AbstractString)::Union{UInt32, Nothing}
    code = compile!(patt).code
    match(code, subject)
end

"""
    onInst(inst::Instruction, vm::VMState)::Bool

Dispatch an instruction by structure, with match statements by opcode for
further dispatch. Returns `false` if the instruction fails, otherwise `true`,
in which case the dispatch function is expected to have altered the VM state.
"""
function onInst(inst::Instruction, vm::VMState)::Bool end

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
    this = thischar(vm)
    if this === nothing
        return false
    end
    code = UInt32(this)
    if code < 128 && inst.vec[code + 1]
        vm.i +=1
        vm.s = nextind(vm.subject, vm.s)
        return true
    else
        return false
    end
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

include("printing.jl")
