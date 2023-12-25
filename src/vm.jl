# JLpeg virtual machine

include("compile.jl")

mutable struct StackFrame 
    i::Int32   # Instruction pointer
    s:: UInt32  # String index
end

CallFrame(i::Int32) = StackFrame(i, 0)

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
   stack::Vector{StackFrame}  # Stack of Instruction offsets
   cap::Vector{CapEntry}
   running::Bool
   matched::Bool
   function VMState(s::AbstractString, p::IVector)
      stack = Vector{StackFrame}(undef, 0)
      cap   = Vector{CapEntry}(undef, 0)
      top = ncodeunits(s)
      return new(s, p, top, 1, 1, stack, cap, false, false)
   end
end

@inline
function thischar(vm::VMState)
    # TODO I'm not 100% convinced this is the best way to handle this.
    if vm.s > vm.top
        return nothing
    end 
    vm.subject[vm.s]
end

@inline
function pushframe!(vm::VMState, frame::StackFrame)
    push!(vm.stack,frame)
end

@inline
function pushcall!(vm::VMState, ptr::Int)
    push!(vm.stack, CallFrame(ptr))
end

@inline
function popframe!(vm::VMState)
    pop!(vm.stack)
end

@inline
function updatetop_s!(vm::VMState)
    vm.stack[end].s = vm.s
end

@inline
function failmatch(vm::VMState)
    if isempty(vm.stack)
        vm.running = false
        vm.matched = false
        return
    end
    frame = popframe!(vm)
    while frame.s == 0 # return from calls
        frame = popframe!(vm)
        if !frame break end
    end # until we find a choice frame or exhaust the stack
    if frame === nothing 
        vm.running = false
        vm.matched = false
    else 
        vm.s = frame.s
        vm.i = frame.i
    end
end

"""
    match(program::IVector, subject::AbstractString)

Match `program` to `subject`, returning the farthest match index.
"""
function Base.match(program::IVector, subject::AbstractString)
    vm = VMState(subject, program)
    vm.running = true
    while vm.running
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
function Base.match(patt::Pattern, subject::AbstractString)
    code = compile!(patt).code
    return match(code, subject)
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
    code = UInt(vm.subject[vm.s])
    if inst.vec[code + 1] # TODO code - 1 to allow for zero bytes
        vm.i +=1
        vm.s = nextind(vm.subject, vm.s)
        return true
    else
        return false
    end
end

"onTestChar"
function onInst(inst::TestCharInst, vm::VMState)::Bool
    this = thischar(vm)  # TODO `this` can be `nothing` but I think what follows is still correct
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
    code = UInt(thichar(vm))
    if code < 128 && inst.vec[code+1]
        vm.i +=1
        return true
    else
        vm.i += inst.l
        return true  # Still not an unwinding fail
    end
end

"onChoice"
function onInst(inst::ChoiceInst, vm::VMState)
    pushframe!(vm, StackFrame(vm.i + inst.l, vm.s + inst.n))
    vm.i += 1
    return true
end

function onInst(inst::LabelInst, vm::VMState)
    @match inst.op begin
        $ICommit         => return onCommit(inst, vm)
        $IJump           => return onJump(inst, vm)
        $ICall           => return onCall(inst, vm)
        $IPartialCommit  => return onPartialCommit(inst, vm)
        # TODO NYI
        $IBackCommit     => return onBackCommit(inst, vm)
    end
end

function onInst(inst::MereInst, vm::VMState)
    @match inst.op begin
        $IEnd       => return onEnd(vm)
        $IReturn    => return onReturn(vm)
        # TODO NYI
        $IFail      => return false
        $IFailTwice => return onFailTwice(vm)
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
    pushcall!(vm, vm.i + inst.l)
    return true
end

@inline
function onEnd(vm::VMState)
    @assert isempty(vm.stack) lazy"hit end instruction with $(length(vm.stack)) on stack"
    vm.running = false
    vm.matched = true
    return true
end

@inline
function onReturn(vm::VMState)
    # TODO if I cache the call stack this will start returning a Tuple, needs fix
    frame = popframe!(vm)
    @assert frame.s == 0 "rule left a choice frame on the stack"
    return true
end

@inline
function onPartialCommit(inst::LabelInst, vm::VMState)
    updatetop_s!(vm) 
    vm.i += inst.l
    return true
end

onFail(vm) = error("NYI")
onFailTwice(vm) = error("NYI")
onBackCommit(inst, vm) = error("NYI")


include("printing.jl")
