# JLpeg virtual machine

include("compile.jl")

const FAIL = Int32(-1)

mutable struct StackFrame 
    i::Int32   # Instruction pointer
    s:: UInt32  # String index
end

CallFrame(i::Int32) = StackFrame(i, 0)

struct CapEntry
    
end

mutable struct VMState
   subject::AbstractString # The subject we're parsing
   program::Vector{Instruction} # Our program
   top::UInt32  # Byte length of subject string
   # Registers
   i::Int32         # Instruction Counter 
   s::UInt32        # Subject pointer
   stack::Vector{StackFrame}  # Stack of Instruction offsets
   cap::Vector{CapEntry}
   running::Bool
   matched::Bool
   function VMState(s::AbstractString, p::Vector{Instruction})
      stack = Vector{StackFrame}(undef, 0)
      cap   = Vector{CapEntry}(undef, 0)
      top = ncodeunits(s)
      return new(s, p, top, 1, 1, stack, cap, false, false)
   end
end

@inline
thischar(vm::VMState) = vm.subject[vm.s]

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

@inline
function pushframe!(vm::VMState, frame::StackFrame)
    push!(vm.stack,frame)
end

@inline
function popframe!(vm::VMState)
    pop!(vm.stack)
end

"""
    match(program::Vector{Instruction}, subject::AbstractString)

Match `program` to `subject`, returning the farthest match index.
"""
function Base.match(program::Vector{Instruction}, subject::AbstractString)
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
    if inst.vec[code]
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
    code = UInt(vm.subject[vm.s])
    if inst.vec[code]
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
        # NYI, these will all take inst 
        $ICall           => return onCall(vm)
        $IPartialCommit  => return onPartialCommit(vm)
        $IBackCommit     => return onBackCommit(vm)
    end
end

function onInst(inst::MereInst, vm::VMState)
    @match inst.op begin
        $IEnd       => return onEnd(vm)
        $IFail      => return false
        $IFailTwice => return onFailTwice(vm)
        $IReturn    => return onReturn(vm)
    end
end

@inline
function onEnd(vm::VMState)
    @assert isempty(vm.stack) lazy"hit end instruction with $(length(vm.stack)) on stack"
    vm.running = false
    vm.matched = true
    return true
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

onFail(vm) = error("NYI")
onFailTwice(vm) = error("NYI")
onReturn(vm) = error("NYI")
onCall(vm) = error("NYI")
onJump(vm) = error("NYI")
onPartialCommit(vm) = error("NYI")
onBackCommit(vm) = error("NYI")


include("printing.jl")
