# JLpeg virtual machine

include("compile.jl")

using Match

const FAIL = Int32(-1)

mutable struct StackEntry
    i::Int32   # Instruction pointer
    s:: UInt32  # String index
    # More?
end

struct CapEntry
    
end

mutable struct VMState
   subject::AbstractString # The subject we're parsing
   program::Vector{Instruction} # Our program
   top::UInt32  # Byte length of subject string
   # Registers
   i::Int32         # Instruction Counter 
   s::UInt32        # Subject pointer
   stack::Vector{StackEntry}  # Stack of Instruction offsets
   cap::Vector{CapEntry}
   running::Bool
   matched::Bool
   function VMState(s::AbstractString, p::Vector{Instruction})
      stack = Vector{StackEntry}(undef, 0)
      cap   = Vector{CapEntry}(undef, 0)
      top = ncodeunits(s)
      return new(s, p, top, 1, 1, stack, cap, false, false)
   end
end

thischar(vm::VMState) = vm.subject[vm.s]

function failmatch(vm::VMState)
    if isempty(vm.stack)
        vm.running = false
        vm.matched = false
    else
        # do the needful
    end
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
        matchInst(inst, vm)
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
    code = compile!(patt)
    return match(code, subject)
end

function matchInst(any::AnyInst, vm::VMState)
    idx = vm.s 
    for i in any.n:-1:1 
        idx = nextind(vm.subject, idx)
        if idx > vm.top && i > 1
            failmatch(vm)
            return
        end
    end
    vm.s = idx
    vm.i += 1
end

function matchInst(inst::CharInst, vm::VMState )
    this = thischar(vm)
    if this == inst.c 
        vm.i += 1
        vm.s = nextind(vm.subject, vm.s)
    else 
        failmatch(vm)
    end
end

function matchInst(inst::MereInst, vm::VMState)
    @match inst.op begin
        $IEnd       => onEnd(vm)
        $IFail      => onFail(vm)
        $IFailTwice => onFailTwice(vm)
        $IReturn    => onReturn(vm)
    end
end

@inline
function onEnd(vm::VMState)
    # Are we at the end of the program?
    if vm.i == length(vm.program)
        vm.running = false
        vm.matched = true
        return
    end
    # Other conditions...
end

onFail(vm) = error("NYI")
onFailTwice(vm) = error("NYI")
onReturn(vm) = error("NYI")

include("printing.jl")
