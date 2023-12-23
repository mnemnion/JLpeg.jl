# JLpeg virtual machine

include("compile.jl")

const FAIL = Int32(-1)

mutable struct StackEntry
    i::Int32   # Instruction pointer
    s:: UInt32  # String index
    # More?
end

struct CaptureEntry

end

mutable struct VMState
   subject::AbstractString # The subject we're parsing
   program::Vector{Instruction} # Our program
   s_end::UInt32  # Byte length of subject string + 1 (maybe just length(subject)?)
   # Registers
   i::Int32         # Instruction Counter 
   s::UInt32        # Subject pointer
   stack::Vector{StackEntry}  # Stack of Instruction offsets
   cap::Vector{CaptureEntry}
   function VMState(s::AbstractString, p::Vector{Instruction})
      stack = Vector{StackEntry}(undef, 0)
      cap   = Vector{CapEntry}(undef, 0)
      s_end = ncodeunits(s)
      return new(s, p, s_end, 1, 1, stack, cap)
   end
end