# Instruction compiling for Patterns

include("pattern.jl")

"Instruction opcodes for JLpeg VM"
@enum Opcode::UInt8 begin
    IAny        # if no char, fail
    IChar       # if char != aux, fail
    ISet        # if char not in buff, fail
    ITestAny    # in no char, jump to 'offset'
    ITestChar   # if char != aux, jump to 'offset'
    ITestSet    # if char not in buff, jump to 'offset'
    ISpan       # read a span of chars in buff
    IUTFR       # if codepoint not in range [offset, utf_to], fail
    IBehind     # walk back 'aux' characters (fail if not possible)
    IReturn     # return from a rule
    IEnd        # end of pattern
    IChoice     # stack a choice; next fail will jump to 'offset'
    IPredChoice # labeled failure: stack a choice; changes label env next fail will jump to 'offset'
    IJump       # jump to 'offset'
    ICall       # call rule at 'offset'
    INameCall   # Call as named capture
    IOpenCall   # call rule number 'key' (must be closed to a ICall)
    ICommit     # pop choice and jump to 'offset'
    IPartialCommit # update top choice to current position and jump
    IBackCommit # backtrack like "fail" but jump to its own 'offset'
    IFailTwice  # pop one choice and then fail
    IFail       # go back to saved state on choice and jump to saved offset
    IFullCapture # complete capture of last 'off' chars
    IOpenCapture # start a capture
    ICloseCapture
    ICloseRunTime
    IThrow      # fails with a given label
    IThrowRec   # fails with a given label and call rule at 'offset'
    INoOp       # to fill empty slots left by optimizations
end

instructionDict = Dict(
    IAny => (i) -> "[op, n] Match any $(i.n) characters",
    IChar => (i) -> "[op, c] Match '$(i.c)'",
    IEnd => (i) -> "[op] Ends a pattern",
    IFail => (i) -> "[op] Fails a pattern" # To be continued
)

Docs.getdoc(i::Instruction) = instructionDict[i.op](i)

struct MereInst <: Instruction
    op::Opcode
end

OpEnd = MereInst(IEnd)
OpFail = MereInst(IFail)
OpFailTwice = MereInst(IFailTwice)
OpReturn = MereInst(IReturn)
OpNoOp = MereInst(INoOp)


struct AnyInst <: Instruction
    op::Opcode
    n::UInt32
    AnyInst(n::Integer) = n ≥ 0 ? new(IAny, n) : error("n must be a natural number")
end

struct CharInst <: Instruction
    op::Opcode
    c::AbstractChar
    CharInst(c::AbstractChar) = new(IChar, c)
end

struct SetInst <: Instruction
    op::Opcode
    vec::BitVector
    SetInst(vec::BitVector) = new(ISet, vec)
end

struct BehindInst <: Instruction
    op::Opcode
    n::UInt32
    BehindInst(n::Integer) = n ≥ 0 ? new(IBehind, n) : error("n must be a natural number")
end

struct LabelInst <: Instruction
    op::Opcode
    l::Int32
end

ChoiceInst(l::Integer) = LabelInst(IChoice, l)
CommitInst(l::Integer) = LabelInst(ICommit, l)
CallInst(l::Integer) = LabelInst(ICall, l)
JumpInst(l::Integer) = LabelInst(IJump, l)
PartialCommitInst(l::Integer) = LabelInst(ICommit, l)
BackCommitInst(l::Integer) = LabelInst(ICommit, l)

struct TestAnyInst <: Instruction
    op::Opcode
    n::UInt32
    l::Int32
    TestAnyInst(n::UInt32, l::Int32) = new(ITestAny, n, l)
end

struct TestCharInst <: Instruction
    op::Opcode
    c::AbstractChar
    l::Int32
    TestCharInst(c::AbstractChar, l::Int32) = new(ITestChar, c, l)
end

struct TestSetInst <: Instruction
    op::Opcode
    vec::BitVector
    l::Int32
    TestSetInst(vec::BitVector, l::Int32) = new(ITestSet, vec, l)
end

struct OpenCallInst <: Instruction
    op::Opcode
    rule::AbstractString # Symbol?
    OpenCallInst(r::AbstractString) = new(IOpenCall, r)
end

"A placeholder for a (usually labeled) Instruction"
struct HoldInst <: Instruction
    op::Opcode
end

# To be continued...

### Compilers

"""
Compile a Pattern.

Translate the Pattern to Instruction codes, appending them to
the `code` field and returning same.  Performs various optimizations
in the process. 
"""
function compile!(patt::Pattern) 
    error("Not Yet Implemented for $(typeof(patt))")
end

function compile!(patt::PSeq)
    if !isempty(patt.code)
        return patt.code
    end
    for p in patt.val
        code = compile!(p)
        if code[end] == OpEnd
            code = code[1:end-1]
        end
        append!(patt.code, code)
        # optimizations?
    end
    push!(patt.code, OpEnd)
    patt.code
end

function compile!(patt::PAny)
    if isempty(patt.code)
        push!(patt.code, AnyInst(patt.val))
    end
    return patt.code
end

function compile!(patt::PChar)
    if isempty(patt.code)
        push!(patt.code, CharInst(patt.val))
    end
    return patt.code 
end

function compile!(patt::PSet)
    if !isempty(patt.code)
        return patt.code
    end
    bvec, prefix_map = vecsforstring(patt.val)
    if bvec !== nothing
        push!(patt.code, SetInst(bvec), OpEnd)
    end
    # We'll deal with the prefix map some other time!
    # Others are a bit more complex! heh. bit.
    return patt.code
end

function compile!(patt::PRange)
    a, b = patt.val
    vec = Vector{typeof(a)}(undef, b - a + 1)
    i = 1
    for code in a:b
        vec[i] = code 
        i += 1
    end
    bvec, prefix_map = vecsforstring(Vector{AbstractChar}(vec))
    if bvec !== nothing
       push!(patt.code, SetInst(bvec), OpEnd)
    end
    return patt.code  
end

function compile!(patt::PChoice)
    if !isempty(patt.code)
        return patt.code
    end
    c = patt.code
    for (idx, p) in enumerate(patt.val)
        pcode = compile!(p)
        if idx == length(patt.val)
            append!(c, pcode)
            break
        end
        len = length(pcode)
        push!(c, ChoiceInst(len + 1))
        append!(c, pcode)
        pop!(c)  # drop the IEnd
        push!(c, HoldInst(ICommit)) 
    end
    for (idx, inst) in enumerate(c)
        if isa(inst, HoldInst) && inst.op == ICommit
            c[idx] = CommitInst(length(c) - idx)
        end
    end
    # Peephole looking for headfail condition == test instruction
    last, this = (OpNoOp, OpNoOp)
    clobber_commit = false
    for(idx, inst) in enumerate(c)
        last, this = (this, inst)
        if last.op == IChoice && this.op == IChar
            c[idx - 1] = TestCharInst(this.c, last.l)
            c[idx] = AnyInst(1)
            clobber_commit = true
        elseif last.op == IChoice && this.op == ISet
            c[idx - 1] = TestSetInst(this.vec, last.l)
            c[idx] = AnyInst(1)
            clobber_commit = true
        end
        if clobber_commit && this.op == ICommit
            c[idx] = JumpInst(this.l)
            clobber_commit = false
        end
    end
    return patt.code
end

"""
    vecsforstring(str::Union{AbstractString, Vector{AbstractChar}})::Tuple{Union{BitVector, Nothing},Union{Dict, Nothing}}

Take a string, or a vector of characters, and break it down into bitvectors which
compactly and quickly test for those characters. 

Return `(ascii, higher)` where `ascii` is all one-byte utf8 characters and higher is a somewhat
complex dict of bitvectors useful for detecting practical multibyte ranges and sets. 
"""
function vecsforstring(str::Union{AbstractString, Vector{AbstractChar}})::Tuple{Union{BitVector, Nothing},Union{Dict, Nothing}}
    bvec = nothing
    prefix_map = nothing
    limit = Char(127)
    for char in str
        if char <= limit
            if bvec === nothing
                bvec = falses(127)
            end
            bvec[UInt(char)] = true
        else
            if prefix_map === nothing
                prefix_map = Dict()
            end
            bytes = collect(codeunits(string(char)))
            if length(bytes) == 2
                prefix!(prefix_map, bytes[1], bytes[2])
            elseif length(bytes) == 3
                prefix!(prefix_map, (bytes[1], bytes[2]), bytes[3])
            elseif length(bytes) == 4
                prefix!(prefix_map, (bytes[1], bytes[2], bytes[3]), bytes[4])
            end
        end
    end
    return (bvec, prefix_map)        
end

function prefix!(map::Dict, key, val)
    # mask off the top two bytes to save space later
    val = val & 0b00111111
    if haskey(map, key)
        push!(map[key], val)
    else
        map[key] = []
        push(map[key], val)
    end
end
