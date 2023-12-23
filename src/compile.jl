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

struct CharInst <: Instruction
    op::Opcode
    c::AbstractChar
    CharInst(c::AbstractChar) = new(IChar, c)
end

struct AnyInst <: Instruction
    op::Opcode
    n::UInt32
    AnyInst(n::Integer) = n ≥ 0 ? new(IAny, n) : error("n must be a natural number")
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
CallInst(l::Integer) = LabelInst(ICall, l)
JumpInst(l::Integer) = LabelInst(IJump, l)
CommitInst(l::Integer) = LabelInst(ICommit, l)
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
    TestAnyInst(c::AbstractChar, l::Int32) = new(ITestChar, c, l)
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

function compile!(patt::PChoice)
    # Unoptimized versions first
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
        push!(c, ChoiceInst(len + 2))
        append!(c, pcode)
        push!(c, HoldInst(ICommit)) 
    end
    for (idx, inst) in enumerate(c)
        if isa(inst, HoldInst) && inst.op == ICommit
            c[idx] = CommitInst(length(c) - idx)
        end
    end
    return patt.code
end

