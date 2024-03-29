# Instruction compiling for Patterns

# Stooges to pad out instructions
const MOE::UInt32 = 0xffffffff
const LARRY::UInt16 = 0xffff
const CURLY::UInt8 = 0xff

struct UrInst <: Instruction
    moe::UInt32
    larry::UInt16
    curly::UInt8
    op::Opcode
    UrInst() = error("UrInst must not be instantiated")
end

struct MereInst <: Instruction
    moe::UInt32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
MereInst(op::Opcode) = MereInst(MOE, LARRY, CURLY, op)

OpEnd = MereInst(IEnd)
OpFail = MereInst(IFail)
OpFailTwice = MereInst(IFailTwice)
OpReturn = MereInst(IReturn)
OpNoOp = MereInst(INoOp)

struct AnyInst <: Instruction
    n::UInt32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
AnyInst(n::Integer) = n ≥ 0 ? AnyInst(UInt32(n), LARRY, CURLY, IAny) : error("n must be a natural number")

"Not yet in use"
struct TestAnyInst <: Instruction
    l::Int32
    n::UInt16
    curly::UInt8
    op::Opcode
end
TestAnyInst(n::UInt32, l::Integer) = TestAnyInst(Int32(l), UInt16(n), CURLY, ITestAny)

struct CharInst <: Instruction
    one::UInt8
    two::UInt8
    three::UInt8
    four::UInt8
    larry::UInt16
    nchar::UInt8
    op::Opcode
end
CharInst(c::AbstractChar) = CharInst(padout(Char(c))..., LARRY, ncodeunits(c), IChar)
CharInst(c::UInt8) = CharInst(c, 0, 0, 0, LARRY, 1, IChar)
struct NotCharInst <: Instruction
    one::UInt8
    two::UInt8
    three::UInt8
    four::UInt8
    larry::UInt16
    nchar::UInt8
    op::Opcode
end
NotCharInst(c::AbstractChar) = NotCharInst(padout(Char(c))..., LARRY, ncodeunits(c), INotChar)

"Not yet in use"
struct TestCharInst <: Instruction
    one::UInt8
    two::UInt8
    three::UInt8
    four::UInt8
    l::Int16
    curly::UInt8
    op::Opcode
end
TestCharInst(c::AbstractChar, l::Integer) = TestCharInst(padout(Char(c))..., Int16(l), CURLY, ITestChar)

"pad the bytes of a Char out to 4"
function padout(c::Char)::Tuple{UInt8,UInt8,UInt8,UInt8}
    cvec = codeunits(string(c))
    if sizeof(cvec) == 1
        return cvec[1], 0x00, 0x00, 0x00
    elseif sizeof(cvec) == 2
        return cvec[1], cvec[2], 0x00, 0x00
    elseif sizeof(cvec) == 3
        return cvec[1], cvec[2], cvec[3], 0x00
    else
        return cvec[1], cvec[2], cvec[3], cvec[4]
    end
end
struct SetInst <: Instruction
    l::Int32
    larry::UInt16
    curly::UInt8
    op::Opcode
end

SetInst() = SetInst(Int32(3), LARRY, CURLY, ISet)
SetInst(l::Integer) = SetInst(Int32(l), LARRY, CURLY, ISet)
LeadSetInst(l::Integer) = SetInst(l, LARRY, CURLY, ILeadSet)

struct NotSetInst <: Instruction
    l::Int32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
NotSetInst(l::Int32) = NotSetInst(l, LARRY, CURLY, INotSet)


"Not yet in use"
struct TestSetInst <: Instruction
    l::Int32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
TestSetInst(l::Integer) = TestSetInst(Int32(l), LARRY, CURLY, ITestSet)

struct MultiVecInst <: Instruction
    l::Int32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
MultiVecInst(l::Integer) = MultiVecInst(Int32(l), LARRY, CURLY, IMultiVec)

struct InstructionVec <: Instruction
    vec::UInt64
end
InstructionVec(vec::Bits{Int64}) = InstructionVec(reinterpret(UInt64,vec.chunk))

struct LeadMultiInst <: Instruction
    l::Int32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
LeadMultiInst(l::Integer) = LeadMultiInst(Int32(l), LARRY, CURLY, ILeadMulti)

struct LeadByteInst <: Instruction
    l::Int32
    larry::UInt16
    b::UInt8
    op::Opcode
end
LeadByteInst(b::UInt8, l::Integer) = LeadByteInst(Int32(l), LARRY, b, IByte)

struct BehindInst <: Instruction
    n::UInt32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
BehindInst(n::Integer) = n ≥ 0 ? BehindInst(n, LARRY, CURLY, IBehind) : error("n must be a natural number")

struct LabelInst <: Instruction
    l::Int32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
LabelInst(op::Opcode, l::Integer) = LabelInst(l, LARRY, CURLY, op)

struct ChoiceInst <: Instruction
    l::Int32
    larry::UInt16
    curly::UInt8
    op::Opcode
end
ChoiceInst(l::Integer) = ChoiceInst(Int32(l), LARRY, CURLY, IChoice)

PredChoiceInst(l::Integer) = LabelInst(IPredChoice, Int32(l))
CommitInst(l::Integer) = LabelInst(ICommit, Int32(l))
CallInst(l::Integer) = LabelInst(ICall, Int32(l))
JumpInst(l::Integer) = LabelInst(IJump, Int32(l))
PartialCommitInst(l::Integer) = LabelInst(IPartialCommit, Int32(l))
BackCommitInst(l::Integer) = LabelInst(IBackCommit, Int32(l))

struct OpenCallInst <: Instruction
    op::Opcode
    rule::Symbol # Symbol?
end
OpenCallInst(r::Symbol) = OpenCallInst(IOpenCall, r)

"A placeholder for an Instruction when the label is not yet known"
struct HoldInst <: Instruction
    moe::UInt32
    larry::UInt16
    curly::UInt8
    op::Opcode
    HoldInst(op::Opcode) = new(MOE, LARRY, CURLY, op)
end

struct CaptureInst <: Instruction
    n::Int16
    tag::UInt16
    larry::UInt16
    kind::CapKind
    op::Opcode
end
CaptureInst(op::Opcode, kind::CapKind) = CaptureInst(Int16(0), UInt16(0), LARRY, kind, op)
CaptureInst(op::Opcode, kind::CapKind, tag::UInt16) = CaptureInst(Int16(0), tag, LARRY, kind, op)

OpenCaptureInst(kind::CapKind) = CaptureInst(IOpenCapture, kind, UInt16(0))
OpenCaptureInst(kind::CapKind, tag::UInt16) = CaptureInst(IOpenCapture, kind, tag)
CloseCaptureInst(kind::CapKind, tag::UInt16) = CaptureInst(Int16(0), tag, LARRY, kind, ICloseCapture)
FullCaptureInst(kind::CapKind, n::Integer, tag::UInt16) = CaptureInst(Int16(n), tag, LARRY, kind, IFullCapture)
CloseRunTimeInst(kind::CapKind, tag::UInt16) = CaptureInst(Int16(0), tag, LARRY, kind, ICloseRunTime)

struct ThrowInst <: Instruction
    mode::UInt32
    tag::UInt16
    curly::UInt8
    op::Opcode
end
ThrowInst(tag::UInt16) = ThrowInst(MOE, tag, CURLY, IThrow)

struct ThrowRecInst <: Instruction
    l::Int32
    tag::UInt16
    curly::UInt8
    op::Opcode
end
ThrowRecInst(tag::UInt16, l::Integer) = ThrowRecInst(Int32(l), tag, CURLY, IThrowRec)

struct OpenMarkInst <: Instruction
    moe::UInt32
    tag::UInt16
    curly::UInt8
    op::Opcode
end
OpenMarkInst(tag::UInt16) = OpenMarkInst(MOE, tag, CURLY, IOpenMark)

struct CloseMarkInst <: Instruction
    moe::UInt32
    tag::UInt16
    curly::UInt8
    op::Opcode
end
CloseMarkInst(tag::UInt16) = CloseMarkInst(MOE, tag, CURLY, ICloseMark)

struct CheckMarkInst <: Instruction
    tag::UInt16
    larry::UInt16
    check::UInt16
    curly::UInt8
    op::Opcode
end
CheckMarkInst(tag::UInt16, check::UInt16) = CheckMarkInst(tag, LARRY, check, CURLY, ICheckMark)

"""
    relabel(inst::Instruction, l::Integer)

Produce an Instruction based on the old one, with the new label.
"""
relabel(inst::Instruction, ::Integer) = error("$(inst.op) instructions have no label")

function _relabeler(T::Union{DataType,UnionAll})
    local params = []
    if !hasfield(T, :l)
        return nothing
    end
    for field in fieldnames(T)
        if field == :l
            push!(params, field)
        else
            push!(params, :(inst.$field))
        end
    end
    return :(relabel(inst::$(T), l::Integer) = $T($(params...),))
end

begin
    for I in subtypes(JLpeg.Instruction)
        eval(_relabeler(I))
    end
end

# ## Vector Ops
#
# Code borrowed from BitPermutations.jl for converting vector instructions
# into bit types

"""
    bitsize(::Type{T})
    bitsize(obj::T)

Number of bits in the binary representations of any primitive type `T`.
"""
function bitsize(x::Type{T}) where {T}
    isprimitivetype(x) || throw(ArgumentError("Argument of `bitsize` must be a primitive type"))
    return 8 * sizeof(x)
end

bitsize(::T) where {T} = bitsize(T)

"""
    shift_safe(::Type{T}, s::Integer)

Changes the shifting amount for bitshifts to guarantee to the compiler that the shift
amount will not exceed `bitsize(T)`.  Because bit shifting behaves slighly
differently in Julia vs. LLVM, this help the compiler emit less code.

See also: https://github.com/JuliaLang/julia/issues/30674.
"""
@inline shift_safe(::Type{T}, s::Integer) where {T} = s & (bitsize(T) - 1)

# #/ borrow

@inline
function Base.getindex(inst::InstructionVec, i::Integer)
    @boundscheck i ≤ 64 || throw(BoundsError)
    u = one(Int64) << shift_safe(Int64, i - 1)
    return !iszero(inst.vec & u)
end

Base.keys(::InstructionVec) = 1:64

function Base.iterate(inst::InstructionVec, i::Integer)
    i += 1
    i > 64 && return nothing
    if @inbounds inst[i]
        return UInt8(i-1), i
    else
        return false, i
    end
end

Base.iterate(inst::InstructionVec) = iterate(inst, 0)

Base.count_ones(inst::InstructionVec) = count_ones(inst.vec)

# # #

# ## Compilers

"""
    compile!(patt::Pattern)::Pattern

Compile a [`Pattern`](@ref).  It isn't necessary to call this, `match`
will compile a `Pattern` if necessary, but this is a useful thing to do during
precompilation.

This translates the Pattern to Instruction codes, appending them to
the `code` field and returning same.  Performs various optimizations
in the process.

"""
function compile!(patt::Pattern)::Pattern
    !isempty(patt.code) && return patt

    build(patt, patt.code)
    push!(patt.code, OpEnd)
    peephole!(patt.code)
    return patt
end

# We have to walk grammars when we build them, to
# get the Throws for rule recovery, so, alone amongst
# the PAuxTs, we treat this like a primitive.
function compile!(patt::Grammar)::Pattern
    !isempty(patt.code) && return patt

    build(patt, patt.code)
    push!(patt.code, OpEnd)
    peephole!(patt.code)
    return patt
end

function compile!(patt::PAuxT)::Pattern
    !isempty(patt.code) && return patt

    build(patt, patt.code)
    push!(patt.code, OpEnd)
    peephole!(patt.code)
    # Hoist capture and throw labels
    prewalkpatt!(patt, patt.aux) do p, aux
        if p isa PCapture
            caps = get!(()-> Dict(), aux, :caps)
            caps[p.tag] = p.cap
        elseif p isa PThrow
            throws = get!(()-> Dict(), aux, :throws)
            throws[p.tag] = p.val
        elseif p isa PCheck
            checks = get!(()-> Dict(), aux, :checks)
            checks[p.check_tag] = p.check
        end
    end
    return patt
end

"""
    prewalkpatt!(λ::Function, patt::Pattern, args...)::Nothing

Apply `λ(patt, args...)` to `patt`, then recursively to all `patt.val::PVector`
values.  Returns `nothing`, `λ` is expected to mutate `args`.
"""
function prewalkpatt!(λ::Function, patt::Pattern, args...)::Nothing
    λ(patt, args...)
    if patt.val isa PVector
        for p in patt
            prewalkpatt!(λ, p::Pattern, args...)
        end
    end
    return
end

function build(patt::Pattern, code::IVector=Inst())::IVector
    error("Not Yet Implemented for $(typeof(patt))")
    return code  # unreachable ofc
end

function build(patt::PAny, code::IVector=Inst())::IVector
    # Optimize away P(0) to P(true) to avoid
    # inefficent VM instruction
    if patt.val == 0
        return code
    end
    push!(code, AnyInst(patt.val))
    return code
end

function build(patt::PChar, code::IVector=Inst())::IVector
    push!(code, CharInst(patt.val))
    return code
end

function build(patt::PByte, code::IVector=Inst())::IVector
    push!(code, CharInst(patt.val))
    return code
end

function build(::PTrue, code::IVector=Inst())::IVector
    return code
end

function build(::PFalse, code::IVector=Inst())::IVector
    push!(code, OpFail)
    return code
end

function build(patt::POpenCall, code::IVector=Inst())::IVector
    push!(code, OpenCallInst(patt.val))
    return code
end

function build(patt::PCall, code::IVector=Inst())::IVector
    push!(code, OpenCallInst(patt.val))
    return code
end

function build(patt::PBehind, code::IVector=Inst())::IVector
    len = fixedlen(only(patt))
    if len === false
       throw(PegError("in B(patt), patt must be of fixed length, not a $(typeof(patt.val[1]))"))
    elseif len == 0  # Return the pattern
        return build(only(patt), code)
    end
    push!(code, BehindInst(len))
    code = build(only(patt), code)
    return code
end

function build(patt::PSet, code::IVector=Inst())::IVector
    # Specialize the empty set
    if isempty(patt.val)  # A valid way of saying "fail"
        push!(code, OpFail)
        return code
    end
    # Specialize one-char sets into PChar
    if length(patt.val) == 1 && first(patt.val).start == first(patt.val).stop
        return build(PChar(first(patt.val).start), code)
    end
    bvec, prefix_map = makebitvectors(patt.val)
    if bvec !== nothing && prefix_map === nothing
        low, high = makemasks(bvec)
        push!(code, SetInst(), low, high)
    else
        encode_multibyte_set!(code, bvec, prefix_map)
    end
    return code
end

function build(patt::PAnd, code::IVector=Inst())::IVector
    push!(code, HoldInst(IPredChoice))
    hold = length(code)
    code = build(only(patt), code)
    l = length(code) - hold
    code[hold] = PredChoiceInst(l)
    push!(code, BackCommitInst(2), OpFail)
    return code
end

function build(patt::PNot, code::IVector=Inst())::IVector
    template = build(only(patt))
    # TODO We can remove captures from PNot patterns,
    # which never succeed (except match-time captures),
    # but this is unlikely to occur until we inline simple calls (which we should)
    #
    # We turn ASCII sets into INotSet, which can
    # succeed against multibyte chars
    if length(template) == 3 && template[1].op == ISet
        push!(code, NotSetInst(template[1].l), template[2], template[3])
        return code
    end
    # We do the same for PChar
    if patt[1] isa PChar
        push!(code, NotCharInst(patt[1].val))
        return code
    end
    l = length(template) + 2  # FailTwice, next
    push!(code, PredChoiceInst(l))
    append!(code, template)
    push!(code, OpFailTwice)
    return code
end

function build(patt::PDiff, code::IVector=Inst())::IVector
    code = build(PSeq(PNot(patt[2]), patt[1]), code)
    return code
end

function build(patt::PSeq, code::IVector=Inst())::IVector
    for p in patt.val
        code = build(p, code)
    end
    return code
end

function build(patt::PStar, code::IVector=Inst())::IVector
    # TODO figure out when TestChar etc come into play
    #
    # bad things can happen when val is also a PStar, specifically
    # when the inner is optional, e.g. ("ab"?)*, so we check for this
    # and fix it when we need to.
    p = only(patt)
    if typeof(p) == PStar && p.n ≤ 0
        if p.n ≤ -1 && (patt.n == 0 || patt.n == 1)
            # "As many optionals as you want, as long as you match one" aka P^0
            return build(p.val[1]^0, code)
        elseif p.n == 0 && (patt.n == 0 || patt.n == 1)
            # Both of these mean "match as few as zero or as many as you can",
            # Which is what p means already.
            return build(p, code)
        elseif p.n == 0 && patt.n == -1
            # Same outcome as the above, but this time it's an optimization,
            # the code actually works fine, but the -1 isn't doing anything here
            return build(p, code)
        end
    end
    # Nullables: pointless for patt.n < 0, infinite loop otherwise, fail:
    if nullable(p)
        error("this $(typeof(p)) is nullable, repetition is not allowed")
    end
    template = build(p)
    # TODO TestPattern optimization goes here, pass a flag to `addstar!`
    if patt.n == 0
        addstar!(code, template)
    elseif patt.n ≥ 1
        for _ = 1:patt.n
            append!(code, template)
        end
        addstar!(code, template)
    elseif patt.n == -1
        push!(code, ChoiceInst(length(template) + 2))
        append!(code, template)
        push!(code, CommitInst(1))
    elseif patt.n < -1
        push!(code, HoldInst(IChoice))
        hold = length(code)
        for _ = patt.n:-2
            append!(code, template)
            push!(code, PartialCommitInst(1))
        end
        append!(code, template)
        push!(code, CommitInst(1))
        code[hold] = ChoiceInst(length(code) - hold)
    else
        error("unreachable, logic is broken")
    end
    return code
end

function addstar!(c::IVector, code::IVector)
    l = length(code)
    if l == 3 && code[1].op == ISet
        # Span instruction
        push!(c, LeadSetInst(0), code[2], code[3])
        return
    end
    push!(c, ChoiceInst(l + 2)) # Choice + PartialCommit
    append!(c, code)
    push!(c, PartialCommitInst(-l))
    # Choice Target
end

function build(patt::PChoice, code::IVector=Inst())::IVector
    for (idx, p) in enumerate(patt)
        pcode = build(p)
        if idx == length(patt.val)
            append!(code, pcode)
            break
        end
        len = length(pcode)
        push!(code, ChoiceInst(len + 2))  # +2 == Choice and Commit
        append!(code, pcode)
        push!(code, HoldInst(ICommit))
    end

    for (idx, inst) in enumerate(code)
        if isa(inst, HoldInst) && inst.op == ICommit
            code[idx] = CommitInst(length(code) - idx + 1)  # + 1 for rest of pattern
        end
    end

    return code
end

function build(patt::PCapture, code::IVector=Inst())::IVector
    # Special-case Cp() & Cc(...)
    if patt.kind == Cposition || patt.kind == Cconst
        full = FullCaptureInst(patt.kind, 0, patt.tag)
        push!(code, full)
        return code
    end
    capture = build(only(patt))
    # If patt[1] is fixedlen, and not a group, we use FullCaptureInst
    if patt.kind ≠ Cgroup
        len = fixedlen(patt[1])
        if len ≠ false
            append!(code, capture)
            push!(code, FullCaptureInst(patt.kind, len, patt.tag))
            return code
        end
    end
    push!(code, OpenCaptureInst(patt.kind, patt.tag))
    append!(code, capture)
    if patt.kind == Ctest || patt.kind == Cvm
        close = CloseRunTimeInst(patt.kind, patt.tag)
    else
        close = CloseCaptureInst(patt.kind, patt.tag)
    end
    push!(code, close)
    return code
end

function build(patt::PThrow, code::IVector=Inst())::IVector
    # Grammars may recode this as ThrowRecInst
    push!(code, ThrowInst(patt.tag))
    return code
end

function build(patt::PMark, code::IVector=Inst())::IVector
    push!(code, OpenMarkInst(patt.tag))
    # TODO Marks can't enclose marks, check
    code = build(patt[1], code)
    push!(code, CloseMarkInst(patt.tag))
    return code
end

function build(patt::PCheck, code::IVector=Inst())::IVector
    push!(code, OpenMarkInst(patt.tag))
    # TODO Marks can't enclose marks, check
    code = build(patt[1], code)
    push!(code, CheckMarkInst(patt.tag, patt.check_tag))
    return code
end

function build(patt::PRule, code::IVector=Inst())::IVector
    code = build(patt[1], code)
    return code
end


"""
    compile!(patt::PGrammar)::Pattern

Compile a Grammar.  This will fail if all rules are not provided.
"""
# TODO add

function build(patt::PGrammar, code::IVector=Inst())::IVector
    aux = patt.aux
    aux[:caps] = Dict()  # TODO TagDict maybe?
    aux[:throws] = Dict()  # It's a distinctive type!
    aux[:callsite] = Dict()
    aux[:start] = patt[1].name
    rules = aux[:rules] = AuxDict()
    for rule in patt
        rules[rule.name] = rule
        # Defaults
        rule.aux[:visiting] = false
        rule.aux[:walked] = false
        rule.aux[:recursive] = false  # We change this where applicable in recursecompile!
    end
    prewalkpatt!(patt, aux) do p::Pattern, a::AuxDict
        if p isa POpenCall
            if haskey(a[:rules], p.val)
                # Replace it with a call having the ref
                return PCall(p, a[:rules][p.val])
            else
                error(PegError("$(patt.start) has no rule $(p.val)"))
            end
        elseif p isa PCapture
            caps = get!(()-> Dict(), aux, :caps)
            caps[p.tag] = p.cap
        elseif p isa PThrow
            throws = get!(()-> Dict(), aux, :throws)
            throws[p.tag] = p.val
        elseif p isa PCheck
            checks = get!(()-> Dict(), aux, :checks)
            checks[p.check_tag] = p.check
        end
    end
    aux[:seen] = Symbol[]
    recursepattern!(patt, aux)
    push!(code, CallInst(2))
    push!(code, HoldInst(IJump))
    hold = length(code)
    for rulename in aux[:seen]
        aux[:callsite][rulename] = length(code) + 1
        code = build(aux[:rules][rulename], code)
        push!(code, OpReturn)
    end
    @assert code[hold] isa HoldInst "c[hold] isa $(typeof(c[hold])) $(c[hold].op))"
    code[hold] = JumpInst(length(code) - 1)
    link!(code, aux)
    delete!(aux, :rules)
    return code
end

"""
    recursepattern!(patt::Pattern, gaux::AuxDict)::Pattern

Recursively walk the pattern.
"""
function recursepattern!(patt::Pattern, gaux::AuxDict)::Pattern
    if patt isa PPrimitive
        return patt
    end
    if patt isa PRule
        if patt.aux[:walked]
            return patt
        else
            push!(gaux[:seen], patt.name)
            patt.aux[:visiting] = true
        end
    end
    if !isa(patt.val, Vector)
        @error "missing primitive maybe? $(typeof(patt)).val isa $(typeof(patt.val))"
    end
    for (idx, p) in enumerate(patt)
        if p isa PCall
            # Self-call?
            if p.ref.aux[:visiting]
                # we're somewhere inside the walk of this expression
                p.ref.aux[:recursive] = true
                continue
            else
                if !p.ref.aux[:walked]
                     # visiting p.ref, compiling p.ref.val[1] (rule body)
                    push!(gaux[:seen], p.ref.name)
                    p.ref.aux[:visiting] = true
                    p.ref.val[1] = recursepattern!(p.ref[1], gaux)
                    p.ref.aux[:visiting] = false
                    p.ref.aux[:walked] = true
                end
            end
        end
        patt.val[idx] = recursepattern!(p, gaux)
    end
    patt.aux[:visiting] = false
    patt.aux[:walked] = true
    return patt
end

"""
    link!(code::IVector, aux::AuxDict)

Links a grammar by replacing IOpenCall instructions with their callsite. Also
performs tail-call elimination and links Throws with a matching rule to that
rule.

# TODO will optimize jumps and jump-like instructions
"""
function link!(code::IVector, aux::AuxDict)
    callsite = aux[:callsite]
    rules = aux[:rules]
    throws = aux[:throws]
    for i ∈ 1:length(code)
        inst = code[i]
        if inst isa InstructionVec
            continue
        elseif inst.op == IOpenCall
            site = callsite[inst.rule]
            l = site - i
            # Tail call?
            if code[i + 1] == OpReturn
                code[i] = JumpInst(l)
            else
                code[i] = CallInst(l)
            end
        elseif inst.op == IThrow
            if haskey(rules, throws[inst.tag])
                site = callsite[throws[inst.tag]]
                l = site - i
                code[i] = ThrowRecInst(inst.tag, l)
            end
        end
    end
end

"""
    peephole!(code::IVector)

Optimizes jumps, and labeled instructions which point to them.
"""
function peephole!(code::IVector)
    for i ∈ 1:length(code)
        # @label redo
        inst = code[i]
        if inst isa InstructionVec
            continue
        end
        op = inst.op
        if hasfield(typeof(inst), :l) && op != IJump
            l = finallabel(code, i)
            if inst.l ≠ l
                code[i] = relabel(inst, l)
            end
        elseif inst.op == IJump
            l = finallabel(code, i)
            if l ≠ inst.l
                println("indirect jump in program (implement redirect)")
            end
            target = code[i + l]
            t_op = target.op
            if t_op == IReturn
                code[i] = OpReturn
            elseif t_op == IFail
                code[i] = OpFail
            elseif t_op == IFailTwice
                code[i] = OpFailTwice
            elseif t_op == IEnd
                code[i] = OpEnd
            elseif t_op == ICommit || t_op == IPartialCommit || t_op == IBackCommit
                println("instruction $i jumps to $t_op: the complex case")
            end
        end
    end
end

"""
    finallabel(code::IVector, i::Integer)::Int32

Follows all jumps to produce the final target of a labeled instruction.
"""
function finallabel(code::IVector, i::Integer)::Int32
    inst = code[i]
    target = inst.l
    while code[i + target].op == IJump
        target += code[i + target].l
    end
    return target
end


"""
    headfail(patt::Pattern)::Bool

Answer if the pattern fails (should it fail) on the first test.
"""
function headfail(patt::Pattern)::Bool
    if isof(patt, PChar, PAny, PSet, PFalse) return true
    elseif isof(patt, PTrue, PStar, PRunTime, PNot, PBehind, PThrow) return false
    elseif isof(patt, PCapture, PGrammar, PRule, PTXInfo, PAnd) return headfail(patt.val[1])
    # Pretty sure this one is wrong...
    # elseif patt isa PCall return headfail(patt.val[2])

    # LPeg does this, I still don't understand it
    elseif patt isa PSeq
        if !nofail(patt.val[2])
            return false
        else
            return headfail(patt.val[1])
        end
    # TODO there's also the disjoint-head version of headfail for PChoice
    elseif patt isa PChoice return all(p -> headfail(p), patt.val)
    else @error "headfail not defined for $(typeof(patt))"
    end
end

"""
    nofail(patt::Pattern)::Bool

Answer if the pattern cannot fail.
"""
function nofail(patt::Pattern)::Bool
    if patt isa PTrue return true
    elseif patt isa PStar
        if patt.n ≤ 0 return true
        else return false
        end
    elseif isof(patt, PChar, PAny, PSet, PFalse, PThrow) return false
    # TODO raise error here, we've resolved all of these
    elseif patt isa POpenCall return false
    # TODO this should be nofail(patt.ref) but we need to watch for recursion
    elseif patt isa PCall return false
    # aka patt.aux[:nofailcheck] = true, top check for no-failability, set it
    # back to nothing/remove the key

    # !nofail but nullable (circumstances differ, e.g. inherent vs. body)
    elseif isof(patt, PNot, PBehind, PRunTime) return false
    # PSeq nofail if entire sequence is nofail
    elseif patt isa PSeq return all(nofail, patt.val)
    # PChoice nofail if any branch is nofail
    elseif patt isa PChoice return any(nofail, patt.val)
    # Wrapped patterns which nofail based on the pattern they enclose
    elseif isof(patt, PCapture, PGrammar, PRule, PTXInfo, PAnd, PMark, PCheck) return nofail(patt.val[1])
    else @error "nofail not defined for $(typeof(patt))"
    end
end

"""
    nullable(patt::Pattern)::Bool

Answer if the pattern can consume no input.
"""
function nullable(patt::Pattern)::Bool
    if isof(patt, PNot, PBehind, PAnd) return true
    elseif isof(patt, PRunTime) return nullable(patt.val[1])
    elseif nofail(patt) return true
    else return false
    end
end

"""
    fixedlen(patt::Pattern)::Union{Integer,Bool}

If a pattern matches a fixed length, return that length,
otherwise return `false`.
"""
function fixedlen(patt::Pattern, seen=IdDict())::Union{Integer,Bool}
    if haskey(seen, patt) # Recursive patterns cannot be fixedlen
        return false
    end
    if isof(patt, PChar, PSet)
        return 1
    elseif isa(patt, PAny)
        return patt.val
    elseif isof(patt, PTrue, PFalse, PAnd, PNot, PBehind)
        return 0
    elseif isof(patt, PStar, PRunTime, POpenCall, PThrow)
        return false
    elseif isa(patt, PCapture)
        return fixedlen(patt[1], seen)
    elseif isa(patt, PRule)
        seen[patt] = true
        return fixedlen(patt[1], seen)
    elseif patt isa PCall
        return fixedlen(patt.ref, seen)
    elseif patt isa PSeq
        len = 0
        for p in patt.val
            l = fixedlen(p, seen)
            if l === false
                return false
            else
                len += l
            end
        end
        return len
    elseif patt isa PChoice
        len = fixedlen(patt.val[1], seen)
        if len === false
            return false
        end
        for i = 2:length(patt.val)
            l = fixedlen(patt.val[i], seen)
            if l === false || len != l
                return false
            end
        end
        return len
    else
        error("unexpected pattern type $(typeof(patt))")
    end
end

"""
    firstset(patt::Pattern)::Pattern

Computes the "firstset" of a `Pattern`, which may be used to test if the
whole pattern will fail, returning it in Instruction form.  This is in the
form of a Pattern, and will be PTrue if `patt` matches `""`.
"""
function firstset(patt::Pattern)::Pattern
    # TODO this is incompatible with direct left recursion,
    # this algorithm will hang and we need to figure out how
    # this is computed for a ← a b
    if isof(patt, PChar, PSet)
        return patt
    elseif patt isa PAny # Only the first char in a PAny is the test
        return PAny(!)
    elseif isof(patt, PTrue, PThrow)
        return PTrue()
    elseif patt isa PFalse
        return PSet("")
    elseif isof(patt, PCapture, PMark, PCheck, PRule)
        return firstset(patt[1])
    elseif isof(patt, PCall)
        return firstset(patt.ref)
    elseif patt isa PChoice
        firsts = map(firstset, patt.val)
        if any((p)-> p isa PAny, firsts)
            return PAny(1)
        elseif any((p)-> p isa PTrue, firsts)
            return PTrue()
        else # | will consolidate PChar and PSets into their union
            return reduce(|, firsts)
        end
    elseif patt isa PSeq
        if !nullable(patt[1])
            return firstset(patt[1])
        else
            f1 = firstset(patt[1])
            if f1 isa PTrue
                return f1
            end
            error("encountered 'strange' PSeq case: $(patt.val)")
            return patt
        end
    elseif patt isa PStar
        if patt.n ≤ 0
            return PTrue()
        else
            return firstset(patt[1])
        end
    elseif patt isa PAnd
        # lpeg does something weird with the follow-set here
        # but isn't it just ~"123" == S"1" firstset? Looks like it!
        return firstset(patt[1])
    elseif patt isa PNot
        # We should use a proper complement here. But for
        # efficiency reasons (which we can solve, later) we
        # treat this as "can't make a firstset", which is PTrue.
        # TODO revisit this? I'm not convinced we're doing this function right.
        return PTrue()
    else
        error("firstset NYI for pattern type $(typeof(patt))")
    end
end

function isof(patt::Pattern, types::DataType...)
    for t in types
        if patt isa t
            return true
        end
    end
    return false
end

function makemasks(bvec::Bits{Int128})
    masks =reinterpret(UInt64, [bvec.chunk])
    return InstructionVec(masks[1]), InstructionVec(masks[2])
end

"""
    vecsforstring(str::Union{AbstractString, Vector{AbstractChar}})::Tuple{Union{Bits, Nothing},Union{Dict, Nothing}}

Take the `.val` of a PSet and break it down into bitvectors which compactly and
quickly test for those characters.

Return `(ascii, higher)` where `ascii` is all one-byte utf8 characters and higher is
a somewhat complex dict of bitvectors useful for detecting practical multibyte ranges
and sets.
"""
function makebitvectors(set::CharSet)::Tuple{Union{Bits, Nothing},Union{Dict, Nothing}}
    bvec = nothing
    prefix_map = nothing
    for elem in Iterators.flatten(set)
        bvec, prefix_map = bvec_char!(bvec, prefix_map, elem)
    end
    return bvec, prefix_map
end

function bvec_char!(bvec::Union{Bits,Nothing}, prefix_map::Union{OrderedDict,Nothing}, char::AbstractChar)
    if char <= Char(127)
        if bvec === nothing
            bvec = Bits{Int128}(0)
        end
        bvec[UInt(char)+1] = true
    else
        if prefix_map === nothing
            prefix_map = OrderedDict{UInt8,Union{OrderedDict,Bits{Int64}}}()
        end
        prefix!(prefix_map, codeunits(string(char))...)
    end
    return bvec, prefix_map
end

function prefix!(map::OrderedDict, b1::UInt8, b2::UInt8)
    b2 &= 0b00111111
    if haskey(map, b1)
        map[b1][b2 + 1] = true
    else
        map[b1] = Bits{Int64}(0)
        map[b1][b2 + 1] = true
    end
end

function prefix!(map::OrderedDict, b1::UInt8, b2::UInt8, b3::UInt8)
    if !haskey(map, b1)
        map[b1] = OrderedDict{UInt8,Bits{Int64}}()
    end
    prefix!(map[b1], b2, b3)
end

function prefix!(map::OrderedDict, b1::UInt8, b2::UInt8, b3::UInt8, b4::UInt8)
    if !haskey(map, b1)
        map[b1] = OrderedDict{UInt8,Union{Bits{Int64},OrderedDict{UInt8,Bits{Int64}}}}()
    end
    prefix!(map[b1], b2, b3, b4)
end


"""
    encode_multibyte_set!(c::IVector, pre::Dict)

Encode instructions to recognize a set containing multibyte characters.
"""
function encode_multibyte_set!(c::IVector, bvec::Union{Bits{Int128},Nothing}, pre::Dict)
    if bvec !== nothing
        push!(c, HoldInst(ILeadSet))  # -> end of MultiSet, after OpFail
        hold = length(c)
        push!(c, OpNoOp, OpNoOp)
    end
    leadidx = nothing  # for if we put a hold for a LeadMultiInst
    # We need to vectorize pre, for a 1-to-1 match with the code
    prevec = []
    # Store offsets as we find them
    sites = IdDict{Any,Integer}()
    # Vectors go last
    vecs = Bits{Int64}[]
    # next set of Dicts, if any
    seconds = []
    # compute headset if there are four or more lead bytes
    if length(pre) ≥ 4
        push!(c, HoldInst(ILeadMulti))
        leadidx = length(c)
        push!(c, OpNoOp)  # room for the instruction vector
    end
    heads = UInt8[]
    for pair in pre
        push!(heads, pair.first & 0b00111111)
        push!(prevec, pair)
        if pair.second isa OrderedDict
            push!(seconds, pair.second)
        else
            push!(vecs, pair.second)
        end
    end
    # if leadidx === nothing
        push!(prevec, OpFail)
    # end
    if !isempty(seconds)
        # thirds?
        thirds = []
        for dict in seconds
            sites[dict] = length(prevec) + 1
            for pair in dict
                push!(prevec, pair)
                if pair.second isa OrderedDict
                    push!(thirds, pair.second)
                else
                    push!(vecs, pair.second)
                end
            end
            push!(prevec, OpFail)
        end
        if !isempty(thirds)
            for dict in thirds
                sites[dict] = length(prevec) + 1
                for pair in dict
                    push!(prevec, pair)
                    push!(vecs, pair.second)
                end
                push!(prevec, OpFail)
            end
        end
    end
    for vec in vecs
        push!(prevec, vec)
        sites[vec] = length(prevec)
        push!(prevec, OpNoOp)
    end
    failidx = nothing  # need this for possible head test
    for (idx, elem) in enumerate(prevec)
        if elem == OpNoOp
            continue
        elseif elem isa Pair
            if elem.first isa UInt8
                l = sites[elem.second] - idx
                push!(c, LeadByteInst(elem.first, l))
            end
        elseif elem == OpFail
            if failidx === nothing
                failidx = idx
            end
            push!(c, OpFail)
        elseif elem isa Bits{Int64}
            push!(c, MultiVecInst(length(prevec) - idx + 1))
            push!(c, InstructionVec(elem))
        end
    end
    if failidx === nothing
        failidx = length(prevec)
        push!(c, OpFail)
    end
    if bvec !== nothing
        @assert c[hold] isa HoldInst "HoldInst not found at 1"
        c[hold] = LeadSetInst(length(c))
        low, high = makemasks(bvec)
        c[hold+1] = low
        c[hold+2] = high
    end
    if leadidx !== nothing
        bvec = Bits{Int64}(0)
        for char in heads
            bvec[char + 1] = true
        end
        c[leadidx] = LeadMultiInst(failidx)
        c[leadidx + 1] = InstructionVec(bvec)
    end
end
