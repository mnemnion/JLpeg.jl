# Instruction compiling for Patterns

"Instruction opcodes for JLpeg VM"
@enum Opcode::UInt8 begin
    IAny        # if no char, fail
    IChar       # if char != aux, fail
    ISet        # if char not in buff, fail
    ILeadSet    # ASCII lead of MultiSet
    IByte       # Test the next byte at vm.s
    IMultiVec   # match a set of end bytes of multibyte char
    ILeadMulti  # Extended lead of MultiSet
    ITestAny    # in no char, jump to 'offset'
    ITestChar   # if char != aux, jump to 'offset'
    ITestSet    # if char not in buff, jump to 'offset'
    INotSet     # Predicate, fails if set matches, doesn't advance s
    INotChar    # Predicate instruction for !P'char'
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
    n::UInt32
    op::Opcode
end
AnyInst(n::Integer) = n ≥ 0 ? AnyInst(UInt32(n), IAny) : error("n must be a natural number")

"Not yet in use"
struct TestAnyInst <: Instruction
    l::Int32
    n::UInt16
    op::Opcode
end
TestAnyInst(n::UInt32, l::Integer) = TestAnyInst(Int32(l), UInt16(n), ITestAny)

struct CharInst <: Instruction
    c::Char
    op::Opcode
end
CharInst(c::AbstractChar) = CharInst(Char(c), IChar)

struct NotCharInst <: Instruction
    c::Char
    op::Opcode
end
NotCharInst(c::AbstractChar) = NotCharInst(Char(c), INotChar)


"Not yet in use"
struct TestCharInst{C} <: Instruction
    l::Int32
    c::Char
    op::Opcode
end
TestCharInst(c::AbstractChar, l::Integer) = TestCharInst(Int32(l),Char(c), ITestChar)

struct SetInst <: Instruction
    vec::Int128
    l::Int32
    op::Opcode
end

SetInst(vec::Bits{Int128}) = SetInst(vec.chunk::Int128, Int32(1), ISet)
SetInst(vec::Bits{Int128}, l::Integer) = SetInst(vec.chunk::Int128, Int32(l)ISet)
SetInst(set::SetInst, l::Integer) = SetInst(set.vec, Int32(l), ISet,)
LeadSetInst(vec::Bits{Int128}, l::Integer) = SetInst(vec.chunk::Int128, Int32(l), ILeadSet)
LeadSetInst(vec::Int128, l::Integer) = SetInst(vec, l, ILeadSet)

struct NotSetInst <: Instruction
    vec::Int128
    l::Int32
    op::Opcode
end
NotSetInst(vec::Bits{Int128}, l::Int32) = NotSetInst(vec.chunk::Int128, l, INotSet)
NotSetInst(vec::Int128, l::Int32) = NotSetInst(vec, l, INotSet)


"Not yet in use"
struct TestSetInst <: Instruction
    vec::Int128
    l::Int32
    op::Opcode
end
TestSetInst(vec::Bits{Int128}, l::Integer) = TestSetInst(vec.chunk, Int32(l), ITestSet)

struct MultiVecInst <: Instruction
    vec::Int64
    l::Int32
    op::Opcode
end
MultiVecInst(vec::Bits{Int64}, l::Integer) = MultiVecInst(vec.chunk, Int32(l), IMultiVec)

struct LeadMultiInst <: Instruction
    vec::Int64
    l::Int32
    op::Opcode
end
LeadMultiInst(vec::Bits{Int64}, l::Integer) = LeadMultiInst(vec.chunk, Int32(l), ILeadMulti)

struct ByteInst <: Instruction
    l::Int32
    b::UInt8
    op::Opcode
end
ByteInst(b::UInt8, l::Integer) = ByteInst(Int32(l), b, IByte)

struct BehindInst <: Instruction
    n::UInt32
    op::Opcode
end
BehindInst(n::Integer) = n ≥ 0 ? BehindInst(n, IBehind) : error("n must be a natural number")

struct LabelInst <: Instruction
    l::Int32
    op::Opcode
end
LabelInst(op::Opcode, l::Integer) = LabelInst(l, op)

struct ChoiceInst <: Instruction
    l::Int32
    op::Opcode
end
ChoiceInst(l::Integer) = ChoiceInst(Int32(l), IChoice)

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

"A placeholder for a (usually labeled) Instruction"
struct HoldInst <: Instruction
    op::Opcode
end

# To be continued...

abstract type CloseRunTimeInst end
abstract type MarkInstruction end
abstract type CheckInstruction end

struct CaptureInst <: Instruction
    op::Opcode
    kind::CapKind
    l::Int16
    tag::UInt16
end
CaptureInst(op::Opcode, kind::CapKind) = CaptureInst(op, kind, Int16(0), UInt16(0))
CaptureInst(op::Opcode, kind::CapKind, tag::UInt16) = CaptureInst(op, kind, Int16(0), tag)

OpenCaptureInst(kind::CapKind) = CaptureInst(IOpenCapture, kind, UInt16(0))
OpenCaptureInst(kind::CapKind, tag::UInt16) = CaptureInst(IOpenCapture, kind, tag)
CloseCaptureInst(kind::CapKind, tag::UInt16) = CaptureInst(ICloseCapture, kind, Int16(0), tag)
FullCaptureInst(kind::CapKind, l::Integer, tag::UInt16) = CaptureInst(IFullCapture, kind, Int16(l), tag)


struct ThrowInst <: Instruction
    tag::UInt16
    op::Opcode
end
ThrowInst(tag::UInt16) = ThrowInst(tag, IThrow)

struct ThrowRecInst <: Instruction
    l::Int32
    tag::UInt16
    op::Opcode
end
ThrowRecInst(tag::UInt16, l::Integer) = ThrowRecInst(Int32(l), tag, IThrowRec)


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

const IVectored = Union{SetInst,NotSetInst,TestSetInst,MultiVecInst,LeadMultiInst}
const IASCIISet = Union{SetInst,NotSetInst,TestSetInst}
const IVec128 = Union{SetInst,NotSetInst,TestSetInst}
const IVec64 = Union{MultiVecInst,LeadMultiInst}

# Code borrowed from BitPermutations.jl for converting vector instructions
#   into bit types

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

function Base.getindex(inst::IVec128, i::Integer)
    @boundscheck i ≤ 128 || throw(BoundsError)
    u = one(Int128) << shift_safe(Int128, i - 1)
    return !iszero(inst.vec & u)
end

function Base.getindex(inst::IVec64, i::Integer)
    @boundscheck i ≤ 64 || throw(BoundsError)
    u = one(Int64) << shift_safe(Int64, i - 1)
    return !iszero(inst.vec & u)
end

Base.keys(::IVec64) = 1:64
Base.keys(::IVec128) = 1:128


function Base.iterate(inst::IASCIISet, i::Integer)
    i += 1
    i > 128 && return nothing
    if inst[i]
        return UInt8(i-1), i
    else
        return false, i
    end
end

function Base.iterate(inst::LeadMultiInst, i::Integer)
    i += 1
    i > 128 && return nothing

    if @inbounds inst[i]
        return UInt8(i-1) | 0b11000000, i
    else
        return false, i
    end
end

function Base.iterate(inst::MultiVecInst, i::Integer)
    i += 1
    i > 64 && return nothing
    if @inbounds inst[i]
        return UInt8(i-1) | 0b10000000, i
    else
        return false, i
    end
end

Base.iterate(inst::IVectored) = iterate(inst, 0)

Base.count_ones(inst::IVectored) = count_ones(inst.vec)

# # #

# ## Compilers

"""
    compile!(patt::Pattern)::Pattern

Compile a [`Pattern`](@ref).

Translate the Pattern to Instruction codes, appending them to
the `code` field and returning same.  Performs various optimizations
in the process.

In most cases, the return value of `compile!` is the same Pattern passed in.
The exceptions are special cases of primitive types, but be sure to reassign
the return value to the variable bound to the original for the general case.
"""
function compile!(patt::Pattern)::Pattern
    if isempty(patt.code)
        if patt.val isa PVector
            for (idx, val) in enumerate(patt)
                patt.val[idx] = compile!(val)
            end
        end
        _compile!(patt)
    else
        patt
    end
end


"""
    prepare!(patt::Pattern)::Pattern

Prepare a pattern for matching.
"""
function prepare!(patt::Pattern)::Pattern
    compile!(patt)
end

function prepare!(patt::PAuxT)::Pattern
    if haskey(patt.aux, :prepared)
        return patt
    end
    patt = compile!(patt)
    if !isa(patt, PAuxT)
        return patt
    end
    if haskey(patt.aux, :prepared)
        return patt
    end
    # We're replacing it with this:
    prewalkpatt!(patt, patt.aux) do p, aux
        if p isa PCapture
            caps = getordict!(aux, :caps)
            caps[p.tag] = p.cap
        elseif p isa PThrow
            throws = getordict!(aux, :throws)
            throws[p.tag] = p.val
        end
    end
    patt.aux[:prepared] = true
    return patt
end

"""
    getordict!(dict::Dict, key::Symbol)

Retrieves a Dict from `dict[:key]`, or if it doesn't exist, creates it and
returns it after adding it to dict.
"""
function getordict!(dict::Dict, key::Symbol)
    if haskey(dict, key)
        dict[key]::Dict
    else
        dict[key] = Dict()
    end
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

const PCopyChild = Union{PNot,PStar,PCapture}

"""
    hoist!(parent::Pattern, child::Pattern)::IVector

The generic `hoist!` returns the code of `child`, this is specialized for
some parent/child contexts where the code must be copied or rewritten.
"""
function hoist!(parent::Pattern, child::Pattern)::IVector
    if parent isa PCopyChild
        copy(child.code)
    else
        child.code
    end
end

function _compile!(patt::Pattern)::Pattern
    error("Not Yet Implemented for $(typeof(patt))")
end

function _compile!(patt::PAny)::Pattern
    # Optimize away P(0) to P(true) to avoid
    # inefficent VM instruction
    if patt.val == 0
        return compile!(PTrue())
    end
    push!(patt.code, AnyInst(patt.val), OpEnd)
    return patt
end

function _compile!(patt::PChar)::Pattern
    push!(patt.code, CharInst(patt.val), OpEnd)
    return patt
end

function _compile!(patt::PTrue)::Pattern
    push!(patt.code, OpEnd)
    return patt
end

function _compile!(patt::PFalse)::Pattern
    push!(patt.code, OpFail, OpEnd)
    return patt
end

function _compile!(patt::POpenCall)::Pattern
    push!(patt.code, OpenCallInst(patt.val), OpEnd)
    return patt
end

function _compile!(patt::PCall)::Pattern
    push!(patt.code, OpenCallInst(patt.val), OpEnd)
    return patt
end

function _compile!(patt::PBehind)::Pattern
    @assert length(patt.val) == 1 "too many patterns in PBehind"
    len = fixedlen(patt.val[1])
    if len === false
       throw(PegError("in B(patt), patt must be of fixed length, not a $(typeof(patt.val[1]))"))
    elseif len == 0  # Return the pattern
        return return patt.val[1]
    end
    push!(patt.code, BehindInst(len))
    pcode = hoist!(patt, patt[1])
    append!(patt.code, pcode)
    @assert patt.code[end] == OpEnd
    return patt
end

function _compile!(patt::PSet)::Pattern
    # Specialize the empty set
    # We'll turn into a Jump when we have the requisite info
    if isempty(patt.val)  # A valid way of saying "fail"
        return compile!(PFalse())
    end
    # Specialize one-char sets into PChar
    if sizeof(patt.val) ≤ 4 && length(patt.val) == 1 && patt.val[1] isa AbstractChar
        return compile!(PChar(first(patt.val)))
    end
    bvec, prefix_map = makebitvectors(patt.val)
    if bvec !== nothing && prefix_map === nothing
        push!(patt.code, SetInst(bvec))
    else
        encode_multibyte_set!(patt.code, bvec, prefix_map)
    end
    pushEnd!(patt.code)
    return patt
end

function _compile!(patt::PAnd)::Pattern
    @assert length(patt.val) == 1 "enclosing rule PAnd has more than one child"
    c = patt.code
    code = hoist!(patt, patt.val[1])
    l = length(code) + 1  # 2 -> Choice, BackCommit
    push!(c, PredChoiceInst(l))
    append!(c, code)
    c[end] = BackCommitInst(2)
    push!(c, OpFail, OpEnd)  # Choice target
    return patt
end

function _compile!(patt::PNot)::Pattern
    @assert length(patt.val) == 1 "enclosing rule PNot has more than one child"
    c = patt.code
    code = hoist!(patt, patt[1])
    # We can remove captures from PNot patterns,
    # which never succeed (except match-time captures)
    # TODO unlikely optimization unless we inline simple calls (which we should)
    for (idx, inst) in enumerate(code)
        if ( ( inst.op == IOpenCapture
            || inst.op == ICloseCapture
            || inst.op == IFullCapture )
                && !(inst.kind == Cruntime))
            code[idx] = OpNoOp
        end
    end
    # We can turn ASCII sets into INotSet, which will
    # therefore match multibyte chars
    if length(code) == 2 && code[1].op == ISet && code[2] == OpEnd
        push!(c, NotSetInst(code[1].vec, code[1].l), OpEnd)
        return patt
    end
    # We do the same for PChar
    if patt.val[1] isa PChar
        push!(c, NotCharInst(patt.val[1].val), OpEnd)
        return patt
    end
    trimEnd!(code)
    l = length(code) + 2  # 3 -> FailTwice, next
    push!(c, PredChoiceInst(l))
    append!(c, code)
    push!(c, OpFailTwice)
    pushEnd!(c)  # Choice target
    return patt
end

function _compile!(patt::PDiff)::Pattern
    v = patt.val
    # TODO Set complementation is rather complex with MultiSets!
    compile!(PSeq(PNot(patt.val[2]), patt.val[1]))
end

function _compile!(patt::PSeq)::Pattern
    # As an optimization, a Seq of one member can just be that member
    if length(patt.val) == 1
        return patt.val[1]
    end
    c = patt.code
    for p in patt.val
        code = hoist!(patt, p)
        append!(c, code)
        trimEnd!(c)
        # optimizations?
    end
    pushEnd!(c)
    return patt
end

function _compile!(patt::PStar)::Pattern
    # TODO figure out when TestChar etc come into play
    #
    # bad things happen when val is  a PStar, specifically
    # when the inner is optional, e.g. ("ab"?)*, so we check for this
    # and fix it when we need to.
    p = patt[1]
    if typeof(p) == PStar && p.n ≤ 0
        if p.n ≤ -1 && (patt.n == 0 || patt.n == 1)
            # "As many optionals as you want, as long as you match one" aka P^0
            return compile!(p.val[1]^0)
        elseif p.n == 0 && (patt.n == 0 || patt.n == 1)
            # Both of these mean "match as few as zero or as many as you can",
            # Which is what p means already.
            return p
        elseif p.n == 0 && patt.n == -1
            # Same outcome as the above, but this time it's an optimization,
            # the code actually works fine, but the -1 isn't doing anything here
            return p
        end
    end
    # Nullables: pointless for patt.n < 0, infinite loop otherwise, fail:
    if nullable(p)
        error("this $(typeof(p)) is nullable, repetition is not allowed")
    end
    c = patt.code
    code = hoist!(patt, p)
    trimEnd!(code)
    # TODO TestPattern optimization goes here, pass a flag to `addstar!`
    if patt.n == 0
        addstar!(c, code)
    elseif patt.n ≥ 1
        for _ = 1:patt.n
            append!(c, code)
        end
        addstar!(c, code)
    elseif patt.n == -1
        push!(c, ChoiceInst(length(code) + 2))
        append!(c, code)
        push!(c, CommitInst(1))
    elseif patt.n < -1
        _choice = length(c) + 1
        push!(c, OpNoOp)
        for _ = patt.n:-2
            append!(c, code)
            push!(c, PartialCommitInst(1))
        end
        append!(c, code)
        push!(c, CommitInst(1))
        c[_choice] = ChoiceInst(length(c) - _choice)
    else
        error("unreachable, logic is broken")
    end
    pushEnd!(c)
    return patt
end

function addstar!(c::IVector, code::Vector{})
    l = length(code)
    if l == 1 && code[1].op == ISet
        # Span instruction
        push!(c, LeadSetInst(code[1].vec, 0))
        return
    end
    push!(c, ChoiceInst(l + 2)) # Choice + PartialCommit
    append!(c, code)
    push!(c, PartialCommitInst(-l))
    # Choice Target
end

# TODO Change PChoice to use Ends, write hoist! for child::PChoice which makes them CommitInst

function _compile!(patt::PChoice)::Pattern
    c = patt.code
    for (idx, p) in enumerate(patt)
        pcode = copy(p.code)
        trimEnd!(pcode)
        if idx == length(patt.val)
            append!(c, pcode)
            break
        end
        len = length(pcode)
        push!(c, ChoiceInst(len + 2))  # +2 == Choice and Commit
        append!(c, pcode)
        push!(c, HoldInst(ICommit))
    end
    pushEnd!(c)

    for (idx, inst) in enumerate(c)
        if isa(inst, HoldInst) && inst.op == ICommit
            c[idx] = CommitInst(length(c) - idx)
        end
    end

    return patt
end

function _compile!(patt::PCapture)::Pattern
    # Special-case Cp()
    patt.aux[:caps] = caps = get(patt.aux, :caps, Dict())  # TODO remove
    if patt.kind == Cposition
        full = FullCaptureInst(Cposition, 0, patt.tag)
        caps[patt.tag] = patt.cap  # TODO remove
        push!(patt.code, full, OpEnd)
        return patt
    end
    c = patt.code
    ccode = hoist!(patt, patt[1])
    # TODO full capture optimization
    trimEnd!(ccode)
    push!(c, OpenCaptureInst(patt.kind, patt.tag))
    append!(c, ccode)
    close = CloseCaptureInst(patt.kind, patt.tag)
    caps[patt.tag] = patt.cap # TODO remove
    push!(c, close)
    pushEnd!(c)
    return patt
end

function _compile!(patt::PThrow)::Pattern
    # Grammars may recode this as ThrowRecInst
    push!(patt.code, ThrowInst(patt.tag), OpEnd)
    return patt
end

function _compile!(patt::PRule)::Pattern
    c = patt.code
    append!(c, hoist!(patt, patt[1]))
    pushEnd!(c)
    return patt
end


"""
    compile!(patt::PGrammar)::Pattern

Compiles _and prepares_ a Grammar.
"""
compile!(patt::PGrammar)::Pattern = _compile!(patt)

function _compile!(patt::PGrammar)::Pattern
    # TODO we want to cache this eventually but the code is... in flux
    empty!(patt.code)
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
    patt = inwalkpatt!(patt, aux) do p::Pattern, a::AuxDict
        if p isa PCapture
            a[:caps][p.tag] = p.cap
        elseif p isa PThrow
            a[:throws][p.tag] = p.val
        elseif p isa POpenCall
            if haskey(a[:rules], p.val)
                # Replace it with a call having the ref
                return PCall(p, a[:rules][p.val])
            else
                error(PegError("$(patt.start) has no rule $(p.val)"))
            end
        end
        return p
    end
    aux[:seen] = Symbol[]
    recursepattern!(patt, aux)
    c = patt.code
    push!(c, CallInst(2))
    push!(c, OpEnd)  # TODO hold this an jump to actual end
    for rulename in aux[:seen]
        rule = compile!(aux[:rules][rulename])
        aux[:callsite][rulename] = length(c) + 1
        append!(c, rule.code)
        trimEnd!(c)
        push!(c, OpReturn)
    end
    push!(c, OpEnd)
    link!(c, aux)
    delete!(aux, :rules)
    aux[:prepared] = true
    return patt
end

"""
    inwalkpatt!(λ::Function, patt::Pattern, aux::AuxDict)::Pattern

Applies `λ`, which must return a Pattern, to `patt, aux`, then recursively
inwalks any sub-patterns of `patt` in `.val`, replacing them with the returned
pattern (which may be, and often is, the same).
"""
function inwalkpatt!(λ::Function, patt::Pattern, aux::AuxDict)::Pattern
    patt = λ(patt, aux)
    if patt.val isa PVector
        for (idx, p) in enumerate(patt)
            patt.val[idx] = inwalkpatt!(λ, p, aux)
        end
    end
    return patt
end

"""
    recursepattern!(patt::Pattern, gaux::AuxDict)::Pattern

Recursively walk the pattern.
"""
function recursepattern!(patt::Pattern, gaux::AuxDict)::Pattern
    if patt isa PPrimitive
        return _compile!(patt)
    end
    if patt isa PRule
        if patt.aux[:visiting]
            @error "shouldn't see a rule while visiting it ($(patt.name))"
            patt.aux[:recursive] = true  # May as well avoid overflowing the stack if this changes
            return patt
        elseif patt.aux[:walked]
            return patt
        else
            push!(gaux[:seen], patt.name)
            patt.aux[:visiting] = true
        end
    end
    if !isa(patt.val, Vector)
        @error "missing primitive maybe? $(typeof(patt)).val isa $(typeof(patt.val))"
    end
    hascap = false
    hascall = false
    for (idx, p) in enumerate(patt)
        if p isa PCall
            hascall = true
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
                    p.ref.aux[:hascall] = get(p.ref[1].aux, :hascall, false)
                end
            end
        elseif p isa PCapture
            hascap = true
        end
        patt.val[idx] = recursepattern!(p, gaux)
    end
    patt.aux[:visiting] = false
    patt.aux[:walked] = true
    if !haskey(patt.aux, :hascall)
        patt.aux[:hascall] = hascall
        patt.aux[:hascap] = hascap
    end
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
        if inst.op == IOpenCall
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
    peephole!(code)
end

"""
    peephole!(code::IVector)

Optimizes jumps, and labeled instructions which point to them.
"""
function peephole!(code::IVector)
    for i ∈ 1:length(code)
        # @label redo
        inst = code[i]
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
            if t_op == IReturn || t_op == IFail || t_op == IFailTwice || t_op == IEnd
                println("instruction $i is a Jump to be transformed to $t_op")
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
    elseif isof(patt, PCapture, PGrammar, PRule, PTXInfo, PAnd) return nofail(patt.val[1])
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
        return 0
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
                len += 1
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

function isof(patt::Pattern, types::DataType...)
    for t in types
        if patt isa t
            return true
        end
    end
    return false
end

"""
    vecsforstring(str::Union{AbstractString, Vector{AbstractChar}})::Tuple{Union{Bits, Nothing},Union{Dict, Nothing}}

Take a string, or a vector of characters, and break it down into bitvectors which
compactly and quickly test for those characters.

Return `(ascii, higher)` where `ascii` is all one-byte utf8 characters and higher is a somewhat
complex dict of bitvectors useful for detecting practical multibyte ranges and sets.
"""
function makebitvectors(set::Settable)::Tuple{Union{Bits, Nothing},Union{Dict, Nothing}}
    bvec = nothing
    prefix_map = nothing
    for elem in set
        if elem isa Char
            bvec, prefix_map = bvec_char!(bvec, prefix_map, elem)
        elseif elem isa Tuple
            for char in elem[1]:elem[2]
                bvec, prefix_map = bvec_char!(bvec, prefix_map, char)
            end
        end
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
    end
    failidx = nothing  # need this for possible head test
    for (idx, elem) in enumerate(prevec)
        if elem isa Pair
            if elem.first isa UInt8
                l = sites[elem.second] - idx
                push!(c, ByteInst(elem.first, l))
            end
        elseif elem == OpFail
            if failidx === nothing
                failidx = idx
            end
            push!(c, OpFail)
        elseif elem isa Bits{Int64}
            push!(c, MultiVecInst(elem, length(prevec) - idx + 1))
        end
    end
    if failidx === nothing
        failidx = length(prevec)
        push!(c, OpFail)
    end
    if bvec !== nothing
        @assert c[1] isa HoldInst "HoldInst not found at 1"
        c[1] = LeadSetInst(bvec, length(c))
    end
    if leadidx !== nothing
        bvec = Bits{Int64}(0)
        for char in heads
            bvec[char + 1] = true
        end
        c[leadidx] = LeadMultiInst(bvec, failidx)
    end
    push!(c, OpEnd)
end

"""
    trimEnd!(code::IVector)

Remove an OpEnd, if present.
"""
function trimEnd!(code::IVector)
    if code[end] == OpEnd
        pop!(code)
    end
end

"""
    pushEnd!(code::IVector)

Put an OpEnd, if needed.
"""
function pushEnd!(code::IVector)
    if code[end] != OpEnd
        push!(code, OpEnd)
    end
end