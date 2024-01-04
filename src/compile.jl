# Instruction compiling for Patterns

"Instruction opcodes for JLpeg VM"
@enum Opcode::UInt8 begin
    IAny        # if no char, fail
    IChar       # if char != aux, fail
    ISet        # if char not in buff, fail
    ILeadSet    # ASCII lead of MultiSet
    IMultiSet   # Multibyte vectored sets
    ITestAny    # in no char, jump to 'offset'
    ITestChar   # if char != aux, jump to 'offset'
    ITestSet    # if char not in buff, jump to 'offset'
    ISpan       # read a span of chars in buff
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
end
AnyInst(n::Integer) = n ≥ 0 ? AnyInst(IAny, UInt32(n)) : error("n must be a natural number")

struct CharInst <: Instruction
    op::Opcode
    c::AbstractChar
end
CharInst(c::AbstractChar) = CharInst(IChar, c)

struct SetInst <: Instruction
    op::Opcode
    vec::BitVector
    l::Int32
end
SetInst(vec::BitVector) = SetInst(ISet, vec, Int32(1))
SetInst(vec::BitVector, l::Integer) = SetInst(ISet, vec, Int32(l))
SetInst(set::SetInst, l::Integer) = SetInst(ISet, set.vec, Int32(l))

LeadSetInst(vec::BitVector, l::Integer) = SetInst(ILeadSet, vec, Int32(l))

struct MultiSetInst <: Instruction
    op::Opcode
    lead::Vector{UInt8}
    vec::BitVector
    l::Int32
end
function MultiSetInst(lead::Vector{UInt8}, vec::BitVector, l::Integer)
    MultiSetInst(IMultiSet, lead, vec, Int32(l))
end
function MultiSetInst(multi::MultiSetInst, l::Integer)
    MultiSetInst(IMultiSet, multi.lead, multi.vec, Int32(l))
end

struct BehindInst <: Instruction
    op::Opcode
    n::UInt32
end
BehindInst(n::Integer) = n ≥ 0 ? BehindInst(IBehind, n) : error("n must be a natural number")

struct LabelInst <: Instruction
    op::Opcode
    l::Int32
end

struct ChoiceInst <: Instruction
    op::Opcode
    l::Int32
    n::Int32
end
ChoiceInst(l::Integer) = ChoiceInst(IChoice, Int32(l), 0)
ChoiceInst(l::Integer, n::Integer) = ChoiceInst(IChoice, Int32(l), Int32(n))

PredChoiceInst(l::Integer) = LabelInst(IPredChoice, Int32(l))
CommitInst(l::Integer) = LabelInst(ICommit, Int32(l))
CallInst(l::Integer) = LabelInst(ICall, Int32(l))
JumpInst(l::Integer) = LabelInst(IJump, Int32(l))
PartialCommitInst(l::Integer) = LabelInst(IPartialCommit, Int32(l))
BackCommitInst(l::Integer) = LabelInst(IBackCommit, Int32(l))

"Not yet in use"
struct TestAnyInst <: Instruction
    op::Opcode
    n::UInt32
    l::Int32
end
TestAnyInst(n::UInt32, l::Int32) = TestAnyInst(ITestAny, n, Int32(l))

"Not yet in use"
struct TestCharInst <: Instruction
    op::Opcode
    c::AbstractChar
    l::Int32
end
TestCharInst(c::AbstractChar, l::Int32) = TestCharInst(ITestChar, c, Int32(l))

"Not yet in use"
struct TestSetInst <: Instruction
    op::Opcode
    vec::BitVector
    l::Int32
end
TestSetInst(vec::BitVector, l::Int32) = TestSetInst(ITestSet, vec, Int32(l))

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

abstract type SpanInst end
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
    op::Opcode
    tag::UInt16
end
ThrowInst(tag::UInt16) = ThrowInst(IThrow, tag)

struct ThrowRecInst <: Instruction
    op::Opcode
    tag::UInt16
    l::Int32
end
ThrowRecInst(tag::UInt16, l::Integer) = ThrowRecInst(IThrowRec, tag, Int32(l))


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
    if patt.val == ""  # A valid way of saying "fail"
        return compile!(PFalse())
    end
    # Specialize one-char sets into PChar
    if sizeof(patt.val) ≤ 4 && length(patt.val) == 1
        return compile!(PChar(first(patt.val)))
    end
    bvec, prefix_map = vecsforstring(patt.val)
    if bvec !== nothing && prefix_map === nothing
        push!(patt.code, SetInst(bvec))
     else
         encode_multibyte_set!(patt.code, bvec, prefix_map)
     end
    pushEnd!(patt.code)
    return patt
end

function _compile!(patt::PRange)::Pattern
    a, b = patt.val
    vec = Vector{typeof(a)}(undef, b - a + 1)
    i = 1
    for char in a:b
        vec[i] = char
        i += 1
    end
    bvec, prefix_map = vecsforstring(Vector{AbstractChar}(vec))
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
    # TODO PSet optimization: complement
    c = patt.code
    code = hoist!(patt, patt[1])
    # We can remove captures from PNot patterns,
    # which never succeed (except match-time captures)I
    for (idx, inst) in enumerate(code)
        if ( ( inst.op == IOpenCapture
            || inst.op == ICloseCapture
            || inst.op == IFullCapture )
                && !(inst.kind == Cruntime))
            code[idx] = OpNoOp
        end
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
    # Optimization: difference of sets can be done with Boolean logic
    headset = isa(v[1], PSet) || isa(v[1], PRange)
    if headset && isa(v[2], PSet) || isa(v[2], PRange)
        ba, bb = v[1].code[1].vec, v[2].code[1].vec
        bvec = ba .& .! bb
        return PSet([SetInst(bvec), OpEnd])
    elseif headset && isa(v[2], PChar)
        bvec = copy(v[1].code[1].vec)
        bvec[Int(v[2].val) + 1] = false
        return PSet([SetInst(bvec), OpEnd])
    end
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
        error("repetition on $(typeof(p)) is not allowed")
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
        addstar!(c, code)
        c[end] = CommitInst(1)
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
    push!(c, ChoiceInst(l + 2)) # Choice + PartialCommit
    append!(c, code)
    push!(c, PartialCommitInst(-l))
    # Choice Target
end

function _compile!(patt::PChoice)::Pattern
    changed, mcode, start_idx = setoptimize!(patt)
    choices = []
    c = patt.code
    if changed
        if start_idx == length(patt.val) + 1
            # We merged everything, no backtrack
            append!(c, mcode)
            pushEnd!(c)
            return patt
        else # This is just the first choice, more to come
            len = length(mcode)
            push!(c, ChoiceInst(len + 2))
            push!(choices, length(c))  # Which is 1 but code follows intent
            append!(c, mcode)
            push!(c, HoldInst(ICommit))
        end
    end

    for idx in start_idx:length(patt.val)
        p = patt.val[idx]
        pcode = copy(p.code)
        trimEnd!(pcode)
        if idx == length(patt.val)
            append!(c, pcode)
            break
        end
        len = length(pcode)
        push!(c, ChoiceInst(len + 2))  # +2 == Choice and Commit
        push!(choices, length(c))
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

# TODO Change PChoice to use Ends, write hoist! for child::PChoice which makes them CommitInst

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

Compiles _and prepares_ a Grammar, which deserves its own approach.
"""
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
        @error "missing primitive maybe? $(typeof(patt)).val <: $(typeof(patt.val))"
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
                    p.ref.aux[:hascall] = p.ref[1].aux[:hascall]
                end
            end
        elseif p isa PCapture
            hascap = true
        end
        p = patt.val[idx] = recursepattern!(p, gaux)
    end
    patt.aux[:visiting] = false
    patt.aux[:walked] = true
    if !haskey(patt.aux, :hascall)
        patt.aux[:hascall] = hascall
        patt.aux[:hascap] = hascap
    end
    return patt
end

function link!(code::IVector, aux::AuxDict)
    callsite = aux[:callsite]
    rules = aux[:rules]
    throws = aux[:throws]
    for (idx, inst) in enumerate(code)
        if inst.op == IOpenCall
            site = callsite[inst.rule]
            l = site - idx
            code[idx] = CallInst(l)
        elseif inst.op == IThrow
            if haskey(rules, throws[inst.tag])
                println("found a recovery rule for $(throws[inst.tag])")
                site = callsite[throws[inst.tag]]
                l = site - idx
                code[idx] = ThrowRecInst(inst.tag, l)
            end
        end
    end
end

"""
    setoptimize!(patt::PChoice)::Bool

Performs any set optimizations which are possible for this set of choices.
"""
function setoptimize!(patt::PChoice)::Tuple{Bool,IVector,Integer}
    # Every choice is as good as another so we always merge them and put them first
    setable, other = [], []
    has_multi = false
    for p in patt.val
        if p isa PChar
            push!(setable, p)
        elseif p isa PSet || p isa PRange
            # Check for multiset opcodes
            has_multi = has_multi || any(code -> code.op == IMultiSet, p.code)
            push!(setable, p)
        else
            push!(other, p)
        end
    end
    # Can't merge 0 or 1 settable choices
    if length(setable) ≤ 1
        return false, patt.code, 1  # patt.code is a placeholder
    elseif !has_multi
        # We can merge these easier
        # No multis means our Sets are final
        mcode = Inst()
        bvec = falses(128)
        for p in setable
            if p isa PSet || p isa PRange
                bvec = bvec .| p.code[1].vec
            else
                if isascii(p.val)
                    bvec[UInt(p.val)+1] = true
                else  # TODO Whatever we do with multibytes we might want this separate
                    push!(other, p)
                end
            end
        end
        push!(mcode, SetInst(bvec))
        # Juggle the choices so the merged ones come first, we're done with them
        empty!(patt.val)
        append!(patt.val, setable)
        i = length(patt.val) + 1
        append!(patt.val, other)
        return true, mcode, i
    else  # Nothing else for now
        return false, patt.code, 1
    end
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
    # This is different than in LPeg and I'm not sure why
    elseif patt isa PSeq return headfail(patt.val[1])
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
    elseif isof(patt, PRange, PChar, PAny, PSet, PFalse, PThrow) return false
    # POpenCall needs checked later
    elseif patt isa POpenCall return false
    elseif patt isa PCall return false  # should be nofail(patt.ref) but we need better rule match
    # !nofail but nullable (circumstances differ, e.g. inherent vs. body)
    elseif isof(patt, PNot, PBehind, PRunTime) return false
    # PSeq nofail if entire sequence is nofail
    elseif patt isa PSeq return all(p -> nofail(p), patt.val)
    # PChoice nofail if any branch is nofail
    elseif patt isa PChoice return any(p -> nofail(p), patt.val)
    # Wrapped patterns nofail based on the pattern they enclose
    elseif isof(patt, PCapture, PGrammar, PRule, PTXInfo, PAnd) return nofail(patt.val[1])
    # Why is this true of PCall?
    # TODO Check this assumption when we implement it
    # elseif patt isa PCall return nofail(patt.val[2])
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
function fixedlen(patt::Pattern)::Union{Integer,Bool}
    if isof(patt, PChar, PSet, PRange)
        return 1
    elseif isof(patt, PAny)
        return patt.val
    elseif isof(patt, PTrue, PFalse, PAnd, PNot, PBehind)
        return 0
    elseif isof(patt, PStar, PRunTime, POpenCall, PThrow)
        return false
    elseif isof(patt, PCapture, PRule)
        return fixedlen(patt[1])
    elseif patt isa PCall
        error("fixedlen not yet implemented for PCall")
    elseif patt isa PSeq
        len = 0
        for p in patt.val
            l = fixedlen(p)
            if l === false
                return false
            else
                len += 1
            end
        end
        return len
    elseif patt isa PChoice
        len = fixedlen(patt.val[1])
        if len === false
            return false
        end
        for i = 2:length(patt.val)
            l = fixedlen(patt.val[i])
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
                bvec = falses(128)
            end
            bvec[UInt(char)+1] = true
        else
            if prefix_map === nothing
                prefix_map = Dict()
            end
            len, bytes = encode_utf8(char)
            if len == 2
                prefix!(prefix_map, [bytes[1]], bytes[2])
            elseif len == 3
                prefix!(prefix_map, [bytes[1], bytes[2]], bytes[3])
            elseif len == 4
                prefix!(prefix_map, [bytes[1], bytes[2], bytes[3]], bytes[4])
            end
        end
    end
    if !isnothing(prefix_map)
        compact_bytevec!(prefix_map)
    end
    return bvec, prefix_map
end

function prefix!(map::Dict, key, val)
    # mask off the top two bytes to save space later
    val = val & 0b00111111
    if haskey(map, key)
        push!(map[key], val)
    else
        map[key] = []
        push!(map[key], val)
    end
end

# later being here
function compact_bytevec!(prefixes)
    for (pre, vec) in prefixes
        bvec = falses(64)
        for byte in vec
            bvec[byte+1] = true
        end
        prefixes[pre] = bvec
    end
end


"""
    encode_multibyte_set!(c::IVector, pre::Dict)

TBW
"""
function encode_multibyte_set!(c::IVector, bvec::Union{BitVector,Nothing}, pre::Dict)
    coll = collect(pre)
    len = length(coll) + 2
    if bvec !== nothing
        push!(c, LeadSetInst(bvec, len))  # -> end of MultiSet, after OpFail
    end
    for (idx, pair) in enumerate(coll)
        bytes, vec = pair
        push!(c, MultiSetInst(bytes, vec, len - idx))
    end
    push!(c, OpFail)
end

function encode_utf8(char::Char)::Tuple{Int, Vector{UInt8}}
    vec = Vector{UInt8}(string(char))
    return length(vec), vec
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