# Instruction compiling for Patterns

include("pattern.jl")

"Instruction opcodes for JLpeg VM"
@enum Opcode::UInt8 begin
    IAny        # if no char, fail
    IChar       # if char != aux, fail
    ISet        # if char not in buff, fail
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
    final::Bool
    SetInst(vec::BitVector) = new(ISet, vec, true)
    SetInst(vec::BitVector, final::Bool) = new(ISet, vec, final)
end

struct MultiSetInst <: Instruction
    op::Opcode
    lead::Vector{UInt8}
    vec::BitVector
    final::Bool
    function MultiSetInst(lead::Vector{UInt8}, vec::BitVector, final::Bool)
        new(IMultiSet, lead, vec, final)
    end
    function MultiSetInst(multi::MultiSetInst, final::Bool)
        new(IMultiSet, multi.lead, multi.vec, final)
    end
end

struct BehindInst <: Instruction
    op::Opcode
    n::UInt32  # Should be an l, surely
    BehindInst(n::Integer) = n ≥ 0 ? new(IBehind, n) : error("n must be a natural number")
end

struct LabelInst <: Instruction
    op::Opcode
    l::Int32
end

struct ChoiceInst <: Instruction
    op::Opcode
    l::Int32
    n::Int32
    ChoiceInst(l::Integer) = new(IChoice, l, 0)
end

CommitInst(l::Integer) = LabelInst(ICommit, Int32(l))
CallInst(l::Integer) = LabelInst(ICall, Int32(l))
JumpInst(l::Integer) = LabelInst(IJump, Int32(l))
PartialCommitInst(l::Integer) = LabelInst(IPartialCommit, Int32(l))
BackCommitInst(l::Integer) = LabelInst(IBackCommit, Int32(l))

struct TestAnyInst <: Instruction
    op::Opcode
    n::UInt32
    l::Int32
    TestAnyInst(n::UInt32, l::Int32) = new(ITestAny, n, Int32(l))
end

struct TestCharInst <: Instruction
    op::Opcode
    c::AbstractChar
    l::Int32
    TestCharInst(c::AbstractChar, l::Int32) = new(ITestChar, c, Int32(l))
end

struct TestSetInst <: Instruction
    op::Opcode
    vec::BitVector
    l::Int32
    TestSetInst(vec::BitVector, l::Int32) = new(ITestSet, vec, Int32(l))
end

struct OpenCallInst <: Instruction
    op::Opcode
    rule::Symbol # Symbol?
    OpenCallInst(r::Symbol) = new(IOpenCall, r)
end

"A placeholder for a (usually labeled) Instruction"
struct HoldInst <: Instruction
    op::Opcode
end

# To be continued...

abstract type SpanInst end
abstract type PredChoiceInst end
abstract type NameCallInst end
abstract type CloseRunTimeInst end
abstract type ThrowInst end
abstract type ThrowRecInst end

 struct OpenCaptureInst <: Instruction
    op::Opcode
    kind::CapKind
    OpenCaptureInst(kind::CapKind) = new(IOpenCapture, kind)
 end

 struct CloseCaptureInst <: Instruction
    op::Opcode
    kind::CapKind
    CloseCaptureInst(kind::CapKind) = new(ICloseCapture, kind)
 end

 struct FullCaptureInst <: Instruction
    op::Opcode
    kind::CapKind
    off::Int32
    FullCaptureInst(kind::CapKind, off::Integer) = new(IFullCapture, kind, Int32(off))
 end


"""
    Base.merge!(dict::AbstractDict, pairs::Pair...)

Adds `pairs` to `dict`.  Synonymous with `push!`, useful for `Union{Dict,Pair}` types.

## Examples
```jldoctest
julia> dict = Dict(:a => 'a', :b => 'b')
Dict{Symbol, Char} with 2 entries:
  :a => 'a'
  :b => 'b'

julia> merge!(dict, :c => 'c', :d => 'd')
Dict{Symbol, Char} with 4 entries:
  :a => 'a'
  :b => 'b'
  :d => 'd'
  :c => 'c'
```
"""
function Base.merge!(dict::AbstractDict, pairs::Pair...)
    for pair in pairs
        dict[pair.first] = pair.second
    end
    return dict
end

### Compilers

"""
    compile!(patt::Pattern)::Pattern

Compile a Pattern.

Translate the Pattern to Instruction codes, appending them to
the `code` field and returning same.  Performs various optimizations
in the process.

In most cases, the return value of `compile!` is the same Pattern passed in.
The exceptions are special cases of primitive types, but be sure to reassign
the return value to the variable bound to the original for the general case.
"""
function compile!(patt::Pattern)::Pattern
    if isempty(patt.code)
        t = typeof(patt.val)
        if t == Vector{Pattern} || t == Vector{PRule}
            for (idx, val) in enumerate(patt.val)
                patt.val[idx] = compile!(val)
                mergeaux!(patt, patt.val[idx])
            end
        end
        _compile!(patt)
    else
        patt
    end
end


"""
    prepare!(patt::Pattern)::Pattern

Prepare a pattern for compiling.
"""
function prepare!(patt::Pattern)::Pattern
    compile!(patt)
end

"""
    prepare!(patt::PAuxT)::Pattern

TBW
"""
function prepare!(patt::PAuxT)::Pattern
    if haskey(patt.aux, :prepared)
        return patt
    end
    patt = compile!(patt)
    if !isa(patt, PAuxT)
        return patt
    end
    for (key, value) in patt.aux
        if value isa Pair
            patt.aux[key] = Dict(value)
        end
    end
    patt.aux[:prepared] = true
    return patt
end

mergeaux!(::Pattern, ::Pattern) = return

function mergeaux!(patt::Pattern, val::PAuxT)
    paux, vaux = patt.aux, val.aux
    for (key, val) in vaux
        if haskey(paux, key)
            # We'll special-case everything until the end then consolidate
            if :cap == key
                # Already promoted to :caps by _compile!
                continue
            elseif :caps == key
                merge!(paux[:caps], val)
            else
                @error "need to be able to merge $key in $(typeof(val)).aux"
            end
        else
            if :cap == key || :prepared == key || :call == key || :terminal == key
                continue
            elseif :caps == key
                paux[:caps] = merge!(Dict(), val)
            else
                @error "need to create-merge $key in $(typeof(patt)).aux"
            end
        end
    end
end


function _compile!(patt::Pattern)::Pattern
    error("Not Yet Implemented for $(typeof(patt))")
end

function _compile!(patt::PAny)::Pattern
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
    push!(patt.code, OpFail)
    return patt
end

function _compile!(patt::POpenCall)::Pattern
    push!(patt.code, OpenCallInst(patt.val))
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

    is_final = prefix_map === nothing ? true : false
    if bvec !== nothing
        push!(patt.code, SetInst(bvec, is_final))
    end
    if prefix_map !== nothing
        encode_multibyte_set!(patt.code, prefix_map)
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
    is_final = prefix_map === nothing ? true : false
    if bvec !== nothing
       push!(patt.code, SetInst(bvec, is_final))
    end
    if prefix_map !== nothing
        encode_multibyte_set!(patt.code, prefix_map)
    end
    pushEnd!(patt.code)
    return patt
end

function _compile!(patt::PAnd)::Pattern
    @assert length(patt.val) == 1 "enclosing rule PAnd has more than one child"
    c = patt.code
    code = copy(patt.val[1].code)
    trimEnd!(code)
    l = length(code) + 2  # 2 -> Choice, BackCommit
    push!(c, ChoiceInst(l))
    append!(c, code)
    push!(c, BackCommitInst(2))
    push!(c, OpFail)  # Choice target
    pushEnd!(c)
    return patt
end

function _compile!(patt::PNot)::Pattern
    @assert length(patt.val) == 1 "enclosing rule PNot has more than one child"
    c = patt.code
    code = copy(patt.val[1].code)
    # We must remove captures from PNot patterns,
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
    push!(c, ChoiceInst(l))
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
    for p in patt.val
        code = copy(p.code)
        trimEnd!(code)
        append!(patt.code, code)
        # optimizations?
    end
    pushEnd!(patt.code)
    return patt
end

function _compile!(patt::PStar)::Pattern
    # TODO figure out when TestChar etc come into play
    #
    # bad things happen when val is  a PStar, specifically
    # when the inner is optional, e.g. ("ab"?)*, so we check for this
    # and fix it when we need to.
    p = patt.val[1]
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
    code = copy(p.code)
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

function _compile!(patt::PCapture)::Pattern
    # Special-case Cp()
    if patt.kind == Cposition
        push!(patt.code, FullCaptureInst(Cposition, 0), OpEnd)
        return patt
    end
    c = patt.code
    ccode = copy(patt.val[1].code)
    # TODO full capture optimization
    trimEnd!(ccode)
    push!(c, OpenCaptureInst(patt.kind))
    append!(c, ccode)
    close = CloseCaptureInst(patt.kind)
    if patt.aux[:cap] !== nothing
        patt.aux[:caps] = close => patt.aux[:cap]
    end
    push!(c, close)
    pushEnd!(c)
    return patt
end

function _compile!(patt::PRule)::Pattern
    c = patt.code
    meta = patt.aux
    calls = meta[:call] = Dict{Int, Symbol}()
    for (idx, op) in enumerate(patt.val[1].code)
        if op.op == IOpenCall
            calls[idx] = op.rule
        end
        push!(c, op)
    end  # TODO inline rules sometimes
    meta[:terminal] = isempty(calls) ? true : false
    pushEnd!(c)
    # TODO probably want to inline 'short' terminals for some value of short. what value?
    return patt
end

# TODO: Grammar
# [ ] Grammar should have its own compile! so we normally don't have to do weird surgery/copying
# [ ] This should begin by replacing all POpenCall with PCall,
#     only then generating code.  What this means is we pass along the rulemap to
#     the first rule, then go visiting subrules: if it's terminal, we compile it,
#     if we see it twice, it's recursive: if that's left recursion, bail out, otherwise
#     we tag as such (and therefore variable length by definition).
#
#     Whenever we reach the end of a visited rule, we know if it's recursive, and all
#     ultimately-terminal calls (at whatever degree of remove) are compiled / we know
#     what we need to know, so we can compile that rule, and when we reach the end of the
#     start rule, we're done, and ready to hoist and link.
#
# [ ] Inlining? Should we? What circumstances?
# [ ] Tail-call elimination

function _compile!(patt::PGrammar)::Pattern
    rules = Dict{Symbol, PRule}()
    meta = patt.aux
    for rule in patt.val
        rules[rule.name] = rule
    end
    meta[:rules] = copy(rules)
    c = patt.code
    start = rules[patt.start]
    meta[:start] = start.name
    fixup = []
    meta[:callsite] = callMap = Dict{Symbol, Int}()
    # TODO we could be fancy and jump to the actual end
    push!(c, CallInst(2))
    push!(c, OpEnd)
    next = coderule!(c, start, rules, fixup, callMap)
    while !isempty(next)
        after = []
        for rule in next
            append!(after, coderule!(c, rule, rules, fixup, callMap))
        end
        next = after
    end
    # fix up missed calls
    for (rulename, i) in fixup
        inst = c[i]
        # println("fixup $rulename call at $i with l→$(meta[:callsite][rulename])")
        @assert (inst.op == IOpenCall && inst.rule == rulename) "bad fixup"
        if haskey(callMap, rulename)
            l =  callMap[rulename] - i
            c[i] = CallInst(l)
        else
            @warn lazy"missing rule $rulename in grammar"
        end
    end
    pushEnd!(c)
    return patt
end

function coderule!(c::IVector, rule::PRule, rules::Dict, fixup::Vector, callMap::Dict)::Vector{PRule}
    next = []
    # Add the instruction pointer to the callMap
    callMap[rule.name] = length(c) + 1
    for inst in rule.code
        if inst.op == IOpenCall
            if haskey(callMap, inst.rule)
                # We need this to be a relative jump
                l = callMap[inst.rule] - length(c) - 1
                push!(c, CallInst(l))
            elseif haskey(rules, inst.rule)
                push!(next, rules[inst.rule])
                pop!(rules, inst.rule)
                push!(c, inst)
                push!(fixup, (inst.rule, length(c)))
            else  # Probably in the queue, if not, we detect that later
                push!(c, inst)
                push!(fixup, (inst.rule, length(c)))
            end
        else
            push!(c, inst)
        end
    end
    trimEnd!(c)
    push!(c, OpReturn)
    return next
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
    elseif patt isa PStar && patt.n ≤ 0 return true
    elseif isof(patt, PRange, PChar, PAny, PSet, PFalse, PThrow) return false
    # POpenCall needs checked later
    elseif patt isa POpenCall return false
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

function encode_multibyte_set!(c::IVector, pre::Dict)
    coll = collect(pre)
    len = length(coll)
    count = 0
    for pair in coll
        count += 1
        bytes, vec = pair
        if count < len
            push!(c, MultiSetInst(bytes, vec, false))
        else
            push!(c, MultiSetInst(bytes, vec, true))
        end
    end
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