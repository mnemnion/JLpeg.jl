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
            end
        end
        _compile!(patt)
    else
        patt 
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
    # Special-case the empty set 
    # We'll turn into a Jump when we have the requisite info
    if patt.val == ""  # A valid way of saying "fail"
        return PFalse()
    end
    bvec, prefix_map = vecsforstring(patt.val)
    if bvec !== nothing
        push!(patt.code, SetInst(bvec), OpEnd)
    end
    # We'll deal with the prefix map some other time!
    # Others are a bit more complex! heh. bit.
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
    if bvec !== nothing
       push!(patt.code, SetInst(bvec), OpEnd)
    end
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
    trimEnd!(code)
    l = length(code) + 2  # 3 -> FailTwice, next 
    push!(c, ChoiceInst(l))
    append!(c, code)
    push!(c, OpFailTwice)
    pushEnd!(c)  # Choice target
    return patt 
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
    # TODO there are several cases:
    # [X] n == 0, aka *
    # [X] n == 1, aka +
    # [X] n == -1 aka ?
    # [ ] n > 1, which is + with repetitions 
    # [ ] n < -1, which is the weird one I'll do last
    #
    # TODO figure out when TestChar etc come into play 
    #
    # bad things happen when val is  a PStar, specifically
    # when the inner is optional, e.g. ("ab"?)*, so we check for this
    # and fix it when we need to.
    # TODO this problem may not occur for even lower than -1, check when we implement.
    p = patt.val[1]
    if typeof(p) == PStar && (p.n ==  0 || p.n == -1)
        if p.n == -1 && (patt.n == 0 || patt.n == 1)
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
    c = patt.code
    code = copy(p.code) 
    trimEnd!(code)
    if patt.n == 0
        addstar!(c, code)
    elseif patt.n == 1
        append!(c, code)
        addstar!(c, code)
    elseif patt.n == -1
        addstar!(c, code)
        c[end] = CommitInst(1)
    else
        @warn "not yet handling PStar.n == $(patt.n)"
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
    c = patt.code
    choices = []
    # Optimizations:
    # Merge Set and Char choices 
    # TODO this will need more work once sets include higher characters 
    allchars = all(p -> begin t = typeof(p); t == PSet || t == PChar || t == PRange end, patt.val)
    if allchars
        bvec = falses(128)
        correct = true
        for p in patt.val 
            if typeof(p) == PSet || typeof(p) == PRange 
                bvec = bvec .| p.code[1].vec
            elseif typeof(p) == PChar 
                if isascii(p.val)
                    bvec[UInt(p.val)+1] = true
                else
                    # Bail until we handle multibyte chars 
                    @warn "multibyte chars not yet optimized"
                    correct = false 
                    break
                end
            end
        end
        if correct
            push!(c, SetInst(bvec), OpEnd)
            return patt 
        end
    end
    # headfail
    # disjoint 
    for (idx, p) in enumerate(patt.val)
        pcode = copy(p.code)
        trimEnd!(pcode)
        if idx == length(patt.val)
            append!(c, pcode)
            break
        end
        len = length(pcode) 
        push!(c, ChoiceInst(len + 2)) # +2 == Choice and Commit
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

function _compile!(patt::PRule)::Pattern
    c = patt.code 
    append!(c, patt.val[1].code)
    trimEnd!(c)
    push!(c, OpReturn)
    return patt
end

function _compile!(patt::PGrammar)::Pattern 
    rules = Dict{Symbol, PRule}()
    for rule in patt.val
        rules[rule.name] = rule 
    end
    c = patt.code
    start = rules[patt.start]
    fixup = []
    callMap = Dict{Symbol, Int}()
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
            else
                push!(c, inst)
            end
        else
            push!(c, inst)
        end
    end
    return next 
end

"""
    headfail(patt::Pattern)::Bool

Answer if the pattern fails (should it fail) on the first character.
"""
function headfail(patt::Pattern)::Bool
    @match patt begin
        ::PChar || ::PAny || ::PSet || ::PFalse => true
        ::PTrue || ::PStar || ::PRunTime || ::PNot || ::PBehind || ::PThrow => false
        ::PCapture || ::PGrammar || ::PRule || ::PTXInfo || ::PAnd => headfail(patt.val[1])
        ::PCall => headfail(patt.val[2])
        # This is different than in LPeg and I'm not sure why
        ::PSeq =>  headfail(patt.val[1])
        ::PChoice => all(p -> headfail(p), patt.val)
        _ => error(lazy"headfail not defined for $(typeof(patt))")
    end
end

"""
    nofail(patt::Pattern)::Bool

Answer if the pattern cannot fail.
"""
function nofail(patt::Pattern)::Bool
    @match patt begin
        ::PTrue => true
        ::PStar, if patt.n ≤ 0 end => true
        ::PChar || ::PAny || ::PSet || ::PFalse || ::PThrow => false
        # OpenCall needs checked when resolved
        ::POpenCall => false 
        # !nofail but nullable (circumstances differ, e.g. inherent vs. body)
        ::PNot || ::PBehind || ::PRunTime => false
        # PSeq nofail if entire sequence is nofail
        ::PSeq => all(p -> nofail(p), patt.val)
        # PChoice nofail if any branch is nofail
        ::PChoice => any(p -> nofail(p), patt.val)
        # Wrapped patterns nofail based on the pattern they enclose
        ::PCapture || ::PGrammar || ::PRule || ::PTXInfo || ::PAnd => nofail(patt.val[1])
        # Why is this true of PCall? Check this assumption when we implement it 
        ::PCall => nofail(patt.val[2])
        _ => error(lazy"nofail not defined for $(typeof(patt))")
    end
end

"""
    nullable(patt::Pattern)::Bool

Answer if the pattern can consume no input.
"""
function nullable(patt::Pattern)::Bool
    @match patt begin
        ::PNot || ::PBehind || ::PAnd => true 
        ::PRunTime => nullable(patt.val[1])
        # Most patterns are nullable if nofail:
        ::Pattern, if nofail(patt) end => true 
        # By process of elimination:
        _ => false
    end
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
    return bvec, prefix_map        
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