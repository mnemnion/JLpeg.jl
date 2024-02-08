# Grammar generators

# Plan of attack: this eventually becomes a full bytecode interpreter, but
# the first thing I need is a generator for Sets, so it will take PSet.

# TODO this needs to take keyword options with defaults, examples:
# full=true, PSets will generate the full set in order

# Status: this combines checking the validity of the MultiSet code (and in fact
# surfaced several bugs) with generating from a Set.  I don't expect the generator
# architecture to run on Instructions, but rather, raw Patterns (or rather an appropriate
# transmutation of them, with an eye towards ease, not speed: an interpreter, not a VM).
#
# So I want to update this code when the transition to multi-instruction Sets is complete,
# but will also tear this out when the work on generators begins in earnest.

function generate(patt::Pattern)::String
    error("Not yet implemented for $(typeof(patt))")
end

"""
    generate(set::PSet)::String

Generate a `String` of all characters matched by a Set.
"""
function generate(set::PSet)::String
    # TODO remove the printf spam when satisfied this is accurate code
    isempty(set.code) && compile!(set)
    buff = IOBuffer()
    c = set.code
    start = 1
    leadcount = nothing
    if c[1].op == ISet
        _generateLow(buff, c[2])
        _generateHigh(buff, c[3])
        return String(take!(buff))
        start += 3
    elseif c[1].op == ILeadSet
        _generateLow(buff, c[2])
        _generateHigh(buff, c[3])
        start += 3
    end
    if c[start].op == ILeadMulti
        leadcount = count_ones(c[start + 1])
        start += 2
    end
    if leadcount === nothing
        leadcount = 0
        for i in start:length(c)
            if c[i] == OpFail
                break
            end
            leadcount += 1
        end
        @assert leadcount > 0  "failure to count lead bytes"
    end
    # Generator state
    countstack = [0, 0, 0, 0]
    gen = true
    off = start
    charstack = UInt8[]
    byte = 1
    # println(leadcount)
    # dbgcount = 0
    while gen
        if countstack[1] ≥ leadcount
            break
        end
        inst = c[off]
        # println("$off: ", inst)
        # println("   $countstack  $byte  $charstack")
        if inst isa LeadByteInst
            push!(charstack, inst.b)
            byte += 1
            off += inst.l + countstack[byte]
        elseif inst isa MultiVecInst
            # dbgcount += 1
            # println("   $dbgcount  $countstack  $byte  $charstack  $off")
            _generateMultiVec(buff, charstack, c[off + 1])
            empty!(charstack)
            countstack[byte - 1] += 1
            off = start + countstack[1]
            byte = 1
            continue
        elseif inst == OpEnd
            error("overshot MultiSet")
            gen = false
        elseif inst == OpFail
            if byte == 1
                error("overshot lead bytes")
            end
            empty!(charstack)
            countstack[byte - 1] += 1
            countstack[byte] = 0
            off = start + countstack[1]
            byte = 1
            continue
        end
    end

    return String(take!(buff))
end


function _generateLow(buff::IOBuffer, inst::InstructionVec)
    for b in inst
        if b isa UInt8
            write(buff, b)
        end
    end
end

function _generateHigh(buff::IOBuffer, inst::InstructionVec)
    for b in inst
        if b isa UInt8
            write(buff, b | 0x01000000)
        end
    end
end

function _generateMultiVec(buff::IOBuffer, charstack::Vector{UInt8}, inst::InstructionVec)
    #stack = UInt8[]
    for b in inst
        if b isa UInt8
            b = b | 0x80
            # append!(stack, charstack)
            # push!(stack, b)
            write(buff, charstack...)
            write(buff, b)
        end
    end
    # println(stack)
end