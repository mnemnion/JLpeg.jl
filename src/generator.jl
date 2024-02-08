# Grammar generators

# Plan of attack: this eventually becomes a full bytecode interpreter, but
# the first thing I need is a generator for Sets, so it will take PSet.

# TODO this needs to take keyword options with defaults, examples:
# full=true, PSets will generate the full set in order

# Status: this conflates "generating characters from a Set" with "confirming that the
# multiset code is valid".  The generator code is unwritten except for this one method,
# but the focus will be on generating random examples of the pattern, and for Sets,
# expanding them into a Vector of the Chars and randomly sampling that is much simpler than
# modifying this code to return a random character would be.
#
# It also makes sense for a generator to receive an IOBuffer, rather than returning String bits.
# We'll want to pass an IOBuffer to the generators and let them feed it, and we need some way to
# seed the randomness so that generation can be consistent between calls, when desired.
#
# So the ultimate fate of this code is unclear. It's very unlikely I'll make further changes to the
# Set compiler or bytecode, but if I do, this code will come into play immediately.
#
# Might end up living in the test framework for JLpegUnicode, which is the suite which really puts Sets
# through their paces.

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
            write(buff, b | 0b01000000)
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