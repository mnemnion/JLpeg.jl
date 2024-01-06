# Grammar generators

# Plan of attack: this eventually becomes a full bytecode interpreter, but
# the first thing I need is a generator for Sets, so it will take PSet.

# TODO this needs to take keyword options with defaults, examples:
# full=true, PSets will generate the full set in order

function generate(patt::Pattern)::String
    error("Not yet implemented for $(typeof(patt))")
end

"""
    generate(set::PSet)::String

Generate a `String` of all characters matched by a Set.
"""
function generate(set::PSet)::String
    isempty(set.code) && compile!(set)
    buff = IOBuffer()
    c = set.code
    start = 1
    leadcount = nothing
    if c[1].op == ISet
        _generateISet(buff, c[1])
        return String(take!(buff))
        start += 1
    elseif c[1].op == ILeadSet
        _generateISet(buff, c[1])
        start += 1
    end
    if c[start].op == ILeadMulti
        leadcount = count_ones(c[start])
        start += 1
    end
    # We need a counter stack
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
    countstack = [0, 0, 0, 0]
    gen = true
    off = start
    charstack = UInt8[]
    byte = 1
    # println(leadcount)
    while gen
        if countstack[1] ≥ leadcount
            break
        end
        inst = c[off]
        # println("$off: ", inst)
        # println("   $countstack  $byte  $charstack")
        if inst isa ByteInst
            push!(charstack, inst.b)
            byte += 1
            off += inst.l + countstack[byte]
        elseif inst isa MultiVecInst
            _generateMultiVec(buff, charstack, inst)
            empty!(charstack)
            countstack[byte - 1] += 1
            off = start + countstack[1]
            byte = 1
            continue
        elseif inst == OpEnd
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

function _generateISet(buff::IOBuffer, inst::Instruction)
    for b in inst
        if b isa UInt8
            write(buff, b)
        end
    end
end

function _generateMultiVec(buff::IOBuffer, charstack::Vector{UInt8}, inst::MultiVecInst)
    # stack = UInt8[]
    for b in inst
        if b isa UInt8
           # append!(stack, charstack)
           #  push!(stack, b)
            write(buff, charstack...)
            write(buff, b)
        end
    end
    # println(String(stack))
end