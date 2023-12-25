# Printing methods for elements of JLpeg

"Show an Instruction"
function Base.show(io::IO, inst::Instruction)
    line = ["⟪$(inst.op)"]
    t = typeof(inst)
    if hasfield(t, :c)
        push!(line, " '$(inst.c)'" )
    end
    if hasfield(t, :n)
        push!(line, " $(inst.n)")
    end
    if hasfield(t, :l)
        push!(line, " ($(inst.l))")
    end
    if hasfield(t, :rule)
        push!(line, " :$(inst.rule)")
    end
    if hasfield(t, :vec)
        push!(line, " $(printset(inst.vec))")
    end
    push!(line, "⟫")
    print(io, join(line))
end

"""
    bitvector_to_compact_repr(bitvec::BitVector)

Shows a set while collapsing ranges.
"""
function bitvector_to_compact_repr(bitvec::BitVector)
    fragments = String[]
    start_idx = 0
    end_idx = 0

    for i in eachindex(bitvec)
        if bitvec[i]
            if start_idx == 0
                start_idx = i
            end
            end_idx = i
        elseif start_idx != 0
            # End of a sequence
            if end_idx - start_idx >= 2
                # Three or more characters in succession
                push!(fragments, "$(Char(start_idx))-$(Char(end_idx))")
            else
                # Individual characters
                for j in start_idx:end_idx
                    push!(fragments, string(Char(j)))
                end
            end
            start_idx = 0
            end_idx = 0
        end
    end

    # Handle the case where the sequence reaches the end of the BitVector
    if start_idx != 0
        if end_idx - start_idx >= 2
            push!(fragments, "$(Char(start_idx))-$(Char(end_idx))")
        else
            for j in start_idx:end_idx
                push!(fragments, string(Char(j)))
            end
        end
    end

    return fragments
end


"String for Set Vector"
function printset(vec::BitVector)::String
    chars = bitvector_to_compact_repr(vec)
    str = "{"

    str *= join(chars, ",")
    str *= "}"
    return str
end

"Show a vector of Bytecode instructions"
function Base.show(io::IO, ::MIME"text/plain", code::IVector)
    compact = get(io, :compact, false)
    if compact 
        if isempty(code)
            print(io, "VM[]")
        else
            print(io,"VM[1:$(length(code))]")
        end
        return
    end
    lines = []
    pad = length(code) > 99 ? 3 : 2
    for (idx, inst) in enumerate(code)
        ipad = lpad(idx, pad, "0")
        line = ["$ipad: $(inst.op)"]
        t = typeof(inst)
        if hasfield(t, :c)
            push!(line, " '$(inst.c)'" )
        end
        if hasfield(t, :n)
            push!(line, " $(inst.n)")
        end
        if hasfield(t, :l)
            off = idx + inst.l
            push!(line, " ($off)")
        end
        if hasfield(t, :rule)
            push!(line, " :$(inst.rule)")
        end
        if hasfield(t, :vec)
            push!(line, " $(printset(inst.vec))")
        end
        push!(lines, join(line))
    end
    print(io, join(lines, "\n"))
end

"Show a vector of patterns"
function Base.show(io::IO, ::MIME"text/plain", patts::Vector{Pattern})
    compact = get(io, :compact, false) 
    lines = ["["]
    for p in patts
        if compact
            push!(lines, string(typeof(p)), ",")
        else
            push!(lines, repr("text/plain", p, context=:compact=>true), ",")
        end
    end
    pop!(lines)
    print(io, join(lines) * "]")
end


"Show a Pattern"
function Base.show(io::IO, ::MIME"text/plain", patt::Pattern)
    lines=[typeof(patt), "("]
    push!(lines, "val→", repr("text/plain", patt.val, context=:compact=>true), ", ")
    if hasfield(typeof(patt), :n)
        push!(lines, "n=", string(patt.n), ", ")
    end
    push!(lines, repr("text/plain", patt.code, context=:compact=>true), ")")
    print(io, join(lines) * ")")
end

# Printing the VMState 
"Show a VMState"
function Base.show(io::IO, ::MIME"text/plain", vm::VMState)
    print(io, vm_to_str(vm))
end

function vm_to_str(vm:: VMState)::String
    lines = []
    sub = vm.subject
    state = "State: i:$(vm.i):$(vm.program[vm.i])"
    subj = " s:$(in_red(sub, vm.s))\n"
    push!(lines, state, subj)
    if isempty(vm.stack) 
        push!(lines, "Frame: []\n")
    else
        push!(lines, "Frames:\n")
    end
    for frame in vm.stack
        if frame.s == 0 
            push!(lines, "[i:$(frame.i)]:$(vm.program[frame.i])]")
        else
            push!(lines, "[i:$(frame.i)]:$(vm.program[frame.i]) s:$(in_red(sub, frame.s))")
        end
        push!(lines, "\n")
    end
    push!(lines, "---\n")
    return join(lines)
end

function in_red(str::String, i::UInt32)
    if i == 0
        str1 = "\x1B[31m*\x1B[0m" 
        str2, _ = substr_at_i(str, UInt32(1))
        return str1 * str2 
    elseif i > sizeof(str)
        str2 = "\x1B[31m*\x1B[0m"
        str1, _ = substr_at_i(str, UInt32(sizeof(str)))
        return str1 * str2
    end
    red_i(substr_at_i(str, i)...)
end

function red_i(str::String, i::UInt32)
    red_start = "\x1B[31m" 
    red_end = "\x1B[0m" 
    sstr = sizeof(str)   
    i1 = clamp(i, 1, sstr)
    i2 = clamp(i+1, 1, sstr)
    highlighted_str = str[1:i1-1] * red_start * str[i1:i1] * red_end 
    if i2 < sstr 
        highlighted_str *= str[i2:end]
    end

    return highlighted_str
end

function substr_at_i(str::String, i::UInt32)
    # Determine the start index
    start_index = i
    for _ = 1:10
        start_index == 1 && break
        start_index = prevind(str, start_index)
    end

    # Determine the end index with bounds checking
    end_index = i
    for _ = 1:10
        if end_index > sizeof(str)
            break
        else
            end_index = nextind(str, end_index)
        end
    end

    if end_index > sizeof(str)
        end_index = sizeof(str)
    end
    # Extract the substring
    substring = str[start_index:end_index]

    # Find the byte index of 'i' within the substring
    byte_index_in_substring = i - start_index + 1

    return (substring, UInt32(byte_index_in_substring))
end

