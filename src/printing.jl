# Printing methods for elements of JLpeg

"Show an Instruction"
function Base.show(io::IO, inst::Instruction)
    print(io, inst_str(inst, Int32(0)))
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
        line = ["$ipad: "]
        frags = inst_pieces(inst, idx)
        push!(line, join(frags))
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
    compact = get(io, :compact, false)
    if compact
        print(io, typeof(patt))
        return
    end
    print(io, patt_str(patt))
end

"Show a Grammar"
function Base.show(io::IO, ::MIME"text/plain", patt::PGrammar)
    if !haskey(patt.meta, :start) # Not yet compiled
        return print(io, patt_str(patt))
    end
    green(s::Union{String,Symbol}) = "\x1b[32m$s\x1b[0m"
    bold(s::Union{String,Symbol}) = "\x1b[1m$s\x1b[0m"
    meta = patt.meta
    pad = length(patt.code) > 99 ? 3 : 2
    lines = [":$(meta[:start])", '-'^(length(string(meta[:start]))+1)]
    mapsite = Dict()
    for (sym, location) in meta[:callsite]
        mapsite[location] = sym
    end
    for (idx, inst) in enumerate(patt.code)
        ipad = lpad(idx, pad, "0")
        label = ""
        isrule = haskey(mapsite, idx)
        if isrule
            ipad = green(ipad)
            label = " :$(green(mapsite[idx]))"
        end
        line = ["  $ipad: "]
        frags = inst_pieces(inst, idx)
        append!(line, frags)
        push!(line, label)
        if inst.op == ICall
            loc = idx + inst.l
            if haskey(mapsite, loc)
                push!(line, " →")
                push!(line,  bold(" :$(mapsite[loc])"))
            else
                @warn "address $loc doesn't match a rule location"
            end
        end
        push!(lines, join(line))
    end
    print(io, join(lines, "\n"))
end



"Show a VMState"
function Base.show(io::IO, ::MIME"text/plain", vm::VMState)
    print(io, vm_to_str(vm))
end

# Pattern Printer

function patt_str(patt::Pattern)::String
    lines=[typeof(patt), "("]
    push!(lines, "val→", repr("text/plain", patt.val, context=:compact=>true), ", ")
    if hasfield(typeof(patt), :n)
        push!(lines, "n=", string(patt.n), ", ")
    end
    push!(lines, repr("text/plain", patt.code, context=:compact=>true), ")")
    join(lines) * ")"
end

# Instruction printing helpers

function inst_str(inst::Instruction, off::Integer)::String
    "⟪$(join(inst_pieces(inst, off)))⟫"
end

"Vector of instruction string fragments"
function inst_pieces(inst::Instruction, off::Integer)::Vector{String}
    line = ["$(inst.op)"]
    t = typeof(inst)
    if hasfield(t, :c)
        push!(line, " '$(inst.c)'" )
    end
    if hasfield(t, :n)
        push!(line, " $(inst.n)")
    end
    if hasfield(t, :l)
        push!(line, " ($(inst.l + off))")
    end
    if hasfield(t, :rule)
        push!(line, " :$(inst.rule)")
    end
    if hasfield(t, :final)
        push!(line, inst.final ? " t" : " f")
    end
    if hasfield(t, :vec) && !hasfield(t, :lead)
        push!(line, " $(printset(inst.vec))")
    elseif hasfield(t, :vec) && hasfield(t, :lead)
        push!(line, " {$(multivec_string(inst))}")
    end
    line
end


"String for Multivector"
function multivec_string(set::Instruction)::String
    frag = []
    lead = set.lead
    vec = set.vec
    si = 0
    ei = 0
    function tochar(i)
        b = (i | 0b10000000) - UInt8(1)
        str = String(push!(copy(lead), b))
        return str
    end
    for i in eachindex(vec)
        if vec[i]
            if si == 0
                si = i
            end
            ei = i
        elseif si != 0
            # Sequence
            if ei - 2 > si
                push!(frag, "$(tochar(si))-$(tochar(ei))")
            else
                for j in si:ei
                    push!(frag, tochar(j))
                end
            end
            si, ei = 0, 0
        end
    end
    if si > 0
        if ei - 2 > si
            push!(frag, "$(tochar(si))-$(tochar(ei))")
        else
            for j in si:ei
                push!(frag, tochar(j))
            end
        end
    end
    return join(frag, ",")
end


"String for Set Vector"
function printset(vec::BitVector)::String
    chars = bitvector_to_compact_repr(vec)
    str = "{"

    str *= join(chars, ",")
    str *= "}"
    return str
end

"""
    bitvector_to_compact_repr(bitvec::BitVector)

Shows a set while collapsing ranges.
"""
function bitvector_to_compact_repr(bitvec::BitVector)
    fragments = String[]
    start_idx = 0
    end_idx = 0
    escaped_chars = Dict(
        '\n' => "\\n",  # newline
        '\t' => "\\t",  # horizontal tab
        '\r' => "\\r",  # carriage return
        '\b' => "\\b",  # backspace
        '\f' => "\\f",  # form feed
        '\\' => "\\\\", # backslash
        '\"' => "\\\"", # double quote
        '\'' => "\\'"   # single quote
    )
    function _encode(i::Integer)
        c = Char(i -1)
        if haskey(escaped_chars, c)
            escaped_chars[c]
        elseif i ≤ 31
            "\\x" * string(i, base=16)
        elseif c == ' '
            "\" \""
        else
            string(c)
        end
    end
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
                push!(fragments, "$(_encode(start_idx))-$(_encode(end_idx))")
            else
                # Individual characters
                for j in start_idx:end_idx
                    push!(fragments, _encode(j))
                end
            end
            start_idx = 0
            end_idx = 0
        end
    end

    # Handle the case where the sequence reaches the end of the BitVector
    if start_idx != 0
        if end_idx - start_idx >= 2
            push!(fragments, "$(_encode(start_idx))-$(_encode(end_idx))")
        else
            for j in start_idx:end_idx
                push!(fragments, string(_encode(j)))
            end
        end
    end

    return fragments
end


# VMState printing helpers

function short_vm(vm::VMState)::String
    o = vm.t_on ? 1 : 0
    b = vm.t_on ? "yes" : "no"
    "State: [i:$(vm.i)] $(inst_str(vm.program[vm.i], vm.i)) $b ⟨$(length(vm.stack) + o)⟩ s:$(in_red(vm.subject, vm.s))\n"
end

function frame_to_str(vm::VMState, i, s)::String
    inst = vm.program[i]
    if s == 0
        "[i:$(i)] $(inst_str(inst,i))"
    else
        "[i:$(i)] $(inst_str(inst, i)) s:$(in_red(vm.subject, s))"
    end
end

function vm_to_str(vm::VMState)::String
    lines = [short_vm(vm)[1:end-1]]
    if !vm.t_on
        push!(lines, "Frame: []")
    else
        push!(lines, "Frames:")
        push!(lines, frame_to_str(vm, vm.ti, vm.ts))
    end
    for frame in Iterators.reverse(vm.stack)
        push!(lines, frame_to_str(vm, frame.i, frame.s))
    end
    push!(lines, "---\n")
    return join(lines, "\n")
end

function in_red(str::String, i::UInt32)
    if isempty(str)
        return "\"\""
    end
    if i == 0
        str1 = "\x1B[38;5;208m*\x1B[0m"
        str2, _ = substr_at_i(str, UInt32(1))
        return str1 * str2
    elseif i > sizeof(str)
        str2 = "\x1B[38;5;208m*\x1B[0m"
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
    if i2 ≠ i1
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

return