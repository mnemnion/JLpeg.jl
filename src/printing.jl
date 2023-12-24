# Printing methods for elements of JLpeg

"Show an Instruction"
function Base.show(io::IO, i::Instruction)
    str = "⟪$(i.op)"
    for field in fieldnames(typeof(i))
        if field ≠ :op 
            val = getfield(i, field)
            str *= ", $field→$val"
        end
    end
    str *= "⟫"
    print(io, str)
end

"""
    bitvector_to_compact_repr(bitvec::BitVector)

Shows a set while collapsing ranges.
"""
function bitvector_to_compact_repr(bitvec::BitVector)
    fragments = String[]
    start_idx = 0
    end_idx = 0

    for i in 1:length(bitvec)
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
function Base.show(io::IO, ::MIME"text/plain", code::Vector{Instruction})
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
    push!(lines, repr("text/plain", patt.code, context=:compact=>true), ")")
    print(io, join(lines) * ")")
end