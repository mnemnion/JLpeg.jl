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
    for (idx, op) in enumerate(code)
        push!(lines, "$idx: $(repr(op))")
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