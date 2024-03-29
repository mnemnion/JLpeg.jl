# Printing methods for elements of JLpeg

using Unicode

"Show an Instruction."
function Base.show(io::IO, ::MIME"text/plain", inst::Instruction)
    print(io, inst_str(inst, Int32(0)))
end

"Show a CapEntry"
function Base.show(io::IO, ::MIME"text/plain", cap::CapFrame)
    print(io, "s:$(Int(cap.s)) $(repr("text/plain",cap.inst))}")
end

"Show a vector of Bytecode instructions."
function Base.show(io::IO, ::MIME"text/plain", code::IVector)
    if isempty(code)
        return print(io, "IVec[]")
    end
    compact = get(io, :compact, false)
    if compact
        print(io,"IVec[1:$(length(code))]")
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

"Show a vector of patterns."
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


"Show a Pattern."
function Base.show(io::IO, ::MIME"text/plain", patt::Pattern)
    compact = get(io, :compact, false)
    if compact
        print(io, typeof(patt))
        return
    end
    print(io, patt_str(patt))
end


"Show a Grammar."
function Base.show(io::IO, ::MIME"text/plain", patt::PGrammar)
    if !haskey(patt.aux, :start) # Not yet compiled
        return print(io, patt_str(patt))
    end
    green(s::Union{String,Symbol}) = "\x1b[32m$s\x1b[0m"
    bold(s::Union{String,Symbol}) = "\x1b[1m$s\x1b[0m"
    orange(s::Union{String,Symbol}) ="\x1B[38;5;208m$s\x1B[0m"
    meta = patt.aux
    cl = length(patt.code)
    pad = cl > 9999 ? 5 : cl > 999 ? 4 : cl > 99 ? 3 : 2
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
            label = green(" :$(mapsite[idx])")
        end
        line = ["  $ipad: "]
        frags = inst_pieces(inst, idx)
        if inst isa CaptureInst
            pop!(frags)
            if meta[:caps][inst.tag] !== nothing
                push!(frags, orange(" $(meta[:caps][inst.tag])"))
            end
        end
        append!(line, frags)
        push!(line, label)
        if !(inst isa InstructionVec) && inst.op == ICall
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

"Show a VMState."
function Base.show(io::IO, ::MIME"text/plain", vm::VMState)
    if get(io, :compact, false)
        print(io, vm_head(vm), "\n")
    else
        print(io, vm_to_str(vm))
    end
end

# Pattern Printer

function patt_str(patt::Pattern)::String
    T = typeof(patt)
    if T == PCapture
        lines=[patt.kind, "("]
    else
        lines=[T, "("]
    end
    push!(lines, "val→", repr("text/plain", patt.val, context=:compact=>true), ", ")
    if hasfield(T, :n)
        push!(lines, "n=", string(patt.n), ", ")
    end
    push!(lines, repr("text/plain", patt.code, context=:compact=>true), ")")
    join(lines) * ")"
end

# Instruction printing helpers

function inst_str(inst::Instruction, off::Integer)::String
    "⟪$(join(inst_pieces(inst, off)))⟫"
end

function inst_pieces(inst::HoldInst, ::Integer)::Vector{String}
    ["HOLD INSTRUCTION → ", " $(inst.op)"]
end

function inst_pieces(inst::CharInst, ::Integer)::Vector{String}
    line = ["$(inst.op) "]
    ch::UInt32 = ((UInt32(inst.one) << 24)
                 + (UInt32(inst.two) << 16)
                 + (UInt32(inst.three) << 8)
                 + inst.four)
    char = reinterpret(Char, ch)
    if Int(char) ≤ 31
        push!(line, repr(char))
    else
        push!(line, string(char))
    end
    return line
end

"Vector of instruction string fragments"
function inst_pieces(inst::Instruction, off::Integer)::Vector{String}
    if inst == OpNoOp || typeof(inst) == InstructionVec
        return ["  Vector: $(printset(inst))"]
    end
    line = ["$(inst.op)"]
    t = typeof(inst)
    if t == CaptureInst
        pop!(line)
        push!(line, "$(inst.kind)")
        if inst.op == IOpenCapture
            push!(line, " open")
        elseif inst.op == ICloseCapture
            push!(line, " close")
        else
            push!(line, " full")
            push!(line, " ($(inst.n))")
        end
        push!(line, " #$(Int(inst.tag))")
        return line
    end
    if hasfield(t, :c)
        push!(line, " $(repr(inst.c))" )
    end
    if hasfield(t, :n)
        push!(line, " $(inst.n)")
    end
    if hasfield(t, :b)
        push!(line, " $(string(inst.b, base=16))")
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
    if hasfield(t, :vec)
        push!(line, " $(printset(inst))")
    end
    return line
end

"String for Set Vector"
function printset(vec::InstructionVec)::String
    chars = bitvector_to_compact_repr(vec)
    str = "{"

    str *= join(chars, ",")
    str *= "}"
    return str
end

"String for MultSet Vector"
function bitvector_to_compact_repr(bitvec::InstructionVec)::Vector{String}
    fragments = String[]
    start_idx = 0
    end_idx = 0
    function _encode(i::Integer)
        string(i, base=16, pad=2)
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

function vm_head(vm::VMState)::String
    o = vm.t_on ? 1 : 0
    b = vm.t_on ? "yes" : "no"
    "State: [i:$(vm.i)] {$(lcap(vm))} $(inst_str(vm.program[vm.i], vm.i)) $b ⟨$(length(vm.stack) + o)⟩ s:$(in_subject(vm.subject, vm.s))"
end

function vm_head_color1(vm::VMState)::String
    o = vm.t_on ? 1 : 0
    b = vm.t_on ? "yes" : "no"
    "State: [i:$(vm.i)] {$(lcap(vm))} $(inst_str(vm.program[vm.i], vm.i)) $b ⟨$(length(vm.stack) + o)⟩ s:$(in_red(vm.subject, vm.s))"
end

function vm_head_color(vm::VMState)::String
    vm_head_color1(vm) * "\n"
end

function frame_to_str(vm::VMState, i, s, c)::String
    inst = vm.program[i]
    if s == 0
        "[i:$(i)] {$c} $(inst_str(inst, i))"
    else
        "[i:$(i)] {$c} $(inst_str(inst, i)) s:$(in_red(vm.subject, s))"
    end
end

function vm_to_str(vm::VMState)::String
    lines = [vm_head_color1(vm)]
    if !vm.t_on
        push!(lines, "Frame: []")
    else
        push!(lines, "Frames:")
        push!(lines, frame_to_str(vm, vm.ti, vm.ts, vm.tc))
    end
    for frame in Iterators.reverse(vm.stack)
        push!(lines, frame_to_str(vm, frame.i, frame.s, frame.c))
    end
    if !isempty(vm.cap)
        push!(lines, "---")
        push!(lines, "Caps:")
        for entry in Iterators.reverse(vm.cap)
            push!(lines, repr(entry))
        end
    end
    push!(lines, "---\n\n")
    return join(lines, "\n")
end

function in_red(str::String, i::UInt32)
    if isempty(str)
        return "\"\""
    end
    if i == 0
        str1 = "\x1B[38;5;208m*\x1B[0m"
        str2 = firstten(str)
        return str1 * str2
    elseif i > sizeof(str)
        str2 = "\x1B[38;5;208m*\x1B[0m"
        str1 = lastten(str)
        return str1 * str2
    end
    red_i(substr_at_i(str, i)...)
end

function red_i(first::AbstractString, at::AbstractString, last::AbstractString)::AbstractString
    bra = "\x1B[31m⟪"
    ket = "⟫\x1B[0m"
    return first * bra * at * ket * last
end

function in_subject(str::String, i::UInt32)
    if isempty(str)
        return "\"\""
    end
    if i == 0
        str1 = "⟪*⟫"
        str2 = firstten(str)
        return str1 * str2
    elseif i > sizeof(str)
        str2 = "⟪*⟫"
        str1 = lastten(str)
        return str1 * str2
    end
    subject_i(substr_at_i(str, i)...)
end

function firstten(str::String)
    count = 0
    for _ in graphemes(str)
        count += 1
        if count > 10
            break
        end
    end
    if count < 10
        return str
    else
        return graphemes(str, 1:count)
    end
end

function lastten(str::String)
    count = 0
    for _ in graphemes(str)
        count += 1
    end
    if count < 10
        return str
    else
        return graphemes(str, count-10:count)
    end
end

function subject_i(first::AbstractString, at::AbstractString, last::AbstractString)::AbstractString
    s_start = "⟪"
    s_end = "⟫"
    return first * s_start * at * s_end * last
end


function substr_at_i(str::String, i::UInt32)
    # Determine the start index
    idx = 0
    at  = 0
    before = 0
    after = 0
    pre = true
    for graph in graphemes(str)
        if pre
            before += 1
        end
        idx += sizeof(graph)
        if pre && idx > i
            at = before - 1
            pre = false
            continue
        elseif pre && idx == i
            at = before
            pre = false
            continue
        end
        if !pre
            after += 1
        end
        if after ≥ 7
            break
        end
    end
    if at < 1
        at = 1
    end
    # five characters back, or first character
    if before > 8
        start = at - 7
    else
        start = 1
    end
    finish = at + after
    return graphemes(str, start:at-1), graphemes(str, at:at), graphemes(str, at+1:finish)
end
