
"""
    oncapmatch(vm::VMState, start=1)::PegMatch

Process the capture list and return what we find.
"""
function aftermatch(vm::VMState, start::Integer=1)::PegMatch
    function _substr(s, f)
        # s should always be a valid index, f might not be,
        # and is in all cases one past what we need
        f1 = prevind(vm.subject, f)
        return @views (vm.subject[s:f1])
    end
    if vm.s == vm.top + 1
        full = true
    else
        full = false
    end
    captures = PegCapture()
    patt = vm.patt
    # there may not be any captures, in which case the whole
    # matched string is the capture:
    if lcap(vm) == 0
        push!(captures, @views vm.subject[start:prevind(vm.subject, vm.s)])
        return PegMatch(vm.subject, full, captures, patt)
    end
    # Otherwise:
    # To handle nested captures of all sorts we use a stack
    capstack = []
    # And another stack for grouping captures
    groupstack = []
    capsdict = patt.aux[:caps]
    for i in 1:lcap(vm)
        cap = vm.cap[i]
        if cap.inst.op == IOpenCapture
            push!(capstack, cap)
            if cap.inst.kind == Cgroup
                # Push our current captures onto the group stack
                push!(groupstack, captures)
                captures = PegCapture()
            end
            continue
        end
        if cap.inst.op == IFullCapture
            # Make a synthetic back capture, reusing this Instruction
            # The only distinct value of bcap we use is .s,
            # Which we calculate thus:
            bcap = CapFrame(cap.s - cap.inst.n, cap.inst)
            if cap.inst.kind == Cgroup # We'll need a cap vector
                push!(groupstack, captures)
                captures = PegCapture()
            end
        elseif cap.inst.op == ICloseCapture
            bcap = pop!(capstack)
            if bcap.inst.tag ≠ cap.inst.tag
                open, close = capsdict[bcap.inst.tag], capsdict[bcap.inst.tag]
                otag, ctag = Int(bcap.inst.tag), Int(cap.inst.tag)
                @warn "mismatched tags: open: $open #$otag, close $close #$ctag"
            end
        end
        ikey = cap.inst
        key = capsdict[cap.inst.tag]
        if bcap.inst.kind == ikey.kind
            if ikey.kind == Csimple
                push!(captures, _substr(bcap.s, cap.s))
            elseif ikey.kind == Csymbol
                sub = _substr(bcap.s, cap.s)
                push!(captures, key => sub)
            elseif ikey.kind == Cgroup
                # grab the outer captures and offsets
                caps = pop!(groupstack)
                if isempty(captures)  # the group is the capture
                    push!(captures, _substr(bcap.s, cap.s))
                    if key !== nothing
                        push!(caps, key => captures)
                    else
                        push!(caps, captures)
                    end
                    captures = caps
                    continue
                end
                if key !== nothing
                    push!(caps, key => captures)
                else
                    push!(caps, captures)
                end
                captures = caps
            elseif ikey.kind == Cposition
                # Note: the original intention was to capture an empty SubString,
                # Those are unfortunately broken (they set the offset to 0) so
                # I'm going to use a numbered position capture for now and
                # maybe write a whole package which does this one thing correctly
                push!(captures, Int(bcap.s))
            elseif ikey.kind == Crange
                if key !== nothing
                    push!(captures, key => [bcap.s:prevind(vm.subject, cap.s)])
                else
                    push!(captures, [bcap.s:prevind(vm.subject, cap.s)])
                end
            elseif ikey.kind == Cconst
                push!(captures, key)
            elseif ikey.kind == Caction
                λ = key::Function
                # The Action either created the group, or it *is* the group
                if ikey.op == IFullCapture || isempty(captures)
                    arg = _substr(bcap.s, cap.s)
                    if isempty(captures) && isempty(groupstack)
                        push!(captures, λ(arg))
                    elseif !isempty(captures)
                        # A FullCapture inside another group to be closed later
                        push!(captures, λ(arg))
                    elseif isempty(captures) && !isempty(groupstack)
                        # We had to use an OpenCap Action, captures is discarded
                        captures = pop!(groupstack)
                        push!(captures, λ(arg))
                    end
                else  # all of captures is our arguments
                    args = captures
                    if isempty(groupstack)
                        captures = PegCapture()
                    else
                        captures = pop!(groupstack)
                    end
                    push!(captures, λ(args...))
                end
                # remove `nothing` matches
                if captures[end] === nothing
                    pop!(captures)
                end
            else
                @warn "doesn't handle the case of $(ikey.kind) yet!"
            end
        else
            error("Unbalanced caps begin $(bcap.inst.kind) end $(cap.inst.kind)")
        end
    end
    if !isempty(capstack)
        amount = length(capstack) == 1 ? "entry" : "entries"
        @warn "left $(length(capstack)) $amount on the capture stack: $(capstack)"
    end
    return PegMatch(vm.subject, full, captures, patt)
end

function afterfail(vm::VMState)::PegFail
    if vm.failtag == 0
        return PegFail(vm.subject, vm.sfar, :default)
    end
    throws = vm.patt.aux[:throws]
    return PegFail(vm.subject, vm.sfar, throws[vm.failtag])
end

## Core Method extensions

"""
    match(patt::Pattern, subject::AbstractString)::Union{PegMatch, PegFail}

Match `patt` to `subject`, returning a `PegMatch` implementing the expected interface
for its supertype `AbstractMatch`, or a `PegFail` with useful information about the
failure.
"""
function Base.match(patt::Pattern, subject::AbstractString)::Union{PegMatch, PegFail}
    vm = VMState(patt, subject)
    runvm!(vm) ? aftermatch(vm) : afterfail(vm)
end
function Base.match(patt::Pattern, subject::AbstractString, bounds::UnitRange{<:Integer})::Union{PegMatch, PegFail}
    vm = VMState(patt, subject, bounds.start, bounds.stop)
    runvm!(vm) ? aftermatch(vm, bounds.start) : afterfail(vm)
end
function Base.match(patt::Pattern, subject::AbstractString, start::Integer)::Union{PegMatch, PegFail}
    vm = VMState(patt, subject, start, ncodeunits(subject))
    runvm!(vm) ? aftermatch(vm, start) : afterfail(vm)
end

"""
    findfirst(patt::Pattern, string::AbstractString)::Union{Integer, Nothing}

Find the first match of `patt` in `string`. Returns the index at the *end* of the match,
such that `@views string[1:findfirst(patt)]` is the match.
"""
function Base.findfirst(patt::Pattern, string::AbstractString)::Union{Integer, Nothing}
    vm = VMState(patt, string)
    runvm!(vm) ? vm.s - 1 : nothing
end

"""
    occursin(needle::Pattern, haystack::AbstractString)

Check if `needle` matches in `haystack`.  PEG patterns, unlike regex, must match
from the first character in the string; to convert a pattern `p` to match anywhere,
use `psearch = "" >> p`.
"""
function Base.occursin(needle::Pattern, haystack::AbstractString)::Bool
    findfirst(needle, haystack) !== nothing ? true : false
end
