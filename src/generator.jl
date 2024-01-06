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
    if c[1].op == ISet
        _generateISet(buff, c[1])
        return String(take!(buff))
    elseif c[1].op == ILeadSet
        _generateISet(buff, c[1])
        start += 1
    end
    if c[start].op == ILeadMulti
        start += 1
    end
    # ...
    return String(take!(buff))
end

function _generateISet(buff::IOBuffer, inst::Instruction)

end
