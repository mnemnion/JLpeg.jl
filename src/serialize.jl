# Serialize and marshall a grammar

# Intended for compile-time macros and interchange formats

hexit(i::Integer) = string(i, base=16)

encode(io::IO, op::JLpeg.Opcode) = write(io, Char(UInt8(op)+65))
encode(io::IO, kind::JLpeg.CapKind) = write(io, Char(UInt8(kind)+65))
encode(io::IO, bytes::Vector{UInt8}) = write(io, '[', hexit(bytes[1]), ',', hexit(bytes[2]), ']')
encode(io::IO, c::AbstractChar) = write(io, c)
encode(io::IO, vec::BitVector) = write(io, "PASS")
function encode(io::IO, n::Integer)
    if n ≤ 26
        write(io, Char(n+97))
    else
        write(io, ':', string(n, base=16))
    end
end

function field_args(T::DataType)
    local params = []
    for field in fieldnames(T)
        push!(params, :(encode(io, inst.$field)))
    end
    return :(encode(io::IOBuffer, inst::$(T)) = ($(params...),))
end

begin
    for I in subtypes(JLpeg.Instruction)
        eval(field_args(I))
    end
end

function encode_instruction(inst::JLpeg.Instruction)
    yo = IOBuffer()
    encode(yo, inst)
    String(take!(yo))
end
