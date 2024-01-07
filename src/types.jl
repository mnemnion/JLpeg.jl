
"""
    Instruction

Abstract type of JLPeg VM instructions.

The possible struct fields have consistent meanings:

| Field | Type                | Meaning                     |
| ----- | ------------------- | --------------------------- |
| `op`  | @enum `UInt8`       | The `Opcode`                |
| `l`   | `Int32`             | An instruction offset       |
| `n`   | `UInt32`            | A subject index             |
| `c`   | `AbstractChar`      | A character to match        |
| `b`   | `UInt8`             | A test byte                 |
| `tag` | `UInt32`            | Key in a capture/throw Dict |
| `vec` | `BitVector` (kinda) | A set's test `BitVector`    |
"""
abstract type Instruction end

"""
Container for various patterns and grammars.
Always has `val`, which may be primitive or a PVector,
and `code`, an IVector. Some patterns have a field
unique to that pattern type.
"""
abstract type Pattern <: AbstractPattern end

"""
    PegError(msg) <: Exception

An error while constructing a [`Pattern`](@ref).
"""
struct PegError <: Exception
    msg::String
end
