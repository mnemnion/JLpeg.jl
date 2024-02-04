
"""
    Instruction

Abstract type of JLPeg VM instructions.

The possible struct fields have consistent meanings:

| Field   | Type                | Meaning                     |
| ------- | ------------------- | --------------------------- |
| `op`    | @enum `UInt8`       | The `Opcode`                |
| `l`     | `Int32|16`          | An instruction offset       |
| `n`     | `UInt32`            | A subject index or offset   |
| `c`     | `Char`              | A character to match        |
| `b`     | `UInt8`             | A test byte                 |
| `tag`   | `UInt16`            | Key in a capture/throw Dict |
| `check` | `UInt16`            | Which check to perform      |
| `vec`   | `BitVector` (kinda) | A set's test `BitVector`    |

There are also fields `moe`, `larry`, and `curly`, which are struct
padding, so that the `op` field (for the instructions which have it) are
in a consistent location.  These consist of all 1s and do not play a role
in the semantics of the VM.
"""
abstract type Instruction end

"""
Container for various patterns and grammars.  Always has `val`, which may be
primitive or a Vector of patterns, and `code`, a Vector of Instructions. Some
patterns have fields unique to that type of pattern.  A pattern which encloses
other patterns will have an `aux` field containing a Dict for metadata.
"""
abstract type Pattern <: AbstractPattern end

"""
    PegError(msg) <: Exception

An error while constructing a [`Pattern`](@ref).
"""
struct PegError <: Exception
    msg::String
end
