
"""
    Instruction

Abstract type of JLPeg VM instructions.

The possible struct fields have consistent meanings:

| Fields  | Meaning                                            |
|---------|----------------------------------------------------|
| `op`    |  The `Opcode`                                      |
| `l`     |  An instruction offset                             |
| `n`     |  A subject offset                                  |
| `c`     |  An `AbstractChar` to match                        |
| `b`     |  A test byte                                       |
| `tag`   |  Key in a capture/throw Dict                       |
| `vec`   |  A set's test `BitVector`                          |
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
    PegError(msg)

An error while constructing a JLPeg Pattern.
"""
struct PegError <: Exception
    msg::String
end
