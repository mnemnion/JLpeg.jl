
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

# Type Aliases

"A Vector of `Instruction`s representing a complete pattern."
const IVector = Vector{Instruction}

"A Vector of `Pattern`s."
const PVector = Vector{Pattern}

"""
    AuxDict = Dict{Symbol, Any}

The `.aux` field of any compound `Pattern`, contains the auxiliary data
needed to correctly compile the pattern.
"""
const AuxDict = Dict{Symbol, Any}

# Enums

"Instruction opcodes for JLpeg VM"
@enum Opcode::UInt8 begin
    IIllegal    # To catch problems with erroneously reinterpreting vector sets
    IAny        # if no char, fail
    IChar       # if char != aux, fail
    ISet        # if char not in buff, fail
    ILeadSet    # ASCII lead of MultiSet
    IByte       # Test the next byte at vm.s
    IMultiVec   # match a set of end bytes of multibyte char
    ILeadMulti  # Extended lead of MultiSet
    ITestAny    # in no char, jump to 'offset'
    ITestChar   # if char != aux, jump to 'offset'
    ITestSet    # if char not in buff, jump to 'offset'
    INotSet     # Predicate, fails if set matches, doesn't advance s
    INotChar    # Predicate instruction for !P'char'
    IBehind     # walk back 'aux' characters (fail if not possible)
    IReturn     # return from a rule
    IEnd        # end of pattern
    IChoice     # stack a choice; next fail will jump to 'offset'
    IPredChoice # labeled failure: stack a choice; changes label env next fail will jump to 'offset'
    IJump       # jump to 'offset'
    ICall       # call rule at 'offset'
    IOpenCall   # call rule number 'key' (must be closed to a ICall)
    ICommit     # pop choice and jump to 'offset'
    IPartialCommit # update top choice to current position and jump
    IBackCommit # backtrack like "fail" but jump to its own 'offset'
    IFailTwice  # pop one choice and then fail
    IFail       # go back to saved state on choice and jump to saved offset
    IFullCapture # complete capture of last 'off' chars
    IOpenCapture # start a capture
    ICloseCapture
    ICloseRunTime
    IThrow      # fails with a given label
    IThrowRec   # fails with a given label and call rule at 'offset'
    IOpenMark   # Begin a marked region
    ICloseMark  # Close a marked region
    ICheckMark  # Close a marked region and check it
    INoOp       # to fill empty slots left by optimizations
end

"A kind of capture"
@enum CapKind::UInt8 begin
    Csimple     # Captures a substring of the region matched
    Csymbol     # Captures its match as a pair `:symbol => "match"` (:symbol can be a string)
    Cgroup      # Groups all its captures into a Vector.
    Cposition   # Captures the empty string to record an offset
    Cconst      # Adds provided constants to the capture vector
    Crange      # Captures a UnitRange [first:last] of region
    Caction     # An action taken on a successful match.
    Ctest       # A runtime test of the captured substring
    Cvm         # A runtime test receiving the full VMState
end

const CapKindDocs = Dict(
    Csimple =>      "Captures a substring of the region matched",
    Csymbol =>      "Captures its match as a pair `:symbol => \"match\"` (:symbol can be a string)",
    Cgroup =>       "Groups all its captures into a Vector.",
    Cposition =>    "Captures the empty string to record an offset",
    Cconst =>       "Captures provided constants into the capture vector",
    Crange =>       "Captures a UnitRange [first:last] of region",
    Caction =>      "An action taken on a successful match.",
    Ctest =>        "A runtime test of the captured substring",
    Cvm =>          "A runtime test receiving the full VMState",
)

Docs.getdoc(ck::CapKind) = Markdown.parse(CapKindDocs[ck])

const InstructionDocs = Dict(
    IIllegal =>    "To catch problems with erroneously reinterpreting vector sets.",
    IAny =>        "Match any n characters, or fail.",
    IChar =>       "Match one UTF-8 character, or fail.",
    IByte =>       "Match the next byte at vm.s, or fail.",
    ISet =>        "Match ASCII character in set, and jump to `.l`, or fail.",
    ILeadSet =>    "Match ASCII character in set, and jump to `.l`. Non-failing.",
    IMultiVec =>   "Match a set of end bytes of multibyte char and jump to `.l`, or fail.",
    ILeadMulti =>  "Match valid first bytes in a UTF-8 set. Non-failing.",
    ITestAny =>    "Match any n characters, or jump to `.l`",
    ITestChar =>   "Match one UTF-8 character, or jump to `.l`",
    ITestSet =>    "Match a Bitvector-encoded set of ASCII characters, or jump to `.l`.",
    INotSet =>     "Fails if a characterm matches the ASCII set. Doesn't advance `s`.",
    INotChar =>    "Match if one UTF-8 char does not match, or fail.",
    IBehind =>     "Walk back 'n' characters, fail if this is not possible.",
    IReturn =>     "Return from a rule.",
    IEnd =>        "End of pattern.",
    IChoice =>     "Stack a choice; next fail will jump to `.l`.",
    IPredChoice => "Stack a choice and set predicate flag to `true`.",
    IJump =>       "Jump to `.l`.",
    ICall =>       "Stack a call frame and call rule at `.l`.",
    ICommit =>     "Pop stack frame and jump to `.l`.",
    IPartialCommit => "Update top choice to current position and jump.",
    IBackCommit => "Backtrack like 'fail' but jump to its own `.l`.",
    IFailTwice =>  "Pop one choice and then fail.",
    IFail =>       "Unwind stacks to last choice frame and jump to saved offset.",
    IFullCapture => "Complete capture of last `.n` chars.",
    IOpenCapture => "Start a capture.",
    ICloseCapture => "Close a capture.",
    ICloseRunTime => "Close a runtime action.",
    IThrow =>      "Fails with a given label.",
    IThrowRec =>   "Fails with a given label and call rule at `.l`.",
    IOpenMark =>   "Begin a marked region.",
    ICloseMark =>  "Close a marked region.",
    ICheckMark =>  "Close a marked region and check it against rule `.tag`.",
    INoOp =>       "To fill empty slots left by optimizations.",
    IOpenCall =>   "Call rule number 'key' (must be closed to a ICall).",
)

Docs.getdoc(op::Opcode) = Markdown.parse(InstructionDocs[op])