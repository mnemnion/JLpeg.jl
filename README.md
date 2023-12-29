# JLpeg: Pattern Matching and Parsing For Julia

JLpeg provides a fast PEG engine for matching patterns in strings, using a bytecode
virtual machine based on the pioneering work of [Roberto
Ierusalimschy](https://www.inf.puc-rio.br/~roberto/docs/peg.pdf).

Compared to regular expressions, PEGs offer greater power and expressivity, being, in
a sense, a formalized and extended version of the deviations from regular languages
offered by production regex engines such as PCRE.  PEGs are able to parse recursive
rule patterns, employ lookahead and lookbehind predicates, and avoid the sort of
worst-case complexity the regex is prone to, for most useful patterns.

Compared with parser combinators, a more common algorithm for matching PEG grammars,
the approach taken by this package is superior.  A bytecode interpreter allows
several key optimizations which parser combinators do not allow; generally such
libraries choose between a naive backtracking algorithm with bad time complexity and
a memorizing packrat algorithm which trades this for bad space complexity, with
consequent memory pressure.  JLpeg generates programs which may be inspected and
modified, and uses an innovative thrown-label pattern to allow excellent error
reporting and recovery.

Compared with a "compiler compiler" such as ANTLR or the classic yacc/bison, JLpeg
does not generate source code, but rather bytecode, which Julia is able to JIT into
near-optimal machine code on the fly.  PEGs are also far more suitable for scanning,
captures, and other pattern-recognition tasks than these programs, which are best
suited to full grammars, a task JLPeg also excels at.

## Patterns

Parsing Expression Grammars are built out of patterns, which begin with atomic units
of recognition and are combined into complex rules which can recognize context free,
and some context sensitive, languages.  LPeg, JLpeg's inspiration, uses a
Snobol-style set of operator overloads as the basic tool for building up patterns, a
practice we also follow.

The API of JLpeg hews closely to [Lpeg](http://www.inf.puc-rio.br/~roberto/lpeg/),
with several extensions, refinements, and a more natively Julian character.

The basic operations are as follows:

| Operator                | Description                                                 |
| ----------------------- | ----------------------------------------------------------- |
| `P(s::String)`          | Match a literal String `s`                                  |
| `P(s::Integer)`         | Match any `n` characters                                    |
| `S(s::String)`          | Match the set of all characters in `string`                 |
| `R("xy")`, `R('x','y')` | Matches any character between `x` and `y` (Range)           |
| `B(patt)`               | Match `patt` behind the cursor, without advancing           |
| `patt^n`                | Match `n` repetitions of `patt` at most                     |
| `patt^-n`               | Match `n` repetitions of `patt` at least                    |
| `patt1 * patt2`         | Match the sequence `patt1` , `patt2`                        |
| `patt1 \| patt2`        | Match `patt1` or `patt2`, in that order                     |
| `patt1 - patt2`         | Match `patt1` if `patt2` does not match                     |
| `!patt`, `¬patt`        | Negative lookahead, succeeds if `patt` fails                |
| `~patt`                 | Lookahead, match `patt` without advancing                   |
| `patt1` >> `patt2`      | Match `patt1`, then search the string for the next `patt2`. |
| `P(true)`, `P(false)`   | Always succeed or always fail, respectively                 |

In keeping with the spirit of LPeg, `P"string"` is equivalent to `P("string")`, and
this is true for `S` and `R` as well.  These basic operations are not self
referential, and without further modification merely match the longest substring
recognized by the pattern, but suffice to match all regulat languages.

## Matching

`match(pattern::Pattern, string::String)` will attempt to match the pattern against
the string, returning a `PegMatch <: AbstractMatch`.  Note that unlike regular
expressions, JLpeg will not skip ahead to find a pattern in a string, unless the
pattern is so constructed.  We offer the easy shorthand `"" >> patt` to convert a
pattern into its searching equivalent; `P""` matches the empty string, and JLPeg will
convert strings and numbers (but not booleans) into patterns when able.

```jldoctest
julia> match(P"123", "123456")
PegMatch(["123"])

julia> match(P"abc" * "123", "abc123")
PegMatch(["abc123"])

julia> match(P"abc" | "123", "123")
PegMatch(["123"])

julia> match(P"abc"^1, "abcabcabc")
PegMatch(["abcabcabc"])

julia> match((!S"123" * R"09")^1, "0987654321")
PegMatch(["0987654"])

julia> match("" >> P"5", "0987654321")
PegMatch(["098765"])

julia> match(~P"abc", "abc123")
PegMatch([""])

julia> match(~P"abc", "123abc") # fails

```

The operators introduce a pattern 'context', where any `a <op> b` combination where
`a` or `b` is a Pattern will attempt to cast the other argument to a Pattern.  This UI is adequate for light work, but the macros discussed later are much cleaner.

Note that unlike regular expressions, PEG always starts with the first character, any
match (other than `nothing`) returned by a call to `match(patt, string)` will
therefore be a prefix of the string, up to and including the entire string.

Most interesting uses of pattern recognition will call for more than matching the
longest substring.  For those more complex cases, we have captures and actions.

## Captures and Actions

A `PegMatch` defaults to the longest `SubString` when no captures are provided, or
when the pattern succeeds but all captures within fail (#TODO I may change this
behavior).  To capture only the substring of interest, use `C(patt)` or just make a
tuple `(patt,)`.  Don't forget the comma or Julia will interpret this as a group.

```jldoctest
julia> match("" >> (P"56",), "1234567")
PegMatch(["56"])
```

This matches the empty string, fast-forwards to the first 56, and captures it.  Note
that the pattern is `(P"56,)`, a tuple, not a group, this is syntax sugar for
`P("") >> C(P("56"))`.

| [ ] | Operation               | What it produces                                        |
| --- | ----------------------- | ------------------------------------------------------- |
| [X] | `C(patt [, key])`       | captures the substring of `patt`                        |
| [X] | `(patt,)`               | same as above, note the comma!                          |
| [X] | `(patt, key)`           | `key` may be `:symbol` or `"string"`                    |
| [X] | `Cg(patt [, key])`      | values produced by `patt`, optionally tagged with `key` |
| [ ] | `[patt], ([patt], key)` |                                                         |
| [X] | `Cp()`                  | current position (matches the empty string)             |
| [X] | `Cr(patt [, key])`      | Range of indexes [start:end] of `patt`, optional `key`  |
| [X] | `A(patt, λ)`,           | the returns of function applied to the captures of patt |
| [X] | `patt / λ`              | ""                                                      |
| [ ] | `Anow(patt, λ)`,        | λ applied to match-time captures at match time          |
| [ ] | `patt // λ`             |                                                         |
| [ ] | `patt % λ`              | fold/reduces captures with λ                            |

### Rules

To match recursive structures, patterns must be able to call themselves, which is a
`Rule`.  A collection of Rules is a Grammar.

As is the PEG convention, a rule reduction uses the left arrow `←`, which you can
type as `\leftarrow` (or in fact `\lef[TAB]`).  We overload `<=` if you happen to hate
Unicode.  A simple Grammar can look like this:

```julia
abc_and = :a <= P"abc" * (:b | P"")
_123s   = :b ← P"123"^1 * :a
abc123  = Grammar(abc_and, _123s)

match(abc123, "abc123123123abc123abc")
```

The `@grammar` and `@rule` macros are much prettier ways to make a pattern, however:

```julia
@grammar abc123 begin
:a  ←  "abc" * (:b | "")
:b  ←  "123"^1 * :a
```

Always use `R"az"` and `S"abc"`` forms for ranges and sets in these macros, or you'll
get the wrong result.

