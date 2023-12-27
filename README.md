# JLpeg: Pattern Matching and Parsing For Julia

JLpeg provides a fast PEG engine for matching patterns in strings, using a bytecode virtual machine based on the pioneering work of [Roberto Ierusalimschy](https://www.inf.puc-rio.br/~roberto/docs/peg.pdf).

Compared to regular expressions, PEGs offer greater power and expressivity, being, in a sense, a formalized and extended version of the deviations from regular languages matched by production regex engines such as PCRE.  PEGs are able to parse recursive rule patterns, employ lookahead and lookbehind predicates, and avoid the sort of worst-case complexity the regex is prone to for most useful patterns.

Compared with parser combinators, a more common algorithm for matching PEG grammars, the approach taken by this package is superior.  A bytecode interpreter allows several key optimizations which parser combinators do not allow; generally such libraries choose between a naive backtracking algorithm with bad time complexity and a memorizing packrat algorithm which trades this for bad space complexity, with consequent memory pressure.  JLpeg generates programs which may be inspected and modified, and uses an innovative thrown-label pattern to allow excellent error reporting and recovery.

Compared with a "compiler compiler" such as ANTLR or the classic yacc/bison, JLpeg does not generate source code, but rather bytecode, which Julia is able to JIT into near-optimal machine code on the fly.  PEGs are also far more suitable for scanning, captures, and other pattern-recognition tasks than these programs, which are best suited to full grammars, a task JLPeg also excels at.


## Interface

The API of JLpeg hews closely to [Lpeg](http://www.inf.puc-rio.br/~roberto/lpeg/), with several extensions and refinements.  The basic operations are as follows:

| Operator                | Description                                                 |
| ----------------------- | ----------------------------------------------------------- |
| `P(s::String)`          | Match a literal String `s`                                  |
| `P(s::Integer)`         | Match any `n` characters                                    |
| `S(s::String)`          | Match the set of all characters in `string`                 |
| `R("xy")`, `R('x','y')` | Matches any character between `x` and `y` (Range)           |
| `patt^n`                | Match `n` repetitions of `patt` at most                     |
| `patt^-n`               | Match `n` repetitions of `patt` at least                    |
| `patt1 * patt2`         | Match the sequence `patt1` , `patt2`                        |
| `patt1 \| patt2`        | Match `patt1` or `patt2`, in that order                     |
| `patt1 - patt2`         | Match `patt1` if `patt2` does not match                     |
| `!patt`                 | Looks ahead for `patt`, succeeds if it does *not* find it   |
| `~patt`, `¬patt`        | Match `patt` without advancing in the string                |
| `patt1` >> `patt2`      | Match `patt1`, then search the string for the next `patt2`. |
| `P(true)`, `P(false)`   | Always succeed or always fail, respectively                 |
| `lpeg.B(patt)`          | Match `patt` behind the cursor, without advancing           |

In keeping with the spirit of LPeg, `P"string"` is equivalent to `P("string")`, and this is true for `S` and `R` as well.

`match(pattern::Pattern, string::String)` will attempt to match the pattern against the string, returning the index of the farthest match on success, and `nothing` on failure.  Note that unlike regular expressions, JLpeg will not skip ahead to find a pattern in a string, unless the pattern is so constructed.  We offer the easy shorthand `"" >> patt` to convert a pattern into its searching equivalent; `P""` matches the empty string, and JLPeg will convert strings and numbers (but not booleans) into patterns when able.


## Rules

Patterns are immutable once defined, and may be built up incrementally into more complex patterns.  To match recursive structures, patterns must be able to call themselves, which is a `Rule`.  A collection of Rules is a Grammar.

As is the PEG convention, a rule is defined with `<=` or `←`.  A simple Grammar can look like this:

```julia
abc_and = :a <= P"abc" * (:b | "")
_123s = :b ← P"123"^1 :a
abc123 = Grammar(abc_and, _123s)

match(abc123, "abc123123123abc123abc")
```

The variable names aren't a part of the rule, which is named by the left-hand symbol