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

## Interface

The API of JLpeg hews closely to [Lpeg](http://www.inf.puc-rio.br/~roberto/lpeg/),
with several extensions and refinements.  The basic operations are as follows:

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
| `!patt`, `¬patt`        | Negative lookahead, succeeds if `patt` fails                |
| `~patt`                 | Lookahead, match `patt` without advancing                   |
| `patt1` >> `patt2`      | Match `patt1`, then search the string for the next `patt2`. |
| `P(true)`, `P(false)`   | Always succeed or always fail, respectively                 |
| `B(patt)`               | Match `patt` behind the cursor, without advancing           |

In keeping with the spirit of LPeg, `P"string"` is equivalent to `P("string")`, and
this is true for `S` and `R` as well.

## Patterns, Rules, and Grammars

The simplest use of JLPeg is as a drop-in replacement for regular expressions:

```jldoctest
julia> match(P"123", "123456")
PegMatch("123")
```

With the immediate advantage that patterns combine.

Patterns are immutable once defined, and may be built up incrementally into more
complex patterns.  Although this may be done with the constructors in `pattern.jl`,
it's far more pleasant and readable to use operators, like so:

```jldoctest
julia> match(P"abc" * "123", "abc123")
PegMatch("abc123")

julia> match(P"abc" | "123", "123")
PegMatch("123")

julia> match(P"abc"^1, "abcabcabc")
PegMatch("abcabcabc")

julia> match((!S"123" * R"09")^1, "0987654321")
PegMatch("0987654")

julia> match("" >> P"5", "0987654321")
PegMatch("098765")

julia> match(~P"abc", "abc123")
PegMatch("")

julia> match(~P"abc", "123abc") # fails

```

Note that unlike regular expressions, PEG always starts with the first character, any
match (other than `nothing`) returned by a call to `match(patt, string)` will
therefore be a prefix of the string, up to and including the entire string.  To get
something usefully similar to a regex, you can start with a capture pattern like
this:

```jldoctest
julia> match("" >> (P"56",), "1234567")
PegMatch("123456", 1="56")
```

JLPeg captures can nest, unlike regexes, so you can define another pattern with the
captures you want and use the recipe above to make it behave like a regex.

This matches the empty string, fast-forwards to the first 56, and captures it.  Note
that the pattern is `(P"56,)`, a tuple, not a group, this is syntax sugar for `P("")
>> C(P("56"))`.

The operators introduce a pattern 'context', where any `a <op> b` combination where
`a` or `b` is a Pattern will attempt to cast the other argument to a Pattern.  We
provide two convenience functions, `modulesugar()` and `extrasugar()`, which will
cast binary operations of `a::Symbol <op> a::String` (in either order) and both
binary and unary operations of `Symbol` to `Pattern`, as used in rules and grammars
(see below); the difference is that `modulesugar` defines these methods in the
enclosing module, and `extrasugar` defines them on `Base`.  Please use `modulesugar`
in any shared packages, as type piracy is impolite.

### Rules

To match recursive structures, patterns must be able to call themselves, which is a
`Rule`.  A collection of Rules is a Grammar.

As is the PEG convention, a rule is defined with `<=` or `←`.  A simple Grammar can
look like this:

```julia
abc_and = :a <= P"abc" * (:b | "")
_123s = :b ← P"123"^1 :a
abc123 = Grammar(abc_and, _123s)

match(abc123, "abc123123123abc123abc")
```

The variable names aren't a part of the rule, which is named by the left-hand symbol.

- [#TODO] A terminal rule, one which doesn't call other rules, may be called on
`match` like an ordinary pattern.

## Captures and Actions

| [ ] | Operation          | What it produces                                        |
| --- | ------------------ | ------------------------------------------------------- |
| [X] | `C(patt)`          | match for `patt` plus all captures made by `patt`       |
| [ ] | `Carg(n)`          | value of the `n`th extra argument                       |
| [ ] | `Cb(key)`          | values of the the previous group capture named `key`    |
| [ ] | `Cc(values)`       | given `values` (matches the empty string)               |
| [ ] | `Cg(patt [, key])` | values produced by `patt`, optionally tagged with `key` |
| [ ] | `Cp()`             | current position (matches the empty string)             |
| [ ] | `Cs(patt)`         | match for patt with the values from nested captures     |
|     |                    | replacing their matches                                 |
| [ ] | `Ct(patt)`         | a table with all captures from patt                     |
| [ ] | `patt / string`    | string, with some marks replaced by captures of patt    |
| [ ] | `patt / number`    | nth value captured by patt                              |
| [ ] | `patt / vec`       | Vector of capturs                                       |
| [ ] | `patt / λ`         | the returns of function applied to the captures of patt |
| [ ] | `patt % λ`         | reduces captures with λ                                 |
| [ ] | `Cmt(patt, λ)`     | λ applied to match-time captures at match time          |
