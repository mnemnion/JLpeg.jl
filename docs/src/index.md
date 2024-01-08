# JLpeg: Pattern Matching and Parsing For Julia

```@meta
CurrentModule = JLpeg
DocTestSetup = quote
    using JLpeg
end
```

JLpeg provides a fast [Parsing Expression
Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) engine for
matching patterns in strings, using a bytecode virtual machine based on the
pioneering work of [Roberto
Ierusalimschy](https://www.inf.puc-rio.br/~roberto/docs/peg.pdf).

Compared to regular expressions, PEGs offer greater power and expressivity, being, in
a sense, a formalized and extended version of the deviations from regular languages
offered by production regex engines such as PCRE.  PEGs are able to parse recursive
rule patterns, employ lookahead and lookbehind predicates, and avoid the sort of
worst-case complexity the regex is prone to, for most useful patterns.

Compared with parser combinators, a more common algorithm for matching PEG grammars,
the approach taken by this package is superior.  A bytecode interpreter allows
several key optimizations which parser combinators do not allow; generally such
libraries choose between a naive backtracking algorithm with bad time complexity, and
a memorizing packrat algorithm which trades this for bad space complexity, with
consequent memory pressure.  JLpeg generates programs which may be inspected and
modified, and uses an innovative thrown-label pattern to allow excellent error
reporting and recovery.

Compared with a "compiler compiler" such as ANTLR or the classic yacc/bison, JLpeg
does not generate source code, but rather bytecode, which Julia is able to JIT into
near-optimal machine code on the fly.  These systems work on tokens, an abstraction
PEGs don't use, which allows JLpeg to parse contextually-valid grammars which cannot
be readily tokenized.  PEGs are also far more suitable for scanning, captures, and
other pattern-recognition tasks than these programs, which are only well-suited to
parsing of full grammars, a task JLpeg also excels at.

## Patterns

Parsing Expression Grammars are built out of patterns, which begin with atomic units
of recognition and are combined into complex rules, which can recognize context free,
and some context sensitive, languages.  LPeg, JLpeg's inspiration, uses a
SNOBOL-style set of operator overloads as the basic tool for building up patterns, a
practice we also follow.

Although many users of JLpeg may prefer to use one of the [Dialects](#Dialects),
patterns and their combination are the building block of JLpeg recognition engines,
that which dialects compile to.

The API of JLpeg hews closely to [Lpeg](http://www.inf.puc-rio.br/~roberto/lpeg/),
with several extensions, refinements, and a more natively Julian character.

## Combination

The basic operations are as follows:

| Operator                | Description                                                 |
| ----------------------- | ----------------------------------------------------------- |
| `P(string::String)`     | match a literal String `string`                             |
| `P(n::UInt)`            | match any `n` characters                                    |
| `P(-n)`                 | match if there are at least `n` characters remaining        |
| `P(sym::Symbol)`        | match the rule named :sym                                   |
| `S(s::String)`          | match the set of all characters in `string`                 |
| `R("xy")`, `R('x','y')` | matches any character between `x` and `y` (Range)           |
| `B(patt)`               | match `patt` behind the cursor, without advancing           |
| `patt^n`                | match at least `n` repetitions of `patt`                    |
| `patt^-n`               | match at most `n` repetitions of `patt`                     |
| `patt1 * patt2`         | match the sequence `patt1` , `patt2`                        |
| `patt1 \| patt2`        | match `patt1` or `patt2`, in that order                     |
| `patt1 - patt2`         | match `patt1` if `patt2` does not match                     |
| `!patt`, `¬patt`        | negative lookahead, succeeds if `patt` fails                |
| `~patt`                 | lookahead, match `patt` without advancing                   |
| `patt1 >> patt2`        | match `patt1`, then search the string for the next `patt2`. |
| `P(true)` or `ϵ`        | always succeed                                              |
| `P(false)` or `∅`       | always fail                                                 |

In keeping with the spirit of LPeg, `P"string"` is equivalent to `P("string")`, and
this is true for `S` and `R` as well.  These basic operations are not recursive, and
without further modification merely match the longest substring recognized by the
pattern, but suffice to match all regular languages.

## Matching

[`match`](@ref)`(pattern::`[`Pattern`](@ref), `string::String)` will
attempt to match the pattern against the string, returning a [`PegMatch`](@ref) `<:
AbstractMatch`. In the event of a failure, it returns a [`PegFail`](@ref), with the
index of the failure at `.errpos`.  Note that unlike regular expressions, JLpeg will
not skip ahead to find a pattern in a string, unless the pattern is so constructed.
We offer the easy shorthand `"" >> patt` to convert a pattern into its searching
equivalent; `P""` matches the empty string, and JLPeg will convert strings and
numbers (but not booleans) into patterns when able.

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
PegFail("123abc", 1)
```

The operators introduce a pattern 'context', where any `a <op> b` combination where
`a` or `b` is a Pattern will attempt to cast the other argument to a Pattern.
Generally, a `MethodError` may be repaired by using `P` on the left side of the the
operator, although we can't guarantee that other method overloads for those operators
might apply. Notably, `*` is used for concatenation of strings; , although in the
JLpeg context, `P"abc" * P"123"` is in fact the same as `P("abc" * "123")`.

This UI is adequate for light work, but the [macros](#Rules-and-Grammars) discussed
later are cleaner to work with, defined such that `P` should never be necessary,
although any of the public names in the [`JLpeg`](reference.md) module may be used.

Note that unlike regular expressions, PEG always starts with the first character, any
match returned by a call to `match(patt, string)` will therefore be a prefix of the
string, up to and including the entire string.

Most interesting uses of pattern recognition will call for more than matching the
longest substring.  For those more complex cases, we have [`Captures`](#Captures) and
[`Actions`](#Actions).

### Rules and Grammars

While simple patterns may be composed by assigning to variables and using those
variable names to build up more complex patterns, this doesn't allow for recursion,
which is essential for matching many strings of interest, perhaps most.

For this purpose, we have rules, which are simply named patterns.  A rule with no
references to another rule within it may be used for matching directly, while those
with such references (including a reference to itself) must be composed into
grammars.

As is the PEG convention, a rule reduction uses the left arrow `←`, which you can
type as `\leftarrow` (or in fact `\lef[TAB]`), also defined as `<--`.  A simple
grammar can look like this:

```jldoctest
abc_and = :a <-- P"abc" * (:b | P"")
_123s   = :b ← P"123"^1 * :a
abc123  = Grammar(abc_and, _123s)

match(abc123, "abc123123123abc123abc")
# output
PegMatch(["abc123123123abc123abc"])
```

The first rule is the start rule, which must succeed if the match is to succeed.  A
grammar which is missing rules will throw a [`PegError`](@ref), but duplicate rules
are undefined behavior: currently JLpeg will compile the last rule of that name it
encounters, but this behavior must not be relied upon.

The preferred way to create rules and grammars is with the macros [`@rule`](@ref) and
[`@grammar`](@ref), which avoid the tedium of decorating expressions with `P`
entirely.  Any `Number`, `String`, `Symbol`, or `Char`, found on its own, is
converted into the pattern equivalent.  While this is not true of booleans, the
idiomatic way to spell `true` and `false` in JLpeg is `""` and `S""` respectively,
and these are compiled into the same code as `P(true)` and `P(false)`.

Exported variable names from `JLpeg` will always refer to the values they have in the
module, any other variable will be escaped, so it will have the expected meaning.

To give an example, this rule:

```julia
@rule :a ← "foo" * ["abc"^0 | S"123"]^1
```

Is equivalent to this expression:

```julia
a = :a ← P("foo") * Cg(P("abc")^0 | S("123"))^1
```

Although the definitions of the operators and string macros would allow this reduction:

```julia
a = :a ← P"foo" * Cg("abc"^0 | S"123")^1
```

Which is admittedly less cumbersome (we try).  Note that the `@rule` form doesn't
require the importation of `@S_str`, or any other public symbol, thanks to the nature
of Julia macros.

A classic example of a task forever beyond the reach of regular expressions is balancing parentheses, with JLpeg this is easy:

```jldoctest
julia> @grammar parens begin
           :par  ←  :s * !1
           :s  ← (:b | (!S"()" * 1))^1
           :b  ← '(' * :s * ')'
       end;

julia> match(parens, "(these (must) balance")
PegFail("(these (must) balance", 22)

julia> match(parens, "(these (must) balance)")
PegMatch(["(these (must) balance)"])

julia> match(parens, "(these (must) balance")
PegFail("(these (must) balance", 22)

julia> match(parens, "(these (must) balance))")
PegFail("(these (must) balance))", 24)

julia> match(parens, "(these (must))) balance)")
PegFail("(these (must))) balance)", 16)
```

`!1` is our equivalent of `$` in regex, a pattern which only succeeds at the end of
input.

The `@grammar` macro doesn't define variable names for the rules, only the grammar
name given before the expression block.  The first rule, as always, is the start
rule, as you can see, it needn't match the variable name.

## Captures

A `PegMatch` defaults to the longest `SubString` when no captures are provided, or
when the pattern succeeds but all captures within fail.  To capture only the
substring of interest, use `C(patt)` or just make a tuple `(patt,)`.  Don't forget
the comma or Julia will interpret this as a group.

```jldoctest
julia> match("" >> (P"56",), "1234567")
PegMatch(["56"])
```

This matches the empty string, fast-forwards to the first 56, and captures it.  Note
that the pattern is `(P"56",)`, a tuple, not a group; this is syntax sugar for
`P("") >> C(P("56"))`.

| [❓] | Operation               | What it produces                                       |
| --- | ----------------------- | ------------------------------------------------------ |
| [✅] | `C(patt [, key])`,      | captures the substring of `patt`                       |
| [✅] | `(patt,)`,              | same as above, note the comma!                         |
| [✅] | `(patt, key)`           | `key` may be `:symbol` or `"string"`                   |
| [✅] | `Cg(patt [, key])`,     | captures a Vector of values produced by `patt`,        |
| [✅] | `[patt], ([patt], key)` | optionally tagged with `key`                           |
| [✅] | `Cp()`                  | captures `""` so `PegMatch.offsets` has the position   |
| [🔶] | `Cc(any)`               | places `any` in `.captures` at the current offset      |
| [✅] | `Cr(patt [, key])`      | Range of indices [start:end] of `patt`, optional `key` |
| [🔶️] | `Ce(patt, :key)`,       | groups the captures of`patt` and creates an Expr       |
| [🔶] | `patt => :key`          | with head `:key` and args `[patt]...`                  |

## Actions

A pattern may be modified with an action to be taken, either at runtime, or, more
commonly, once the match has completed.  These actions are supplied with all captures
in `patt`, or the substring matched by `patt` itself if `patt` contains no captures
of its own.

`T(:label)` doesn't capture, but rather, fails the match, records the position
of that failure, and throws `:label`.  If there is a rule by that name, it is
attempted for error recovery, otherwise `:label` and the error position are attached
to the `PegFail` struct, in the event that the whole pattern fails.  The label `:default`
is reserved by JLpeg for reporting failure of patterns which didn't otherwise throw a label.

| [❓] | Action                | Consequence                                          |
| --- | --------------------- | ---------------------------------------------------- |
| [✅] | `A(patt, λ)`,         | the returns of `λ` applied to the captures of `patt` |
| [✅] | `patt \|> λ`          |                                                      |
| [⭕️] | `Anow(patt, λ)`,      | captures `λ(C(patt)...)` at match time, return       |
| [⭕️] | `patt > λ`            | `nothing` to fail the match                          |
| [✅] | `T(:label)`,          | fail the match and throw `:label`                    |
| [✅] | `patt % :label`       | shorthand for `patt \| T(:label)`                    |
| [⭕️] | `M(patt, :label)`     | mark a the region of `patt` for later reference      |
| [⭕️] | `K(patt, :label, op)` | checK `patt` against the last mark with `op`         |

## Dialects

The operator-combining pattern primitives are the basis of JLpeg. Being ordinary
Julia code, they offer the maximum level of flexibility and power, and are the basis
of the rest of the system.

For many uses, users may prefer one of the several dialects provided. These are
parsers written in, of course, JLpeg itself, which compile down to ordinary patterns.
