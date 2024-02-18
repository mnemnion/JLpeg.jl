# Captures and Actions

```@meta
CurrentModule = JLpeg
DocTestSetup = quote
    using JLpeg
    import JLpeg.Combinators: *, -, %, |, ^, ~, !, >>, >:, inv
end
```

PEGs define a recognizer of a string, not a scanner.  The match always begins with the
first character, and either matches as much of the string as it is able, or fails.
Patterns can be made to scan with `>>`, but this will still match the whole prefix of
the string.  Most practical work with patterns will use captures, to capture the
region of interest.

To capture the substrings you're looking for, use `C(patt)`, or just make a tuple
`(patt,)`.

```jldoctest
julia> match("" >> (P"56",), "1234567")
PegMatch(["56"])
```

This matches the empty string, fast-forwards to the first 56, and captures it.  Note
that the pattern is `(P"56",)`, a tuple, not a group; this is syntax sugar for
`P("") >> C(P("56"))`.  The capture can receive a key, which may be either a
[`Symbol`](@extref `Core.Symbol`) or a [`String`](@extref `Strings`): `(P"56", :fiftysix)` or `(P"56", "fifty six")`.

## Captures

| [❓]  | Operation               | What it produces                                       |
|------|:------------------------|:-------------------------------------------------------|
| [✅]  | `C(patt [, key])`,      | captures the substring of `patt`                       |
| [✅]  | `(patt,)`               | same as above, note the comma!                         |
| [✅]  | `(patt, key)`           | `key` may be `:symbol` or `"string"`                   |
| [✅]  | `Cg(patt [, key])`,     | captures a Vector of values produced by `patt`,        |
| [✅]  | `[patt], [patt, key]`   | optionally tagged with `key`                           |
| [✅]  | `Cp()`                  | captures `""` so `PegMatch.offsets` has the position   |
| [🔶] | `Cc(any)`                | places `any` in `.captures` at the current offset      |
| [✅]  | `Cr(patt [, key])`      | Range of indices [start:end] of `patt`, optional `key` |

Some more examples:

```jldoctest
julia> @rule :cap123 ← [((S"123"^1,) | R"az"^1)^1];

julia> match(cap123, "abc123zyz123def")
PegMatch([["123", "123"]])

julia> @rule :cap_pos ← [((S"123"^1,) | R"az"^1 * Cp())^1];

julia> match(cap_pos, "abc123zyz123def")
PegMatch([[4, "123", 10, "123", 16]])

julia> @rule :capABC ← [((S"ABC"^1,) | R"az"^1)^1, :capABC];

julia> match(capABC, "abcBCAzyzCCCdef")
PegMatch([:capABC => ["BCA", "CCC"]])
```

The form in `:capABC`, where the rule is grouped as a capture and given a symbol
which is the same as the rule name, is extremely common and gets its own shorthand:

```jldoctest
julia> @rule :capABC <--> ((S"ABC"^1,) | R"az"^1)^1;

julia> match(capABC, "abcBCAzyzCCCdef")
PegMatch([:capABC => ["BCA", "CCC"]])
```

With both `⟷` and `↔︎` as synonyms, these are `\longleftrightarrow` and
`\:left_right_arrow:` respectively.

## Actions

A pattern may be modified with an action to be taken, either at while matching, or
once the match has completed.  Here's a summary of the available actions, which we
will describe in some detail in this section.

| [✅]  | Action               | Consequence                                       |
|------|-----------------------|:--------------------------------------------------|
| [✅] | `A(patt, λ)`,         | Captures the return value of `λ`, as applied      |
| [✅] | `patt <\| λ`          | to the captures in `patt`                         |
| [✅] | `Q(patt, λ)`          | Tests the substring of `patt` with `λ(str)::Bool` |
| [✅] | `Avm!(patt, λ)`       | If `patt` matches, calls `λ(vm)::Bool`            |
| [✅] | `M(patt, :label)`     | `M`ark a the region of `patt` for later reference |
| [✅️] | `K(patt, :label, op)` | Chec`K` `patt` against the last mark with `op`    |
| [✅] | `T(:label)`,          | Fail the match and throw `:label`                 |
| [✅] | `patt % :label`       | Shorthand for `patt \| T(:label)`                 |

The use of `<|` is meant to be mnemonic of [`|>`](@extref
`Function-composition-and-piping`) for ordinary piping (and shares its
usefully low precedence), without pirating the meaning of the pipe operator.  This way
`patt |> λ` will do the expected thing, `λ(patt)`.

`A` acts on captures.  It works like a group capture, taking the substring of `patt`,
or if `patt` contains captures, it receives those captures as a [`Vararg`](@extref
`Core.Vararg`), and the return value is inserted into the capture vector, without
splatting.  In the current implementation this only happens after a match is
completed, but future releases may choose to evaluate some captures earlier.
Accordingly, `λ` passed to `A` should be free of side effects.  To stress a point,
`A` already has group-capture semantics, so in patterns like `[patt] <| λ`, `λ` will
receive a [`PegCapture`](@ref), not the contents of `[patt]`.

`Q`, a "query" action, happens during the match, as soon as `patt` is matched.  It
receives the region matched by `patt` as a SubString, and must return a `Bool`, which
determines if that pattern succeeds or fails.

As an example, here's a use of `Q` to match strings of digits divisible by 3 in base 10.

```@setup byThree
using JLpeg
import JLpeg.Combinators: *, -, %, |, ^, ~, !, >>, inv
```

```@repl byThree
bythree(str) = parse(Int, str) % 3 == 0
mthree = Q(R"09"^1, bythree);
match(mthree, "333")
match(mthree, "335")
```

While this kind of inline validation can be useful, `Q` can be put to more
interesting uses.  With care, and some closed-over state, this can be used to handle
the [context sensitivity of parsing
C](https://eli.thegreenplace.net/2007/11/24/the-context-sensitivity-of-cs-grammar),
or other languages in which the meaning of a 'token' changes based on its definition.
The resulting parser would not be reentrant, which I don't find completely
satisfactory.  I hope to provide a more durable solution in a later release.

`Q` only takes the actual SubString of `patt`, and it is a runtime error for `patt`
to have any captures.  This is for clarity of semantics as well as performance.  A
future release may change this behavior, either by extending the semantics of `Q` or
adding another action, and this also ensures that such a feature won't change the
behavior of valid patterns.  Note that `Q` is not itself a capture, and it may be
contained within as many levels of capturing as you would like.

`Avm!` also calls a provided `λ` if `patt` matches, but instead of being provided
with a SubString, the argument is the entire [`VMState`](@ref).  The function must
return a boolean, which as usual is the success or failure of the pattern.  The
obvious use for this action is debugging, or setting resource limits on a parser (for
example, checking the depth of `vm.stack`), this is also the suggested way to yield a
parser running inside a Task.  The internals of the VM are documented, but aren't
considered part of JPpeg's public interface, so if you succumb to the temptation to
use this power for some sort of fancy hack, I suggest writing tests which confirm
that whatever internals that hack relies on continue to function as expected. Caveat
lector!

Mostly, I expect users will be content to use `Q` to provide arbitrary runtime
behavior to a pattern, or if there's a need to compare two regions of the string, the
mark and check system, described next.

## Marks and Checks

Validation and parsing of strings frequently requires comparison between two
substrings.  For our example, let's consider this toy XML tag grammar.

```@setup toyXML
using JLpeg
```

```@repl toyXML
@grammar xmltag begin
    :doc ← :tags * !1
    :tags ← :opentag * :tags^0 * :closetag
    :opentag ← "<" * R"az"^1 * ">"
    :closetag ← "</" * R"az"^1 * ">"
end;

match(xmltag, "<a><b></b></a>")

match(xmltag, "<a><b></b>")
```

So far so good! We've got a nice recursive tag matcher, without [summoning Zalgo](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454).  One problem though: it allows any close tag to match any open tag.

```@repl toyXML
match(xmltag, "<a><b></a></b>")
```

To solve this, we may use the mark and check mechanism, like so.

```@repl toyXML
@grammar xmltag begin
    :doc ← :tags * !1
    :tags ← :opentag * :tags^0 * :closetag
    :opentag ← "<" * M(R"az"^1, :tag) * ">"
    :closetag ← "</" * K(R"az"^1, :tag) * ">"
end;

match(xmltag, "<a><b></a></b>")

match(xmltag, "<a><b></b></a>")
```

That's more like it!  This mechanism allows for a fully declarative PEG grammar which
matches well-formed XML.  It's also applicable to Python-style indentation, Lua's long
strings, and much more.

`K` has a two-argument form (shown), where the check confirms that the two substrings
are identical.  In the three-argument form, a function may be provided, which must
have the signature `(marked::AbstractString, checked::AbstractString)::Bool`, with
the result of calling the function on the regions of interest used to pass or fail
the match.  This may also be written in [do syntax](@extref
`Do-Block-Syntax-for-Function-Arguments`).

For the most common comparisons, `JLpeg` provides several built-ins, which may be
invoked by providing these symbols as the third argument.  These don't have to
allocate SubStrings or look up a function, and should always be preferred for what
they do.

| Built-in  |           Match when           |
| :-------- | :----------------------------- |
| `:(==)`   | Regions are identical          |
| `:length` | Regions have the same length   |
| `:close`  | There is a mark with this tag  |
| `:always` | Whether the mark exists or not |
| `:gt`     | length(r2) > length(r1)        |
| `:lt`     | length(r2) < length(r1)        |
| `:gte`    | length(r2) ≥ length(r1)        |
| `:lte`    | length(r2) ≤ length(r1)        |

The builtin `:(==)` is the default used in the two-argument form of [`K`](@ref).  The
length comparisons in the table may look backward, but are easy to remember in
practice: `K(patt, :tag, :gt)` means "this is greater than that", and so on.  These
are abbreviations and not symbols, because, for one example, `K(patt, :tag, >)` is a
perfectly valid check, but will use lexicographic comparison (as always with strings
and `>`), not length comparison.  `:(==)` is identical in meaning to `==` in this
context, but may be safely elided.

Note that all of the length-examining builtins use byte width, or codeunits. Not
codepoints, graphemes, or [`textwidth`](@extref `Base.Unicode.textwidth`).  This is
the fastest of the options and probably the least surprising.  The `length` in
codepoints was considered and rejected; considered because Julia uses it as the
standard concept of string length, rejected for a few reasons.  One, the regions
under comparison have already been verified with patterns, so they aren't unknowns.
Two, both Julia and JLpeg allow invalid UTF-8 in string types, and while the behavior
of `length` on invalid UTF-8 is defined, it's borderline useless in this context.
Three, user might be tempted to treat this as grapheme length, which it emphatically
is not, creating code which works sometimes but not consistently.

Bespoke comparisons based on some other standard may always be employed as
user-provided check functions.

With the exception of `:always`, a check will fail if there is no mark with the same
key.  This includes user-provided functions, which won't trigger if a mark isn't
found. All checks, _including_ `:always`, are only performed if the enclosed pattern
succeeds.  If your comparison doesn't rely on a prior region, you want `Q`, not `K`.

Marks and checks are independent of the capture mechanism, but since the regions of
interest are frequently worth capturing, we provide [`CM(patt, :sym)`](@ref CM) as a
shorthand for `C(M(patt, :sym), :sym)`, and [`CK(patt, :sym, check)`](@ref CK) as
shorthand for `C(K(patt, :sym, check), :sym)`.

### Checks and Predicates

Successful checks will remove the corresponding mark, unless they're inside
predicates, `~` or `!`.  The "exception to the exception" is `:always`, which, given
the semantics of the name, and the fact that it exists entirely to remove marks, we
felt should do what it says on the label.

This differing behavior inside predicates is the better semantic, since it allows
lookahead for a checked mark which is later consumed.

As an example, we'll show a grammar for [Lua's long
strings](https://www.inf.puc-rio.br/~roberto/lpeg/lpeg.html#ex).  This serves to
illustrate both the motive for our choice of check behavior inside predicates, and
the reason for adding JLpeg's innovative mark and check mechanism. LPeg can provide
equivalent functionality for many cases, using a more general but somewhat cumbersome
mechanism, the "match-time capture", with no direct equivalent in JLpeg.

```@setup longstring
using JLpeg #hide
```

```@repl longstring
@grammar longstr begin
    :str ← :open * :body * :close
    :open ← '[' * M("="^0, :equals) * '['
    :body ← ((!:close * 1)^0, :string)
    :close ← ']' * K("="^0, :equals) * ']'
end;

match(longstr, "[[]]")

match(longstr, "[[long strings]]")

match(longstr, "[==[end with a ]=] token]==]")

match(longstr, "[==[the equals must balance]=]")
```

Lua's long strings are a nice bit of syntax, because they have the enclosure
property: it is always possible to wrap a literal string as a program string, without
modifying the string itself, because the equals signs in e.g. `[===[` must be matched
with `]===]`.  I wish Julia had a [string
syntax](https://github.com/JuliaLang/julia/discussions/53171) which functions the
same way.

We see that the `:body` rule contains `(!:close * 1)^0`, a pattern which experienced
PEG users will recognize as matching zero or more characters, so long as the
lookahead doesn't match the `:close` rule.

Because the negative-lookahead `:close` always matches the `:close` rule first on the
closing region, if `K` didn't behave differently inside predicates, this would
consume the mark, so the `:close` in the `:str` rule would always fail.

### Limitation and Performance

Marks and checks come with an **important limitation**: they must not contain other
marks and checks.  Any other pattern is ok, including captures and all other actions.
A future version of JLpeg may check for this condition and refuse to compile, the
current behavior will silently corrupt your parse.  The author's opinion is that this
is no limitation in practice, after all, one might match the longer region and
perform whatever inner checks are desired.  If you find yourself with a real-world
grammar which would benefit from nested marks, feel free to open an issue, and we can
decide if the complexity, performance impact, and unclear semantics (does an inner
mark come before, or after, its enclosing mark?) is worth it.

Note that marks are only removed once a corresponding check succeeds, and grammars or
strings which don't close their marks will leave them on the mark stack.  This is
mostly harmless, the worst that can come of it is JLpeg throwing an `InexactError`
once there are more than `typemax(UInt16)` marks on the stack (assuming the OS can
provide adequate amounts of heap), but it's certainly possible to create bad
performance.  For example, by stacking up a bunch of mark `:a` while checking for
mark `:b`, every check will be forced to fruitlessly look for the nonexistent mark on
a growing stack of marks.  It takes some real effort to produce a grammar which will
do this, however.

If you have a grammar where some paired regions may be implicitly closed, you can use
`K(patt, :tag, :always)` to close the mark by fiat; this check succeeds whether the
mark exists or not.  If you want the check to fail if the mark doesn't exist, use
`K(patt, :tag, :close)` instead.  This comes up in Markdown parsing, where, for
example, `**` for emphasis is allowed to end when the paragraph ends, without needing
to be closed.

Final note: this thorough discussion might leave some with the impression that marks
and checks aren't performant, are tricky, to be avoided in practice, etc.  Fear not!
Typical uses will consume marks nearly as fast as they're generated, and it's rare
for marks to overlap, that is, normally the check will be compared against the latest
mark.  To cite our XML example, the mark stack will be as deep as the tags are
nested, comparisons are always against the top mark, and any mismatch fails the
entire parse. This has the same time complexity as a grammar which doesn't validate
that tags match, with a small added constant factor which is well-used.

Grammars normally have tidy properties, forming a neat heirarchical tree in which
options either close themselves by succeeding or are backtracked off the stack (and
of course, marks within a choice which fails are removed during backtracking). Proper
use of `M` and `K` requires the author to balance this mechanism on their own.  While
it's possible to write a self-balancing analogue of this mechanism, we felt that the
added flexibility was more than worth the additional care which must be taken.
Consider what would be involved in writing the `longstr` grammar above, if `:close`
had to be structurally balanced against `:open` somehow.

Practical use of mark and check is as fast as it reasonably can be, and enables
recognition of many common patterns in strings.

## Throws and Recovery

The greatest challenge for good parsing has always been error reporting and recovery.
With old-school [lex and yacc](https://www.wikiwand.com/en/Yacc), the conventional
wisdom was to develop the grammar for a language or DSL using the compiler-compiler
toolkit, to assure that the grammar is actually in a useful context-free class, then
handroll a recursive-descent parser for production use, in order to provide users
with useful error messages when they inevitably create syntax errors.

[`JLpeg`](../index.md), being a PEG parser, is a formalization of the classic
recursive-descent parsing strategy.  It includes a mechanism pioneered by
[lpeglabel](https://github.com/sqmedeiros/lpeglabel), modestly improved in our
implementation, which allows a pattern to throw a specific error when an expected
aspect of parsing is violated.

`T(:label)` fails the match, records the position of that failure, and throws
`:label`.  If there is a rule by that name, it is attempted for error recovery,
otherwise `:label` and the error position are attached to the `PegFail` struct, in
the event that the whole pattern fails.  The label `:default` is reserved by JLpeg
for reporting failure of patterns which didn't otherwise throw a label.

Consider a simplified pattern for matching single-quoted strings:

```@setup badstring
using JLpeg #hide
```

```@repl badstring
@rule :string ← "'" * (!"'" * 1)^0 * "'";

match(string, "'a string'")

match(string, "'not a string")
```

We do mark the point of failure, which is better than average; a normal PEG or parser
combinator will silently fail to match, without informing the user of where. But we
can do better with throws.

```@repl badstring
@rule :string  ←  "'" * (!"'" * 1)^0 * ("'" % :badstring)

match(string, "'not a string")
```

This provides us both with the point of failure, and the cause, data which can be
used to provide a helpful error message to the user.

A Throw with a matching rule will attempt that rule on throw, if this succeeds, the
parse continues.  This can be used to embed errors while continuing to check the
validity of the grammar, as in this example.

```@repl badstring
@grammar strmatch begin
    :strings ← " "^0 * "'" * (!S"'\n" * 1)^0 * ("'" % :missedend) * :strings^0 * !1
    :missedend ← ("\n", :str_newline_error)
end;

match(strmatch, "'a string' 'another string'")

match(strmatch, "'a string\n 'another string'")

match(strmatch, "'a string' 'another string")
```

Here we have a grammar matching at least one single-quoted string, which may not
contain a literal newline. If we fail to match a string, the `:missedend` rule looks
for a newline, which it captures and tags, enabling the parse to continue.  Note that
when the recovery rule doesn't match, the label remains in place, and will be
provided if the whole parse fails at that location.

Subsequent code can look for `:str_newline_error`, or any number of such
error-signalling keys.  Since a [`SubString`](@extref `Base.SubString`) has its start
point in the `.offset` field, this may be used to inform the user where the missing
close quote belongs.

A note about `patt % :throw`: the `%` has alt semantics, so the label is thrown only
when `patt` fails.  Sometimes you want to throw when a pattern succeeds, this is
accomplished with `patt * T(:throw)`.

Throws, like checks, behave differently inside predicates: they become a normal
failure, without setting the label or trying any recovery rule.  Pattern failure in
lookahead has a different meaning than failure to consume input, generally speaking,
and this little tweak allows more patterns to be used both for lookahead and ordinary
matching without having to write them twice. If you want to throw because of lookahead,
this is easy enough, `!'\n' % :throw` will throw a the label if there's a newline.
