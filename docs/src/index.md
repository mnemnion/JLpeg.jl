# JLpeg: Pattern Matching and Parsing For Julia

```@meta
CurrentModule = JLpeg
DocTestSetup = quote
    using JLpeg
    import JLpeg.Combinators: *, -, %, |, ^, ~, !, >>, >:, inv
end
```

JLpeg provides a fast [Parsing Expression
Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) engine for
matching patterns in strings, using a bytecode virtual machine based on the
pioneering work of [Roberto
Ierusalimschy](https://www.inf.puc-rio.br/~roberto/docs/peg.pdf).

Compared to regular expressions, PEGs offer greater power and expressivity.  They
match a superset of regex patterns, while formalizing and extending the deviations
from regular languages offered by production regex engines such as PCRE.  PEGs are
able to parse recursive rule patterns, employ lookahead and lookbehind predicates,
and freely intermix functions for context-sensitive matching.  JLpeg has an
innovative mechanism for matching against two regions of a string, which can match
opening and closing XML tags, and cleanly parse indentation-sensitive languages.

Compared with parser combinators, a more common algorithm for matching PEG grammars,
the approach taken by this package is superior.  A bytecode interpreter allows
several key optimizations which parser combinators do not in practice employ.
Generally such libraries choose between a naive backtracking algorithm with bad time
complexity, and a memorizing packrat algorithm which trades this for bad space
complexity, with consequent memory pressure.  Critically, a `JLpeg` pattern of the
form `a ← "b" / . a` becomes a loop, rather than consuming the program stack.  JLpeg
generates programs which may be inspected and modified, and uses an innovative
thrown-label pattern to allow excellent error reporting and recovery.

Compared with a "compiler compiler" such as ANTLR or the classic yacc/bison, `JLpeg`
does not generate source code, but rather bytecode, which Julia is able to JIT into
near-optimal machine code on the fly.  These systems require an imput stream to be
reduced to tokens, an abstraction PEGs do not need, which allows `JLpeg` to parse
contextually-valid grammars which cannot be readily tokenized.  PEGs are also far
more suitable for scanning, captures, and other pattern-recognition tasks than these
programs, which are only well-suited to parsing of full grammars, a task `JLpeg` also
excels at.

!!! warning "UTF-8 Native"

    JLpeg is a parser for [UTF-8](https://en.wikipedia.org/wiki/UTF-8), **only**.
    This is the native encoding for Julia, and the only one anyone should be
    using, in the author's opinion.  Text in other encodings must be converted,
    or JLpeg patterns will simply fail.  The interface accepts AbstractStrings,
    which includes those in other encodings, and no attempt is made to detect
    this particular failure mode.  JLpeg will parse invalid UTF-8 sequences
    without throwing errors, and even provides the raw patterns needed to
    match them, but provides no way to convert patterns to work with any
    other encoding.  If you have strings in another format, you can use
    [`transcode`](@extref `Base.transcode`) from the Julia standard library to
    convert them to be compatible with the rest of Julia, and JLpeg.
