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