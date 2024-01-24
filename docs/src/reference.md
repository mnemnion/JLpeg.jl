# Reference

```@contents
Pages = ["reference.md"]
```

## Core API

The basic structure of recognition in JLPeg is a `Pattern`.

```@docs
JLpeg.Pattern
```

These are created in a wide variety of ways, all based on three functions,
[`P`](@ref), [`S`](@ref), and [`R`](@ref), with the rest of the work done with a
great quantity of operator overloading.

```@docs
JLpeg.PegMatch
JLpeg.PegFail
JLpeg.PegError
```

## Pattern Matching

```@docs
match
JLpeg.compile!
```

## Constructing Patterns

Patterns are built by [combining](index.md#Combination) the products of these constructors
into more complex Patterns.

```@docs
JLpeg.P
JLpeg.@P_str
JLpeg.S(s::AbstractString)
JLpeg.@S_str
R(::AbstractString)
R(::AbstractChar, ::AbstractChar)
JLpeg.@R_str
JLpeg.B
```

## JLpeg.Combinators

```@docs
JLpeg.Combinators
```

## Captures and Actions

PEG patterns are recognizers, matching the longest prefix of the string which they're
able.  [Captures](index.md#Captures) allow for substrings to be captured within the
match, [Actions](index.md#Actions) perform actions at the index of the match, or on a
capture.

```@docs
JLpeg.C
JLpeg.Cg
JLpeg.Cr
JLpeg.Cc
JLpeg.Cp
JLpeg.M
JLpeg.CM
JLpeg.K
JLpeg.CK
JLpeg.A
JLpeg.Anow
JLpeg.T
```

## Rules and Grammars

The great advantage (other than composability) PEGs have over regular expressions is
the ability to match recursive patterns.  These are constructed out of Rules and Grammars.

```@docs
JLpeg.Grammar
JLpeg.Rule
JLpeg.@rule
JLpeg.@grammar
```

## Dialects

A work in progress.

```@docs
JLpeg.re
```

## Generators

A PEG is a specification of a class of algorithms which are valid on a universe of
strings.  While the common thing to do is use this specification to construct a recognizer
for such strings, it may also be used to create generators for valid strings in that universe.

JLpeg aspires to provide a complete set of generators for our patterns. So far, we have:

```@docs
JLpeg.generate
```
