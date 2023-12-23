# Planning for jLPEG

  The intention here is to translate [LPEG](https://github.com/sqmedeiros/lpeglabel), specifically the lpeglabel fork, into idiomatic Julia, and expand it with the various bells and whistles which I've introduced, or plan to, into PEGs generally.

## Primary Sources

In addition to LPEG itself (linked above) we have:

- [LPEG Paper](https://www.inf.puc-rio.br/~roberto/docs/peg.pdf) paper describing the implementation of LPEG.


## Prior Art

- [parsimonious.jl](https://github.com/gitfoxi/Parsimonious.jl): A port of a Python library of the same name.  This takes strings and outputs ParseTrees, which will be of some use.  No idea about the internals yet.

- [PEG.jl](https://github.com/wdebeaum/PEG.jl): "Define a Parsing Expression Grammar via a macro and abuse of Julia syntax", a promising quote.  Not how I'm intending to implement this but we'll see what happens. Also no idea about internals.

- [PEGParser.jl](https://github.com/abeschneider/PEGParser.jl): uses Packrat, has a useful-looking `@grammar` macro.  Other than "it's packrat", no idea how it works yet.


## Tools

- [Match.jl](https://juliaservices.github.io/Match.jl/stable/): a macro for match/case style statements, which is an approach to the VM proper.


## Notes on OG Lpeg

The OG code contains the basic algorithms we need, but they're expressed in a way which makes a close translation useless. For one thing they heavily manipulate the Lua VM, and for another, it's written in C, with all that implies. 

The flow in the OG begins at the bottom of `lpltree.c`, where the Lua interface is created and registered, and works its way backward from the perspective I need to take to translate it. 

The interface is various ways of creating patterns, the details of which are largely useless for us, since it's all about pulling the relevant information out of Lua into C.  This involves creating two structs, `Pattern`s and `TTree`s, where a Pattern is a container for both a Tree and its bytecode.  Trees are tagged with an enum saying what variety of tree we're dealing with, a second enum indicating the variety of capture (if it is one), either zero, one, or two children, and sometimes a counter. 

Trees are built into more complex patterns through combinator rules, which become parent to the existing patterns, and as grammars, where they eventually get checked and resolved in various ways. 

`lp_match` causes the tree to be compiled, and these instructions are passed to the VM for match proper, which runs the bytecode. 

## Strategy

The distinction between Pattern and TTree isn't something we really need, I don't think.  The distinction is basically between the encapsulation of Lua-side userdata, code, and a TTree, where the latter is used to do manipulation of the pattern stuff without affecting the Lua VM.  We can just have Patterns as containers that also do the Tree-like things. 

## Implementation Notes and Details

### UTF-8 Charsets

OG Lpeg uses charsets, a 256 bit array for detecting the presence of a character. I'd like to find a nice compact way to represent a utf-8 charset, which takes advantage of the lower bit width of the following characters. 

For many applications, we'll just want one charset-as-normal, but if we're parsing utf8 we can have a second kind with additional followsets.

The SecondCharset is a nine byte vector, where the first byte is the matched character and the rest are the valid followthroughs.  We have as many of those as we need, then ThirdCharsets of ten bytes and FourthCharsets of 11.  We don't need the SecondCharsets if all the variation is in the third byte, and same for the FourthCharset and the Third, this is a common pattern in real utf-8 ranges. 

This structure can be pessimized by very sparse character sets, but we'll know in advance how many characters we're trying to compress into a set.  The very nature of defining a Unicode range means that those will be the sort of narrow which we want to fit into this data structure, but as a generalizable rule for optimization, we only want to use this instead of Alt comparisons if the Charset approach would be shorter in total length.

As I think about it, these are distinct, rather than aggregate: SingleCharset, SecondCharset, ThirdCharset, and FourthCharset.  


## Instructions

New ones are noted as such.

### Char _char_

Julia has a native unicode character type, so this can be up to four bytes.

### Jump _label_

### Choice _label_ _offset_

### Call _label_

### OpenCall _string_

### Return

### Commit _label_

### PartialCommit _label_

### BackCommit _label_

### Capture _directive_ _offset_

Directives are _begin_, _end_, and _full_ (both _begin_ and _end_)

### NameCapture _directive_ _offset_ _name_ (new)

These captures are retained by name and used in comparisons. Work in progress.

### Fail

### FailTwice

### End 

### Any _n_ 

### SingleCharset 

### SecondCharset (new)

### ThirdCharset (new)

### FourthCharset (new)

### Span _Charset_

### TestChar _Char_ _label_

### TestCharset _Charset_ _label_

### TestAny _n_ _label_