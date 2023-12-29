# Grammar macro module

using MacroTools


const postwalk, prewalk = MacroTools.postwalk, MacroTools.prewalk

function wrap_rule(expr::Expr)::Expr
    function for_x(x)
        if x isa String || x isa QuoteNode
            return :(P($x))
        elseif x isa Expr
            if @capture(x, (@S_str(P(val_))))
                :(@S_str($val))
            elseif @capture(x, (@R_str(P(val_))))
                :(@R_str($val))
            elseif @capture(x, ((cap_, P(sym_))))
                :(($cap, $sym))
            else
                x
            end
        else
            return x
        end
    end
    @capture(expr, (sym_ ← rulebody_) | (sym_ <= rulebody_)) || error("malformed rule in $(expr)")
    :($sym ← $(postwalk(for_x, rulebody)))
end


"""
    @grammar(name, rules)

Syntax sugar for defining a set of rules as a single grammar. Expects a block `rules`,
each of which is a rule-pair as can be created with `<=` or `←`.  `"string"` will be
interpreted as `P("string")`, and `:symbol` as `P(:symbol)`.  Be sure to use the
macro forms `S"123"` and `R"az"` for sets and ranges, which will otherwise be
transformed into `S(P("123"))`, which is invalid.

## Example use

This simple grammar captures the first string of numbers it finds:

```jldoctest
julia> @grammar capnums begin
    :nums  ←  (:num,) | P(1) * :nums # Numbers are not wrapped
    :num   ←  S"123"^1
end
[...]
julia> match(capnums, "abc123abc123")
PegMatch(["123"])
```

More extensive examples may be found in the documentation.
"""
macro grammar(name, expr)
    @capture(expr, begin rules__ end)
    local rs = [wrap_rule(rule) for rule in rules]
    local g = :($(esc(name)) = Grammar($(rs...)))
    return g
end
