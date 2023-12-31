# Grammar macro module

using MacroTools

const postwalk, prewalk = MacroTools.postwalk, MacroTools.prewalk

const 🔠 = P  # If you had a name collision here, welp. Dirty hack...

const ops = [:*, :|, :^, :~, :¬, :!, :>>, :/, ://, :(=>), :%]

function wrap_rule(expr::Expr)::Expr
    function for_x(x)
        if x isa String || x isa QuoteNode || x isa Char
            return :(🔠($x))
        elseif x isa Expr
            if @capture(x, (@S_str(🔠(val_))))
                :(@S_str($val))
            elseif @capture(x, (@R_str(🔠(val_))))
                :(@R_str($val))
            elseif @capture(x, ((cap_, 🔠(sym1_))))
                :(C($cap, $sym1))
            elseif @capture(x, ([val_, sym_]))
                if sym isa Expr && sym.head == :call
                    :(Cg([$val, $(sym.args[2])]))
                else
                    return x
                end
            elseif @capture(x, fn_(params__))
                if fn in ops
                    return x
                end
                for (idx, param) in enumerate(params)
                    if isexpr(param) && param.head == :call && param.args[1] == :🔠
                        params[idx] = param.args[2]
                    end
                end
                :($fn($(params...)))
            else
                return x
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
interpolated as `P("string")`, and `:symbol` as `P(:symbol)`.  Be sure to use the
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
    :($(esc(name)) = Grammar($(rs...)))
end

"""
    @rule :name ← [pattern...]

Sugared form for rule definition. Assigns the rule in-scope with the given `name`:

```julia
# Wrong
name = @rule :name  ←  "foo" | "bar"
# Right
@rule :name ← "foo" | "bar"
```
"""
macro rule(expr)
    @capture(expr, (sym_ ← rulebody_) | (sym_ <= rulebody_)) || error("malformed rule in $(expr)")
    local r = wrap_rule(expr)
    local name = sym.value
    :($(esc(name)) = $r)
end