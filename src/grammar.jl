# Grammar macro module

using MacroTools

const postwalk, prewalk = MacroTools.postwalk, MacroTools.prewalk

const 🔠 = P  # Won't interfere with user uses of P

const ops = Set([:*, :|, :^, :~, :¬, :!, :>>, :|>, :>, :(=>), :%, :./,])
const JPublic = Set(names(JLpeg)) ∪ ops

function wrap_rule_body(rulebody::Expr)::Expr
    function for_x(x)
        if x isa String || x isa QuoteNode || x isa Char || x isa Integer
            return :(🔠($x))
        elseif x isa Symbol && !(x ∈ JPublic)
            return esc(x)
        elseif x isa Expr
            # Unmunge various pattern'ed values
            if @capture(x, (@S_str(🔠(val_))))
                :(@S_str($val))
            elseif @capture(x, (@R_str(🔠(val_))))
                :(@R_str($val))
            elseif @capture(x, ((cap_, 🔠(sym1_))))
                :(C($cap, $sym1))
            elseif @capture(x, ([val_, 🔠(sym2_)]))
                :(Cg($val, $sym2))
            elseif @capture(x, val_^🔠(sym3_))
                :($val^$sym3)
            elseif @capture(x, fn_(params__))
                if fn ∈ ops
                    return x
                end
                for (idx, param) ∈ enumerate(params)
                    if isexpr(param) && param.head == :call && param.args[1] == :🔠
                        params[idx] = param.args[2]
                    end
                end
                :($(fn)($(params...)))
            else
                return x
            end
        else
            return x
        end
    end
    :($(postwalk(for_x, rulebody)))
end

function wrap_rule(expr::Expr)::Expr
    if @capture(expr, (sym_ ← rulebody_) | (sym_ <-- rulebody_))
        return wrap_rule_body(expr)
    elseif @capture(expr, (sym_ <--> rulebody_))
        local body = wrap_rule_body(rulebody)
        :(C($sym ← $body, $sym))
    else
        error("malformed rule in $(expr)")
    end
end


"""
    @grammar(name, rules), @grammar(name, vars, rules)

Syntax sugar for defining a set of rules as a single grammar. Expects a block
`rules`, each of which is a rule-pair as can be created with `←`, or, if you must,
`<=`.  `"string"` will be interpolated as `P("string")`, and `:symbol` as
`P(:symbol)`.  Be sure to use the macro forms `S"123"` and `R"az"` for sets and
ranges, which will otherwise be transformed into `S(P("123"))`, which is invalid.

The three-expression form of `@grammar` takes a tuple of variable names, which are to
be escaped, that is, interpreted as what they mean in the local scope.

## Example use

This simple grammar captures the first string of numbers it finds:

```jldoctest
julia> @grammar capnums begin
            :nums  ←  (:num,) | 1 * :nums
            :num   ←  S"123"^1
        end;
JLpeg.PGrammar(val→[JLpeg.PRule,JLpeg.PRule], IVec[]))

julia> match(capnums, "abc123abc123")
PegMatch(["123"])
```

This one also captures the lowercase letters, converting them to uppercase. Because
`uppercase` is in the list of variables, it is interpreted locally; otherwise the
`:abc` rule would use `JLpeg.uppercase`, which is seldom what you would want.

```jldoctest
julia> upper = uppercase;  # Creating a synonym with no definition within JLpeg

julia> @grammar uppernums (upper,) begin
           :nums  ←  (:num,) | :abc * :nums
           :num   ←  S"123"^1
           :abc   ←  R"az"^1 |> upper
       end;


julia> match(uppernums, "abc123abc123")
PegMatch(["ABC", "123"])
```

More extensive examples may be found in the documentation.
"""
macro grammar(name, expr)
    @capture(expr, begin
        rules__
    end)
    local rs = [wrap_rule(rule) for rule ∈ rules]
    :($(esc(name)) = Grammar($(rs...)))
end

"""
    @rule :name ← pattern...

Sugared form for rule definition. Assigns the rule in-scope with the given `name`:

```julia
# Wrong
name = @rule :name  ←  "foo" | "bar"
# Right
@rule :name ← "foo" | "bar"
```
"""
macro rule(expr)
    if @capture(expr, (sym_ ← rulebody_) | (sym_ <-- rulebody_))
        local r = wrap_rule(expr)
        local name = sym.value
        :($(esc(name)) = $r)
    elseif @capture(expr, (sym_ <--> rulebody_))
        local body = wrap_rule_body(rulebody)
        local name = sym.value
        :($(esc(name)) = C($sym ← $body, $sym))
    else
        error("malformed rule in $(expr)")
    end
end
