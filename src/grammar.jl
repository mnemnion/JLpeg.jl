# Grammar macro module

# TODO Make this a real module, don't forget ε and ∅ !

using MacroTools

const postwalk, prewalk = MacroTools.postwalk, MacroTools.prewalk

const 🔠 = P  # Won't interfere with user uses of P

const ops = Set([:*, :|, :^, :~, :¬, :!, :>>, :<|, :%,])
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
            elseif @capture(x, val_ % 🔠(sym4_))
                :($val % $sym4)
            elseif @capture(x, val_^[🔠(sym5_)])
                :($val^[$sym5])
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
    elseif @capture(expr, (sym_ ⟷ rulebody_) | (sym_ <--> rulebody_) | (sym_ ↔︎ rulebody_))
        local body = wrap_rule_body(rulebody)
        :($sym ← Cg($body, $sym))
    else
        error("malformed rule in $(expr)")
    end
end


"""
    @grammar(name, rules)

Syntax sugar for defining a set of rules as a single grammar. Expects a block
`rules`, each of which is a rule-pair as can be created with `←`, or, if you must,
`<=`.  `"string"` will be interpolated as `P("string")`, and `:symbol` as
`P(:symbol)`.  Be sure to use the macro forms `S"123"` and `R"az"` for sets and
ranges, which will otherwise be transformed into `S(P("123"))`, which is invalid.

## Example use

This simple grammar captures the first string of numbers it finds:

```jldoctest
julia> @grammar capnums begin
            :nums  ←  (:num,) | 1 * :nums
            :num   ←  S"123"^1
        end;

julia> match(capnums, "abc123abc123")
PegMatch(["123"])
```

This one also captures the lowercase letters, before converting them to uppercase.

```jldoctest
julia> upper = uppercase;  # A thoroughly unhygienic macro

julia> @grammar uppernums begin
           :nums  ←  (:num,) | :abc * :nums
           :num   ←  S"123"^1
           :abc   ←  R"az"^1 <| upper
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
macro rule(expr::Expr)
    if @capture(expr, (sym_ ← rulebody_)
                    | (sym_ ⟷ rulebody_)
                    | (sym_ <--> rulebody_)
                    | (sym_ ↔︎ rulebody_)
                    | (sym_ <-- rulebody_))
        local r = wrap_rule(expr)
        local name = sym.value
        :($(esc(name)) = $r)
    else
        error("malformed rule in $(expr)")
    end
end

macro rule(expr::Expr, erest::Expr...)
    if @capture(expr, (sym_ ← rulebody_)
                    | (sym_ ⟷ rulebody_)
                    | (sym_ <--> rulebody_)
                    | (sym_ ↔︎ rulebody_)
                    | (sym_ <-- rulebody_))
        error("extra expression in $(sym) :  $(erest...)")
    else
        error("malformed rule: $(expr) $(erest...)")
    end
end
