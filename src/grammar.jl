# Grammar macro module
module GrammarMacros

export @grammar, @rule, @construle, @constgrammar

using ..JLpeg
import ..JLpeg: ε, ∅
import ..JLpeg.Combinators: *, -, %, |, ^, ~, !, >>, >:, inv
import MacroTools: @capture, postwalk, prewalk, isexpr

const 🔠 = P  # Won't interfere with user uses of P

const ops = Set([:*, :|, :^, :~, :¬, :!, :>>, :<|, :%,])
const JPublic = Set(names(JLpeg)) ∪ ops ∪ [:ε, :∅]

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
            elseif @capture(x, (@P_str(🔠(val_))))
                :(@P_str($val))
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

function wrap_rule_body(body::Any)
    return :($body)
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

function grammar_builder(name, expr)
    @capture(expr, begin
        rules__
    end)
    local rs = [wrap_rule(rule) for rule ∈ rules]
    :($(esc(name)) = Grammar($(rs...)))
end

function rule_builder(expr::Expr)
    if @capture(expr, (sym_ ← rulebody_)
                    | (sym_ ⟷ rulebody_)
                    | (sym_ <--> rulebody_)
                    | (sym_ ↔︎ rulebody_)
                    | (sym_ <-- rulebody_))
        local r = wrap_rule(expr)
        if sym isa QuoteNode
            local name = sym.value
            return :($(esc(name)) = $r)
        else
            error("Illegal rule name (must be :symbol): $sym")
        end
    else
        error("malformed rule in $(expr)")
    end
end

"""
    @grammar(name, rules)

Syntax sugar for defining a set of rules as a single grammar. Expects a block
`rules`, each of which is a rule-pair as can be created with `←`, or `<--`.
`"string"` will be interpolated as `P("string")`, `:symbol` as `P(:symbol)`, and an
integer `n` as `P(n)`.

Any variable name exported by JLpeg will refer to the same value as the export, while
any other variable is escaped, and will have the meaning it has in the scope where
`@grammar` is called.

## Example use

This simple grammar captures the first string of numbers it finds:

```jldoctest
julia> @grammar capnums begin
            :nums  ←  (:num,) | 1 * :nums
            :num   ←  R"09"^1
        end;

julia> match(capnums, "abc123abc123")
PegMatch(["123"])
```

This one also captures the lowercase letters, converting them to uppercase.

```jldoctest
julia> upper = uppercase;  # A thoroughly unhygienic macro

julia> @grammar uppernums begin
           :nums  ←  (:num,) | :abc * :nums
           :num   ←  R"09"^1
           :abc   ←  R"az"^1 <| upper
       end;

julia> match(uppernums, "abc123abc123")
PegMatch(["ABC", "123"])
```

More extensive examples may be found in the documentation.
"""
macro grammar(name, expr)
    grammar_builder(name, expr)
end

"""
    @constgrammar name, rules

Identical to [`@grammar`](@ref), but assigns the result to a
constant.  This is only valid in global scope.
"""
macro constgrammar(name, expr)
    return Expr(:const, grammar_builder(name, expr))
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

In terms of scope and variable escaping, `@rule` functions identically to `@grammar`.
"""
macro rule(expr::Expr)
    return rule_builder(expr)
end

"""
    @construle :name ← pattern...

Identical to [`@rule`](@ref), but assigns the rule to a constant
variable.  Only legal in the top scope.
"""
macro construle(expr::Expr)
    rule_expr = rule_builder(expr)
    return Expr(:const, rule_expr)
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

end # Module