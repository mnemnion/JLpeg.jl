# Grammar macro module

using MacroTools

const postwalk, prewalk = MacroTools.postwalk, MacroTools.prewalk

const 🔠 = P  # Won't interfere with user uses of P

const ops = [:*, :|, :^, :~, :¬, :!, :>>, :|>, :>, :(=>), :%, :./,]

function wrap_rule(expr::Expr, rules::Expr)::Expr
    escapes = rules.args
    function for_x(x)
        if x isa String || x isa QuoteNode || x isa Char
            return :(🔠($x))
        elseif x isa Symbol && x ∈ escapes
            return esc(x)
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
    @capture(expr, (sym_ ← rulebody_) | (sym_ <= rulebody_)) || error("malformed rule in $(expr)")
    :($sym ← $(postwalk(for_x, rulebody)))
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
        end
JLpeg.PGrammar(val→[JLpeg.PRule,JLpeg.PRule], IVec[]))

julia> match(capnums, "abc123abc123")
PegMatch(["123"])
```

This one also captures the lowercase letters, converting them to uppercase. Because
`uppercase` is in the list of variables, it is interpreted locally; otherwise the
`:abc` rule would use `JLpeg.uppercase`, which is seldom what you would want.

```jldoctest
julia> upper = uppercase  # Creating a synonym with no definition within JLpeg
uppercase (generic function with 2 methods)

julia> @grammar uppernums (upper,) begin
           :nums  ←  (:num,) | :abc * :nums
           :num   ←  S"123"^1
           :abc   ←  R"az"^1 |> upper
       end
JLpeg.PGrammar(val→[JLpeg.PRule,JLpeg.PRule,JLpeg.PRule], IVec[]))

julia> match(uppernums, "abc123abc123")
PegMatch(["ABC", "123"])
```

More extensive examples may be found in the documentation.
"""
macro grammar(name, expr)
    @capture(expr, begin rules__ end)
    local rs = [wrap_rule(rule, :(())) for rule ∈ rules]
    :($(esc(name)) = Grammar($(rs...)))
end

macro grammar(name, syms, expr)
    if !(syms.head == :tuple)
        throw(ArgumentError("expression b in `@grammar a (b,) begin ... end` must be a tuple"))
    elseif !(all(s -> s isa Symbol, syms.args))
        throw(ArgumentError("all elements of @grammar tuple must be symbols"))
    end
    @capture(expr, begin rules__ end)
    local rs = [wrap_rule(rule, syms) for rule ∈ rules]
    :($(esc(name)) = Grammar($(rs...)))
end

"""
    @grammar! name begin
        # rules...
    end

Compile-time version of `@grammar`.  This macro creates and compiles the `Grammar`
during (Julia) compiling, setting `name` directly to the resulting grammar.

Doesn't have a variable-escaping variant, because those variables don't exist at
the time the grammar is compiled to bytecode.
"""
macro grammar!(name, expr)
    @capture(expr, begin rules__ end)
    local rs = [wrap_rule(rule, :(())) for rule ∈ rules]
    local gexpr = :(Grammar($(rs...)))
    local g_now = prepare!(eval(gexpr))
    :($(esc(name)) = :($$g_now))
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
    @capture(expr, (sym_ ← rulebody_) | (sym_ <= rulebody_)) || error("malformed rule in $(expr)")
    local r = wrap_rule(expr, :(()))
    local name = sym.value
    :($(esc(name)) = $r)
end

"""
    @rule (vars,) :name  ←  pattern...

Variable-escaping version of @rule.

```jldoctest
julia> @rule (uppercase,) :upfoobar  ←  ("foo" | "bar") |> uppercase;

julia> match(upfoobar, "foo")
PegMatch(["FOO"])
```
"""
macro rule(syms, expr)
    if !(syms isa Expr) || !(syms.head == :tuple)
        throw(ArgumentError("in `@rule (a,) b`, a must be a tuple, got $(typeof(syms))"))
    elseif !(all(s -> s isa Symbol, syms.args))
        throw(ArgumentError("all elements of `@rule (a,) b` tuple must be Symbols"))
    end
    @capture(expr, (sym_ ← rulebody_) | (sym_ <= rulebody_)) || error("malformed rule in $(expr)")
    local r = wrap_rule(expr, syms)
    local name = sym.value
    :($(esc(name)) = $r)
end


"""
    @rule! :name ← patt...

Compile-time version of `@rule`, assigns `name` directly to the fully-compiled
`Rule(:name, patt)`.  Doesn't have a symbol-escaping form, because the values of
those symbols aren't known at the time the rule is compiled.
"""
macro rule!(expr)
    @capture(expr, (sym_ ← rulebody_) | (sym_ <= rulebody_)) || error("malformed rule in $(expr)")
    local r = wrap_rule(expr, :(()))
    local name = sym.value
    local r_now = prepare!(eval(r))
    :($(esc(name)) = :($$r_now))
end