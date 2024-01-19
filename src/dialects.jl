# Dialects for creating PEG patterns and grammars

"""
    re : An Interpretation of LPeg's `re` module

The first dialect, intended, among other things, as a useful
bootstrap of other dialects.
"""
@grammar re begin
    :re           ←  (:grammar | :pattern) * !1
    :pattern      ←  :expr
    :grammar     ⟷  :rule^1
    :rule         ←  [(:name, :rulename) * :S * :arrow * :S * [:expr] * :S, :rule]
    :expr         ←  :alt | :seq | :element
    :alt          ←  [(:seq | :element) * :S * (S"|/" * :S * (:seq | :element))^1, :alt]
    :seq          ←  [:element * (:S * :element * :S)^1, :seq]

    :element      ←  ["&" * :S * :element, :and] | ["!" * :S * :element, :not] | :action | :suffix
    :suffix       ←  (([:primary * :S * ( (S"+*?", :kleene)
                                         | "^" * ((S"+-"^-1,) * (:number,), :rep)
                                         | "^" * ["[" * (:number, :start) * ":" * (:number, :stop) * "]", :reprange] * :S), :suffixed])
                     | :primary * :S)

    :action        ←  :suffix * (("|>" | "<|") * :S * (:name, :action) |
                                ">:" * :S * (:name, :runtime))


    :primary       ←  ( "(" * [:expr] * ")" | :string | :class | :defined
                      | ["{" * :expr * ":" * (:name^-1, :groupname) * "}", :groupcapture]
                      # | "=" * :name  # TODO this is mark/check syntax, support somehow
                      | ("()", :positioncapture)
                      | ["("  * :expr * "," * :S * (((":" * :name, :capname) % :badcapture) * :S)^-1  * ")", :capture]
                      | (".", :any)
                      | (:name, :call) * :S * !(:arrow) )

    :class        ←  "[" * "^" * (:item * (!"]" * :item)^0, :notset) * "]" |
                     "[" * (:item * (!"]" * :item)^0, :set) * "]"
    :item         ←  :defined | :range | P(1)
    :range        ←  [(P(1),) * "-" * (!S"]" * (P(1),)), :range]

    :S            ←  ((S"\t\n\v\r ")^1 | "#" * (!S"\n" * P(1))^0 * "\n")^0
    :name         ←  (R"AZ" | R"az" | "_") * (R"AZ" | R"az" | "_")^0
    :arrow        ←  "<-" | "←"
    :number       ←  R"09"^1
    # TODO disallow \n and add a thrown label in :string
    :string       ←  "\"" * ((!S"\"" * P(1))^0, :string) * "\"" | "'" * ((!S"'" * P(1))^0, :string) * "'"
    :defined      ←  "%" * (:name, :defined)
end
