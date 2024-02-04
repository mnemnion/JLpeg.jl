# Dialects for creating PEG patterns and grammars

"""
    re : An Interpretation of LPeg's `re` module

The first dialect, intended, among other things, as a useful
bootstrap of other dialects.
"""
@grammar re begin
    :re           ←  (:grammar | :pattern) * !1
    :pattern     ⟷  :expr
    :grammar     ⟷  :rule^1
    :rule        ⟷  (:name, :rulename) * :S * :arrow * :S * [:expr] * :S
    :expr         ←  :alt | :seq | :element
    :alt         ⟷  (:seq | :element) * :S * (S"|/" * :S * (:seq | :element))^1
    :seq         ⟷  :element * (:S * :element * :S)^1

    :element      ←  ["&" * :S * :element, :and] | ["!" * :S * :element, :not] | :action | :suffix
    :suffix       ←  (([:primary * :S * ( (S"+*?", :kleene)
                                         | "^" * ((S"+-"^-1,) * (:number,), :rep)
                                         | "^" * ["[" * (:number, :start) * ":" * (:number, :stop) * "]", :reprange]) * :S])
                     | :primary * :S)

    :action       ⟷  :suffix * ( ("|>" | "<|") * :S * (:name, :action)
                                | "|?" * :S * (:name, :test)
                                | "%" * :S * (:name, :throw))


    :primary       ←  ( "(" * [:expr] * ")" | :string | :class | :range
                      | ["[" * :expr * ","^-1 * :S * (":" * (:name, :groupname))^-1 * :S * "]", :groupcapture]
                      # | "=" * :name  # TODO this is mark/check syntax, support somehow
                      | ("()", :positioncapture)
                      | ["("  * :expr * ((("," * :S)^-1 * ((":" * :name, :capname) % :badcapture) * :S) | :S * "," * :S )^-1  * ")", :capture]
                      | (".", :any)
                      | (:name, :call) * :S * !(:arrow) )

    :class        ←  ("{" * "^" * ((!"}" * 1)^0, :notset) * "}"
                     | "{" * ":" * (:name, :defined) * :S * "}"
                     | "{" * ((!"}" * 1)^0, :set) * "}")

    :range        ←  [ "<" * (P(1),) * "-"^-1 * (P(1),) * ">", :range]

    :S            ←  ((S"\t\n\v\r ")^1 | "#" * (!S"\n" * P(1))^0 * "\n")^0
    :name         ←  (R"AZ" | R"az" | "_") * (R"AZ" | R"az" | "_")^1
    :arrow        ←  "<-" | "←"
    :number       ←  R"09"^1
    # TODO disallow \n and add a thrown label in :string
    :string       ←  "\"" * ((!S"\"" * P(1))^0, :string) * "\"" | "'" * ((!S"'" * P(1))^0, :string) * "'"
end
