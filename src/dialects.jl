# Dialects for creating PEG patterns and grammars

"""
    re : An Interpretation of LPeg's `re` module

The first dialect, intended, among other things, as a useful
bootstrap of other dialects.
"""
@grammar re begin
    :pattern      ←  :exp * !P(1)
    :exp          ←  :S * (:grammar | :alternative)
    :alternative  ←  ([:seq * :S * ("|" * :S * :alternative)^1, :alt] | :seq * :S)^1
    :seq          ←  [(:prefix * :S)^2, :seq] | :prefix * :S

    :prefix       ←  "&" * :S * :prefix | "!" * :S * :prefix | :suffix
    :suffix       ←  ( :primary * :S * !S"+*?^|<:"
                     | [:primary * :S * ( (S"+*?", :kleene)
                                         | "^" * (( S"+-"^-1,) * (:num,), :rep)
                                         | ("|>" | "<|") * :S * (:name, :action)
                                         | ">:" * :S * (:name, :runtime) * :S)^1, :suffixed])

    :primary      ← ( "(" * [:exp] * ")" | :string | :class | :defined
                      | ["{:" * :exp * ":" * (:name^-1, :groupname) * "}", :groupcapture]
                      # | "=" * :name  # TODO this is mark/check syntax, support somehow
                      | ("{}", :positioncapture)
                      | ["{"  * :exp *  "}", :capture]
                      | (".", :any)
                      | (:name * :S * !(:arrow), :call) )

    :grammar      <-->  :rule^1
    :rule         ←  [(:name, :rulename) * :S * :arrow * [:exp], :rule]
    :class        ←  "[" * "^"^-1 * :item * (!"]" * :item)^0 * "]"
    :item         ←  :defined | :range | P(1)
    :range        ←  [(P(1),) * "-" * (!S"]" * (P(1),)), :range]

    :S            ←  (S"\t\n\v\r " | "#" * (!S"\n" * P(1))^0 * "\n")^0
    :name         ←  (R"AZ" | R"az" | "_") * (R"AZ" | R"az" | "_")^0
    :arrow        ←  "<-" | "←"
    :num          ←  (R"09"^1, :num)
    # TODO disallow \n and add a thrown label in :string
    :string       ←  "\"" * ((!S"\"" * P(1))^0, :string) * "\"" | "'" * ((!S"'" * P(1))^0, :string) * "'"
    :defined      ←  "%" * (:name, :defined)
end
