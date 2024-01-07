# Dialects for creating PEG patterns and grammars

"""
    re : An Interpretation of LPeg's `re` module

The first dialect, intended, among other things, as a useful
bootstrap of other dialects.
"""
@grammar re begin
    :pattern      ←  :exp * !P(1)
    :exp          ←  :S * (:grammar | :alternative)
    :alternative  ←  :seq * ("/" * :S * :seq)^0
    :seq          ←  :prefix^0

    :prefix       ←  "&" * :S * :prefix | "!" * :S * :prefix | :suffix
    :suffix       ←  :primary * :S * ((S"+*?"
                                 | "^" * S"+-"^-1 * :num
                                 | "->" * :S * (:string | "{}" | :name)
                                 | ">>" * :S * :name
                                 | "=>" * :S * :name) * :S)^0

    :primary      ← ( "(" * :exp * ")" | :string | :class | :defined
                      |  "{:}" * (:name * ":")^-1 * :exp * ":}"
                      | "=" * :name
                      | "{}"
                      | "{~" * :exp * "~}"
                      | "{|" * :exp * "|}"
                      | "{"  * :exp *  "}"
                      | "."
                      | :name * :S * !(:arrow)
                      | "<" * :name * ">")

    :grammar      ←  :definition^1
    :definition   ←  [(:name,) * :S * :arrow * (:exp,), :definition]
    :class        ←  "[" * "^"^-1 * :item * (!"]" * :item)^0 * "]"
    :item         ←  :defined | :range | P(1)
    :range        ←  [(P(1),) * "-" * (!S"]" * (P(1),)), :range]

    :S            ←  (S"\t\n\v\r " | "--" * (!S"\n" * P(1))^0 * "\n")^0
    :name         ←  (R"AZ" | R"az" | "_") * (R"AZ" | R"az" | "_")^0
    :arrow        ←  "<-" | "←"
    :num          ←  (R"09"^1, :num)
    :string       ←  "\"" * ((!S"\"" * P(1))^0, :string) * "\"" | "'" * ((!S"'" * P(1))^0, :string) * "'"
    :defined      ← "%" * (:name, :defined)
end
