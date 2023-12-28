using JLpeg
using Test

@testset "JLpeg.jl" begin
    # Write your tests here.
    a = P("abc")
    b = P("def")
    c = P("ghi")
    ad = a * P("d")
    abc = a | b | c
    @test findfirst(a, "abc") == 3
    @test findfirst(a, "abd") === nothing
    @test findfirst(a, "abcd") == 3
    @test findfirst(ad, "abcd") == 4
    @test findfirst(ad, "abcdef") == 4
    @test findfirst(abc, "abc") == 3
    @test findfirst(abc, "def") == 3
    @test findfirst(abc, "defg") == 3
    @test findfirst(abc, "ghi") == 3
    @test findfirst(abc, "bcd") === nothing
    # Sets and Ranges (same thing different cloth)
    bcf = S("bcf")
    @test findfirst(bcf, "b") == 1
    @test findfirst(bcf, "ba") == 1
    @test findfirst(bcf, "q") === nothing
    atwords = bcf * P("at")
    @test findfirst(atwords, "bat") == 3
    @test findfirst(atwords, "cat") == 3
    @test findfirst(atwords, "mat") === nothing
    threeL = R("az") * R("az") * R("az")
    @test findfirst(threeL, "foo") == 3
    @test findfirst(threeL, "BAR") === nothing
    @test findfirst(threeL, "quux") == 3
    @test findfirst(threeL, "123") === nothing
    numeven = R("09") * S("02468")
    @test findfirst(numeven, "12") == 2
    @test findfirst(numeven, "13") === nothing
    # Empty Set
    eset = S""
    @test findfirst(eset, "something") === nothing
    @test findfirst(eset, "") === nothing
    #Choice
    afew = S("123") | P("abc") | S("xyz")
    @test findfirst(afew, "1") == 1
    @test findfirst(afew, "abc") == 3
    @test findfirst(afew, "z") == 1
    aacd = P("aabc") | P("aacd")
    @test findfirst(aacd, "aacd") == 4
    nofail = P("")
    @test findfirst(nofail, "abc") == 0
    @test findfirst(nofail, "") == 0
    nope = P(false)
    @test findfirst(nope, "") === nothing
    @test findfirst(nope, "something") === nothing
    yep = P(true)
    @test findfirst(yep, "") == 0
    @test findfirst(yep, "abc") == 0
    # TODO start grouping these
    # Tests charset amalgamation
    glom = S("146") | R("AZ") | P("q") | P("*")
    @test findfirst(glom, "1") == 1
    @test findfirst(glom, "B") == 1
    @test findfirst(glom, "q") == 1
    @test findfirst(glom, "*") == 1
    @test findfirst(glom, "!") === nothing
    # Multibyte sets
    greeks = (R"ΑΩ" | R"αω"| P"Ά" | P"ύ" | P" ")^1
    @test findfirst(greeks, "Το Πνεύμα Άγιοπ") == 28
    # Tests Kleene *
    abstar = P("ab")^0
    @test findfirst(abstar, "") == 0
    @test findfirst(abstar, "ab") == 2
    @test findfirst(abstar, "abababab") == 8
    @test findfirst(abstar, "abc") == 2
    @test findfirst(abstar, "bc") == 0
    # Tests Kleene +
    abplus = P("ab")^1
    @test findfirst(abplus, "") === nothing
    @test findfirst(abplus, "ab") == 2
    @test findfirst(abplus, "abababab") == 8
    @test findfirst(abplus, "abc") == 2
    @test findfirst(abplus, "bc") === nothing
    # Tests ?
    opt = P("ab")^-1
    @test findfirst(opt, "") == 0
    @test findfirst(opt, "ab") == 2
    @test findfirst(opt, "abababab") == 2
    @test findfirst(opt, "abc") == 2
    @test findfirst(opt, "bc") == 0
    # Makes sure recursive reps don't hang
    inside = ((P("ab")^0))^0
    @test findfirst(inside, "abab") == 4
    @test findfirst(inside, "") == 0
    inside = ((P("ab")^-1))^0
    @test findfirst(inside, "abab") == 4
    @test findfirst(inside, "") == 0
    inside = ((P("ab")^-1))^1
    @test findfirst(inside, "abab") == 4
    @test findfirst(inside, "") == 0
    inside = ((P("ab")^0))^1
    @test findfirst(inside, "abab") == 4
    @test findfirst(inside, "") == 0
    # Grammars!
    g1 = Grammar(:a <= P"123" * :b, :b <= S"abd" * (:a | P"q"))
    @test findfirst(g1, "123a123b123dq") == 13
    # more of that to come..
    # Predicates
    pand = ~P"abc" * S"abcd"^1
    @test findfirst(pand, "abcd") == 4
    @test findfirst(pand, "abcdddd") == 7
    @test findfirst(pand, "abdcc") === nothing
    pnot = (!P"a" * R"az")^0 * P"after"
    @test findfirst(pnot, "after") == 5
    @test findfirst(pnot, "bcxdafter") == 9
    @test findfirst(pnot, "aafter") === nothing
    @test findfirst(pnot, "bcadxafter") === nothing
    pdiff = (S"abc" - P"a")^1
    @test findfirst(pdiff, "bcbc") == 4
    @test findfirst(pdiff, "bcbca") == 4
    @test findfirst(pdiff, "abcbca") === nothing
    pset = R"az" - S"bcd"
    @test findfirst(pset, "a") == 1
    @test findfirst(pset, "c") === nothing
    # 🎄 🎄 Merry Christmas! 🎄 🎄
    pno_l = (R"az" - P"l")^1
    @test findfirst(pno_l, "abcdefghijkmnopqrstuvwxyz") == 25
    @test findfirst(pno_l, "abcdefghijklmnopqrstuvwxyz") == 11
    @test findfirst(P(-3), "a") === nothing
    @test findfirst(P(-3), "aaaa") == 0
    # Fast Forward
    ff = "" >> P"end"
    @test findfirst(ff, "all the way until the end") == 25
    # A Real Grammar
    lisp = Grammar(
        :lisp    ←  :_ * P"(" * :body * :_ * P")" * :_,
        :body    ←  :_ * (P(:atom) | :number | :lisp)  * P(:body)^-1,
        :atom    ←  (R"az" | R"AZ")^1,
        :number  ←  R"09"^1,
        :_       ←  S("\t\n ")^0 )
    @test findfirst(lisp, "(12)") == 4
    @test findfirst(lisp, "(12 23 bob)") == 11
    @test findfirst(lisp, "(12 23 bob (recursive (recursed)))") == 34
    @test findfirst(lisp, "(12 23 bob (recursive recursed))") == 32
    @test findfirst(lisp, "(not ,quote a real lisp)") === nothing
    # Captures: Simple (SubString) captures
    cap1 = C("123")
    @test match(cap1, "123")[1] == "123"
    @test match(cap1, "123")[1] isa SubString
    cap2 = P"abc" * C("123")
    @test match(cap2, "abc123")[1] == "123"
    cap3 = cap2^1
    @test match(cap3, "abc123abc123abc123").captures == ["123", "123", "123"]
    capff = cap = P"" >> (P"56",)
    @test match(capff, "12345678").captures == ["56"]
    capsym = "" >> C("end", :the_end)
    @test match(capsym, "it's at the end")[:the_end] == "end"
end
