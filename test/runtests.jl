using JLpeg
using Test

@testset "JLpeg.jl" begin
    # Write your tests here.
    a = P("abc")
    b = P("def")
    c = P("ghi")
    ad = a * P("d")
    abc = a | b | c
    @test match(a, "abc") == 4
    @test match(a, "abd") === nothing
    @test match(a, "abcd") == 4
    @test match(ad, "abcd") == 5
    @test match(ad, "abcdef") == 5
    @test match(abc, "abc") == 4
    @test match(abc, "def") == 4
    @test match(abc, "defg") == 4
    @test match(abc, "ghi") == 4
    @test match(abc, "bcd") === nothing
    # Sets and Ranges (same thing different cloth)
    bcf = S("bcf")
    @test match(bcf, "b") == 2
    @test match(bcf, "ba") == 2
    @test match(bcf, "q") === nothing
    atwords = bcf * P("at")
    @test match(atwords, "bat") == 4
    @test match(atwords, "cat") == 4
    @test match(atwords, "mat") === nothing
    threeL = R("az") * R("az") * R("az")
    @test match(threeL, "foo") == 4
    @test match(threeL, "BAR") === nothing
    @test match(threeL, "quux") == 4
    @test match(threeL, "123") === nothing
    numeven = R("09") * S("02468")
    @test match(numeven, "12") == 3
    @test match(numeven, "13") === nothing
    # Empty Set
    eset = S""
    @test match(eset, "something") === nothing
    @test match(eset, "") === nothing
    #Choice
    afew = S("123") | P("abc") | S("xyz")
    @test match(afew, "1") == 2
    @test match(afew, "abc") == 4
    @test match(afew, "z") == 2
    aacd = P("aabc") | P("aacd")
    @test match(aacd, "aacd") == 5
    nofail = P("")
    @test match(nofail, "abc") == 1
    @test match(nofail, "") == 1
    nope = P(false)
    @test match(nope, "") === nothing
    @test match(nope, "something") === nothing
    yep = P(true)
    @test match(yep, "") == 1
    @test match(yep, "abc") == 1
    # TODO start grouping these
    # Tests charset amalgamation
    glom = S("146") | R("AZ") | P("q") | P("*")
    @test match(glom, "1") == 2
    @test match(glom, "B") == 2
    @test match(glom, "q") == 2
    @test match(glom, "*") == 2
    @test match(glom, "!") === nothing
    # Multibyte sets
    greeks = (R"ΑΩ" | R"αω"| P"Ά" | P"ύ" | P" ")^1
    @test match(greeks, "Το Πνεύμα Άγιοπ") == 29
    # Tests Kleene *
    abstar = P("ab")^0
    @test match(abstar, "") == 1
    @test match(abstar, "ab") == 3
    @test match(abstar, "abababab") == 9
    @test match(abstar, "abc") == 3
    @test match(abstar, "bc") == 1
    # Tests Kleene +
    abplus = P("ab")^1
    @test match(abplus, "") === nothing
    @test match(abplus, "ab") == 3
    @test match(abplus, "abababab") == 9
    @test match(abplus, "abc") == 3
    @test match(abplus, "bc") === nothing
    # Tests ?
    opt = P("ab")^-1
    @test match(opt, "") == 1
    @test match(opt, "ab") == 3
    @test match(opt, "abababab") == 3
    @test match(opt, "abc") == 3
    @test match(opt, "bc") == 1
    # Makes sure recursive reps don't hang
    inside = ((P("ab")^0))^0
    @test match(inside, "abab") == 5
    @test match(inside, "") == 1
    inside = ((P("ab")^-1))^0
    @test match(inside, "abab") == 5
    @test match(inside, "") == 1
    inside = ((P("ab")^-1))^1
    @test match(inside, "abab") == 5
    @test match(inside, "") == 1
    inside = ((P("ab")^0))^1
    @test match(inside, "abab") == 5
    @test match(inside, "") == 1
    # Grammars!
    g1 = Grammar(:a <= P"123" * :b, :b <= S"abd" * (:a | P"q"))
    @test match(g1, "123a123b123dq") == 14
    # more of that to come..
    # Predicates
    pand = ~P"abc" * S"abcd"^1
    @test match(pand, "abcd") == 5
    @test match(pand, "abcdddd") == 8
    @test match(pand, "abdcc") === nothing
    pnot = (!P"a" * R"az")^0 * P"after"
    @test match(pnot, "after") == 6
    @test match(pnot, "bcxdafter") == 10
    @test match(pnot, "aafter") === nothing
    @test match(pnot, "bcadxafter") === nothing
    pdiff = (S"abc" - P"a")^1
    @test match(pdiff, "bcbc") == 5
    @test match(pdiff, "bcbca") == 5
    @test match(pdiff, "abcbca") === nothing
    pset = R"az" - S"bcd"
    @test match(pset, "a") == 2
    @test match(pset, "c") === nothing
    # 🎄 🎄 Merry Christmas! 🎄 🎄
    pno_l = (R"az" - P"l")^1
    @test match(pno_l, "abcdefghijkmnopqrstuvwxyz") == 0x1a
    @test match(pno_l, "abcdefghijklmnopqrstuvwxyz") == 0x0c
    @test match(P(-3), "a") === nothing
    @test match(P(-3), "aaaa") == 1
    # Fast Forward
    ff = "" >> P"end"
    @test match(ff, "all the way until the end") == 26
    # A Real Grammar
    lisp = Grammar(
        :lisp    ←  :_ * P"(" * :body * :_ * P")" * :_,
        :body    ←  :_ * (P(:atom) | :number | :lisp)  * P(:body)^-1,
        :atom    ←  (R"az" | R"AZ")^1,
        :number  ←  R"09"^1,
        :_       ←  S("\t\n ")^0 )
    @test match(lisp, "(12)") == 5
    @test match(lisp, "(12 23 bob)") == 12
    @test match(lisp, "(12 23 bob (recursive (recursed)))") == 35
    @test match(lisp, "(12 23 bob (recursive recursed))") == 33
    @test match(lisp, "(not ,quote a real lisp)") === nothing
    # Captures: Simple (SubString) captures
    cap1 = C("123")
    @test match(cap1, "123")[1] == "123"
    @test match(cap1, "abc") === nothing
    @test match(cap1, "123")[1] isa SubString
    cap2 = P"abc" * C("123")
    @test match(cap2, "abc123")[1] == "123"
    cap3 = cap2^1
    @test match(cap3, "abc123abc123abc123") == ("123", "123", "123")
end
