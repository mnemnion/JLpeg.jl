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
    g1 = PGrammar(:a <= P"123" * :b, :b <= S"abd" * (:a | P"q"))
    @test match(g1, "123a123b123dq") == 14
end
