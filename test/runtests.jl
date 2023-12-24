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
end
