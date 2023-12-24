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
end
