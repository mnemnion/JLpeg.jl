using JLpeg; import JLpeg as J
using Test
using TestSetExtensions
using InteractiveUtils

@testset ExtendedTestSet "Patterns" begin
    @testset "Sequences" begin
        a = P("abc")
        b = P("def")
        c = P("ghi")
        ad = a * P("d")
        abc = a | b | c
        @test match(a, "abc")[1] == "abc"
        @test match(a, "abd") isa PegFail
        @test match(a, "abcd")[1] == "abc"
        @test match(ad, "abcd")[1] == "abcd"
        @test match(ad, "abcdef")[1] == "abcd"
        @test match(abc, "abc")[1] == "abc"
        @test match(abc, "def")[1] == "def"
        @test match(abc, "defg")[1] == "def"
        @test match(abc, "ghi")[1] == "ghi"
        @test match(abc, "bcd") isa PegFail
        # The empty sequence!
        ptrue = P""
        @test match(ptrue, "")[1] == ""
    end
    @testset "Any" begin
        @test match(P(3), "abcd")[1] == "abc"
        @test match(P(5), "abcd") isa PegFail
        @test match(P(0), "")[1] == ""
    end
    @testset "Sets and Ranges" begin
        bcf = S("bcf")
        atwords = bcf * P("at")
        threeL = R("az") * R("az") * R("az")
        numeven = R("09") * S("02468")
        # Empty Set
        eset = S""
        @test match(bcf, "b")[1] == "b"
        @test match(bcf, "ba")[1] == "b"
        @test match(bcf, "q") isa PegFail
        @test match(atwords, "bat")[1] == "bat"
        @test match(atwords, "cat")[1] == "cat"
        @test match(atwords, "mat") isa PegFail
        @test match(threeL, "foo")[1] == "foo"
        @test match(threeL, "BAR") isa PegFail
        @test match(threeL, "quux")[1] == "quu"
        @test match(threeL, "123") isa PegFail
        @test match(numeven, "12")[1] == "12"
        @test match(numeven, "13") isa PegFail
        @test match(eset, "something") isa PegFail
        @test match(eset, "") isa PegFail
    end
    @testset "Ordered Choice" begin
        afew = S("123") | P("abc") | S("xyz")
        @test match(afew, "1")[1] == "1"
        @test match(afew, "abc")[1] == "abc"
        @test match(afew, "z")[1] == "z"
        aacd = P("aabc") | P("aacd")
        @test match(aacd, "aacd")[1] == "aacd"
        nofail = P("")
        @test match(nofail, "abc")[1] == ""
        @test match(nofail, "")[1] == ""
        nope = P(false)
        @test match(nope, "") isa PegFail
        @test match(nope, "something") isa PegFail
        yep = P(true)
        @test match(yep, "")[1] == ""
        @test match(yep, "abc")[1] == ""
    end
    @testset "Charset Amalgamation" begin
        glom = S("146") | R("AZ") | P("q") | P("*")
        @test match(glom, "1")[1] == "1"
        @test match(glom, "B")[1] == "B"
        @test match(glom, "q")[1] == "q"
        @test match(glom, "*")[1] == "*"
        @test match(glom, "!") isa PegFail
        # Multibyte sets
        greeks = (R"ΑΩ" | R"αω"| P"Ά" | P"ύ" | P" ")^1
        @test match(greeks, "Το Πνεύμα Άγιοπ")[1] == "Το Πνεύμα Άγιοπ"
    end
    @testset "Repetition" begin
        abstar = P("ab")^0
        @test match(abstar, "")[1] == ""
        @test match(abstar, "ab")[1] == "ab"
        @test match(abstar, "abababab")[1] == "abababab"
        @test match(abstar, "abc")[1] == "ab"
        @test match(abstar, "bc")[1] == ""
        # Tests Kleene +
        abplus = P("ab")^1
        @test match(abplus, "") isa PegFail
        @test match(abplus, "ab")[1] == "ab"
        @test match(abplus, "abababab")[1] == "abababab"
        @test match(abplus, "abc")[1] == "ab"
        @test match(abplus, "bc") isa PegFail
        # Tests ?
        opt = P("ab")^-1
        @test match(opt, "")[1] == ""
        @test match(opt, "ab")[1] == "ab"
        @test match(opt, "abababab")[1] == "ab"
        @test match(opt, "abc")[1] == "ab"
        @test match(opt, "bc")[1] == ""
        # Makes sure recursive reps don't hang
        inside = ((P("ab")^0))^0
        @test match(inside, "abab")[1] == "abab"
        @test match(inside, "")[1] == ""
        inside = ((P("ab")^-1))^0
        @test match(inside, "abab")[1] == "abab"
        @test match(inside, "")[1] == ""
        inside = ((P("ab")^-1))^1
        @test match(inside, "abab")[1] == "abab"
        @test match(inside, "")[1] == ""
        inside = ((P("ab")^0))^1
        @test match(inside, "abab")[1] == "abab"
        @test match(inside, "")[1] == ""
        ab4plus = P"ab"^4
        @test match(ab4plus, "ababab") isa PegFail
        @test match(ab4plus, "abababab")[1] == "abababab"
        @test match(ab4plus, "abababababababab")[1] == "abababababababab"
        ab4minus = P"ab"^-4
        @test match(ab4minus, "")[1] == ""
        @test match(ab4minus, "ab")[1] == "ab"
        @test match(ab4minus, "abababab")[1] == "abababab"
        @test match(ab4minus, "abababababab")[1] == "abababab"
    end
    @testset "Predicates" begin
        pand = ~P"abc" * S"abcd"^1
        @test match(pand, "abcd")[1] == "abcd"
        @test match(pand, "abcdddd")[1] == "abcdddd"
        @test match(pand, "abdcc") isa PegFail
        pnot = (!P"a" * R"az")^0 * P"after"
        @test match(pnot, "after")[1] == "after"
        @test match(pnot, "bcxdafter")[1] == "bcxdafter"
        @test match(pnot, "aafter") isa PegFail
        @test match(pnot, "bcadxafter") isa PegFail
        pdiff = (S"abc" - P"a")^1
        @test match(pdiff, "bcbc")[1] == "bcbc"
        @test match(pdiff, "bcbca")[1] == "bcbc"
        @test match(pdiff, "abcbca") isa PegFail
        pset = R"az" - S"bcd"
        @test match(pset, "a")[1] == "a"
        @test match(pset, "c") isa PegFail
        # 🎄 🎄 Merry Christmas! 🎄 🎄
        pno_l = (R"az" - P"l")^1
        @test match(pno_l, "abcdefghijkmnopqrstuvwxyz")[1] == "abcdefghijkmnopqrstuvwxyz"
        @test match(pno_l, "abcdefghijklmnopqrstuvwxyz")[1] == "abcdefghijk"
        @test match(P(-3), "a") isa PegFail
        @test match(P(-3), "aaaa")[1] == ""
        p_one = (!P"1" * P(1))
        @test match(p_one, "2") isa PegMatch
        @test match(p_one, "1") isa PegFail
        @test match(!P"1", "") isa PegMatch
    end
    @testset "Match behind" begin
        behind = P(3) * B("abc") * P"d"
        @test match(behind, "abcd")[1] == "abcd"
        toofar = P(3) * B(4) * P(true)
        @test match(toofar, "abcd").errpos == 4
        atstart = !B(1)
        not_at_start = P(1) * atstart
        @test match(atstart, "12")[1] == ""
        @test match(not_at_start, "12") isa PegFail
        bvar = B(P"a"^1)
        @test_throws PegError match(bvar, "aaaaa")
        bfalse = P"12" * B(P(false))
        @test match(bfalse, "12") isa PegFail
    end
    @testset "Fast Forward" begin
        ff = "" >> P"end"
        @test findfirst(ff, "all the way until the end") == 25
        @test match(ff, "all the way until the end")[1] == "all the way until the end"
        @test occursin(ff, "all the way until the end")
    end
    @testset "Grammars" begin
        g1 = Grammar(:a <-- P"123" * :b, :b <-- S"abd" * (:a | P"q"))
        @test match(g1, "123a123b123dq")[1] == "123a123b123dq"
        # A Real Grammar
        lisp = Grammar(
            :lisp    ←  :_ * P"(" * :body * :_ * P")" * :_,
            :body    ←  :_ * (P(:atom) | :number | :lisp)  * P(:body)^-1,
            :atom    ←  (R"az" | R"AZ")^1,
            :number  ←  R"09"^1,
            :_       ←  S("\t\n ")^0 )
        @test match(lisp, "(12)")[1] == "(12)"
        @test match(lisp, "(12 23 bob)")[1] == "(12 23 bob)"
        @test match(lisp, "(12 23 bob (recursive (recursed)))")[1] == "(12 23 bob (recursive (recursed)))"
        @test match(lisp, "(12 23 bob (recursive recursed))")[1] == "(12 23 bob (recursive recursed))"
        @test match(lisp, "(not ,quote a real lisp)") isa PegFail
    end
    @testset "Macro tests" begin
        upper = uppercase
        @rule :upcase  ←  "abc" |> upper
        @test match(upcase, "abc")[1] == "ABC"
        @test_throws LoadError @eval @rule [:a, :b, :c] :a ← :b
        @test_throws MethodError begin
            try
                @eval @rule :a ← :b "malformed"
            catch e
                if isa(e, LoadError)
                    throw(e.error)
                else
                    throw(e)
                end
            end
        end
        @test_throws "malformed rule" @eval @rule :a != :b
        @rule :nums <--> R"09"^1
        @test match(nums, "1234abc")[:nums] == "1234"
    end
    @testset "Captures" begin
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
        @test match(capsym, "it's in the middle of the string") isa PegFail
        capstr = "" >> C("middle", "the middle")
        @test match(capstr, "it's in the middle of the string")["the middle"] == "middle"
        # Captures: Grouped captures
        groupedcap = Cg(P("" >> C(R"09"^1))^1)
        @test match(groupedcap, "abc123def456")[1] == ["123", "456"]
        namegroup = Cg(P("" >> C(R"09"^1))^1, :numbers)
        @test match(namegroup, "abc123def456")[:numbers] == ["123", "456"]
        capvec = P"" * [P"123"] * [P"abc"]
        @test match(capvec, "123abc").captures == [["123"], ["abc"]]
        poscap = P"123" * Cp() * P"abc"
        @test match(poscap, "123abc").offsets[1] == 4
        func = A("" >> P"func", uppercase)
        @test match(func, "Make my func the Pfunc")[1] == "MAKE MY FUNC"
        funky = "" >> C("func") |> uppercase
        @test match(funky, "make my fun the Pfunc")[1] == "FUNC"
        caprange = (Cr("123") | P(1))^1
        @test match(caprange, "abc123abc123abc").captures == [[4:6], [10:12]]
        capconst = P"123" * Cc(12, 23, 32)
        @test match(capconst, "123")[1] == (12, 23, 32)
        # Test @grammar macro (and certain capture conditions)
        @grammar capnums begin
            :nums  ←  ((:num,) | P(1) * :nums)^1
            :num  ←  S"123"^1
        end
        @test match(capnums, "abc123abc123").captures == ["123", "123"]
        @rule :ccap ← "abc" * Cc(12, "string", :symbol)
        @test match(ccap, "abc")[1] == (12, "string", :symbol)
    end

    @testset "PegMatch Interface" begin
        @rule :pmix <-- ("a",) * ("b", :b) * ("c",)
        mix = match(pmix, "abc")
        @test collect(enumerate(mix)) == [1 => "a", 2 => "b", 3 => "c"]
        @test collect(pairs(mix)) == [1 => "a", :b => "b", 3 => "c"]
        @test collect(mix) == ["a", :b => "b", "c"]
    end
    @testset "Throws" begin
        pthrow = P"123" * (P"abc" | T(:noletter))
        @test match(pthrow, "123abc")[1] == "123abc"
        @test match(pthrow, "1234").label == :noletter
        @test begin
            fail = match(pthrow, "123456")
            "123456"[fail.errpos:end] == "456"
        end
        # Test predicate unwinding
        predthrow = (~P"abc" * T(:nope)) | P"abcd"
        @test match(predthrow, "abcd")[1] == "abcd"
        @test match(predthrow, "abc").label == :default
        notalphanum = (R"az" | R"09")^1 * (!P(1) % :notalphanum)
        @test match(notalphanum, "abc123abcz")[1] == "abc123abcz"
        @test match(notalphanum, "abc123abcz!").label == :notalphanum
        @grammar throwrec begin
            :foo  ←  "foo" * (!P(1) % :bar)
            :bar  ←  "bar" % :nobar
        end
        @test match(throwrec, "foobar")[1] == "foobar"
        @test match(throwrec, "fooba").label == :nobar
    end
    @testset "`re` dialect" begin
        @test match(re, "'string'")[:string] == "string"
        @test match(re, "[a-z]")[:range] == ["a", "z"]
        @test match(re, "sym")[1] == "sym"
        @test match(re, "a <- b c*")[:definition] == ["a", " b c*"]
    end
    @testset "MultiSet refactor tests" begin
        emojiascii = (S"😀😆😂🥲" | S"abcd")^1 * !P(1)
        @test match(emojiascii, "😀a😆bbb😂🥲ccc😀") isa PegMatch
        allrange = (R"az" | R"αω" | R"ሀሏ" | R"👆👏")^1 * !P(1)
        @test match(allrange, "abθqηζzሆሊηሊt👊ሊ👋η👎z") isa PegMatch
        not123 = compile!(!S"123")
        @test match(not123, "4") isa PegMatch
        @test match(not123, "3") isa PegFail
        @test match(not123, "🤡") isa PegMatch
        sphinx0 = (R"az" | " ")^0
        @test match(sphinx0, "sphinx of black quartz judge my vow").last == 35
    end

    @testset "Type tests" begin
        for I in subtypes(J.Instruction)
            if hasfield(I, :vec)
                @test I <: J.IVectored
                @test length(methods(iterate, (I, Integer))) > 0
            end
        end
    end
    @testset "Code tests" begin
        # Tests for code optimization purposes go here
        setinst = compile!(S"123").code[1]
        @test isbits(setinst)
        allrange = compile!((R"az" | R"αω" | R"ሀሏ" | R"👆👏")^1 * !P(1))
        @test repr(allrange.code) == "01: ILeadSet (18) {a-z}\n02: ILeadMulti (7) {0f,10,22,31}\n03: IByte cf (14)\n04: IByte f0 (8)\n05: IByte ce (15)\n06: IByte e1 (10)\n07: IFail\n08: IByte 9f (12)\n09: IFail\n10: IByte 88 (16)\n11: IFail\n12: IByte 91 (17)\n13: IFail\n14: IMultiVec (18) {01-0a}\n15: IMultiVec (18) {32-40}\n16: IMultiVec (18) {01-10}\n17: IMultiVec (18) {07-10}\n18: IChoice (37)\n19: ILeadSet (36) {a-z}\n20: ILeadMulti (25) {0f,10,22,31}\n21: IByte cf (32)\n22: IByte f0 (26)\n23: IByte ce (33)\n24: IByte e1 (28)\n25: IFail\n26: IByte 9f (30)\n27: IFail\n28: IByte 88 (34)\n29: IFail\n30: IByte 91 (35)\n31: IFail\n32: IMultiVec (36) {01-0a}\n33: IMultiVec (36) {32-40}\n34: IMultiVec (36) {01-10}\n35: IMultiVec (36) {07-10}\n36: IPartialCommit (19)\n37: IPredChoice (40)\n38: IAny 1\n39: IFailTwice\n40: IEnd"
    end
end
