using JLpeg; import JLpeg as J
import JLpeg.Combinators: *, -, %, |, ^, ~, !, >>, >:, inv
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
        # Tests a^[n:m]
        afew = P"a"^[2:4]
        @test match(afew, "a") isa PegFail
        @test match(afew, "aa")[1] == "aa"
        @test match(afew, "aaaa")[1] == "aaaa"
        @test match(afew, "aaaaaaa")[1] == "aaaa"
        four = P"a"^[4:4]
        @test match(four, "aaaa")[1] == "aaaa"
        @test match(four, "aaa") isa PegFail
        @test match(four, "aaaaa")[1] == "aaaa"
        ezthree = P"a"^[3]
        @test match(ezthree, "aaaa")[1] == "aaa"
        @test match(ezthree, "aaa")[1] == "aaa"
        @test match(ezthree, "aa") isa PegFail
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
        @rule :upcase  ←  "abc" <| upper
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
        @test match(nums, "1234abc")[:nums] == ["1234"]
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
        funky = "" >> C("func") <| uppercase
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
        @rule :capABC <--> ((S"ABC"^1,) | R"az"^1)^1
        @test match(capABC, "abcBCAzyzCCCd")[:capABC] == ["BCA", "CCC"]
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
        @rule :selfcall ← "123" * :selfcall
        @test_throws PegError match(selfcall, "1234")
        @test_throws "extra expression" @eval @rule :baddate ← (R"09"^1, :year) * "-" * (R"09"^1,) * "-" (R"09"^1, :day)
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
        @test repr("text/plain", allrange.code) == "01: ILeadSet (18) {a-z}\n02: ILeadMulti (7) {0f,10,22,31}\n03: IByte cf (14)\n04: IByte f0 (8)\n05: IByte ce (15)\n06: IByte e1 (10)\n07: IFail\n08: IByte 9f (12)\n09: IFail\n10: IByte 88 (16)\n11: IFail\n12: IByte 91 (17)\n13: IFail\n14: IMultiVec (18) {01-0a}\n15: IMultiVec (18) {32-40}\n16: IMultiVec (18) {01-10}\n17: IMultiVec (18) {07-10}\n18: IChoice (37)\n19: ILeadSet (36) {a-z}\n20: ILeadMulti (25) {0f,10,22,31}\n21: IByte cf (32)\n22: IByte f0 (26)\n23: IByte ce (33)\n24: IByte e1 (28)\n25: IFail\n26: IByte 9f (30)\n27: IFail\n28: IByte 88 (34)\n29: IFail\n30: IByte 91 (35)\n31: IFail\n32: IMultiVec (36) {01-0a}\n33: IMultiVec (36) {32-40}\n34: IMultiVec (36) {01-10}\n35: IMultiVec (36) {07-10}\n36: IPartialCommit (19)\n37: IPredChoice (40)\n38: IAny 1\n39: IFailTwice\n40: IEnd"
        compile!(re)
        @test repr("text/plain",re.code) == "001: ICall (3)\n002: IEnd\n003: ICall (8)\n004: IPredChoice (7)\n005: IAny 1\n006: IFailTwice\n007: IReturn\n008: ICall (14)\n009: IChoice (12)\n010: ICall (27)\n011: ICommit (13)\n012: IJump (52)\n013: IReturn\n014: IChoice (26)\n015: IChoice (18)\n016: ISet (17) {\\t-\\xb,\\r,\" \"}\n017: ICommit (25)\n018: IChar '-'\n019: IChar '-'\n020: IChoice (24)\n021: INotChar '\\n'\n022: IAny 1\n023: IPartialCommit (21)\n024: IChar '\\n'\n025: IPartialCommit (15)\n026: IReturn\n027: ICall (32)\n028: IChoice (31)\n029: ICall (32)\n030: IPartialCommit (29)\n031: IReturn\n032: Cgroup open #3\n033: Csimple open #1\n034: ICall (43)\n035: Csimple close #1\n036: ICall (14)\n037: ICall (46)\n038: Csimple open #2\n039: ICall (8)\n040: Csimple close #2\n041: Cgroup close #3\n042: IReturn\n043: ISet (44) {A-Z,_,a-z}\n044: ILeadSet (44) {A-Z,_,a-z}\n045: IReturn\n046: IChoice (50)\n047: IChar '<'\n048: IChar '-'\n049: ICommit (51)\n050: IChar '←'\n051: IReturn\n052: ICall (59)\n053: IChoice (58)\n054: IChar '/'\n055: ICall (14)\n056: ICall (59)\n057: IPartialCommit (54)\n058: IReturn\n059: IChoice (62)\n060: ICall (63)\n061: IPartialCommit (60)\n062: IReturn\n063: IChoice (68)\n064: IChar '&'\n065: ICall (14)\n066: ICall (63)\n067: ICommit (74)\n068: IChoice (73)\n069: IChar '!'\n070: ICall (14)\n071: ICall (63)\n072: ICommit (74)\n073: IJump (75)\n074: IReturn\n075: ICall (112)\n076: ICall (14)\n077: IChoice (111)\n078: IChoice (81)\n079: ISet (80) {*,+,?}\n080: ICommit (109)\n081: IChoice (86)\n082: IChar '^'\n083: ICommit (84)\n084: ICall (233)\n085: ICommit (109)\n086: IChoice (99)\n087: IChar '-'\n088: IChar '>'\n089: ICall (14)\n090: IChoice (93)\n091: ICall (179)\n092: ICommit (98)\n093: IChoice (97)\n094: IChar '{'\n095: IChar '}'\n096: ICommit (98)\n097: ICall (43)\n098: ICommit (109)\n099: IChoice (105)\n100: IChar '>'\n101: IChar '>'\n102: ICall (14)\n103: ICall (43)\n104: ICommit (109)\n105: IChar '='\n106: IChar '>'\n107: ICall (14)\n108: ICall (43)\n109: ICall (14)\n110: IPartialCommit (78)\n111: IReturn\n112: IChoice (117)\n113: IChar '('\n114: ICall (8)\n115: IChar ')'\n116: ICommit (178)\n117: IChoice (120)\n118: ICall (179)\n119: ICommit (178)\n120: IChoice (123)\n121: ICall (198)\n122: ICommit (178)\n123: IChoice (126)\n124: ICall (217)\n125: ICommit (178)\n126: IChoice (138)\n127: IChar '{'\n128: IChar ':'\n129: IChar '}'\n130: IChoice (134)\n131: ICall (43)\n132: IChar ':'\n133: ICommit (134)\n134: ICall (8)\n135: IChar ':'\n136: IChar '}'\n137: ICommit (178)\n138: IChoice (142)\n139: IChar '='\n140: ICall (43)\n141: ICommit (178)\n142: IChoice (146)\n143: IChar '{'\n144: IChar '}'\n145: ICommit (178)\n146: IChoice (153)\n147: IChar '{'\n148: IChar '~'\n149: ICall (8)\n150: IChar '~'\n151: IChar '}'\n152: ICommit (178)\n153: IChoice (160)\n154: IChar '{'\n155: IChar '|'\n156: ICall (8)\n157: IChar '|'\n158: IChar '}'\n159: ICommit (178)\n160: IChoice (165)\n161: IChar '{'\n162: ICall (8)\n163: IChar '}'\n164: ICommit (178)\n165: IChoice (168)\n166: IChar '.'\n167: ICommit (178)\n168: IChoice (175)\n169: ICall (43)\n170: ICall (14)\n171: IPredChoice (174)\n172: ICall (46)\n173: IFailTwice\n174: ICommit (178)\n175: IChar '<'\n176: ICall (43)\n177: IChar '>'\n178: IReturn\n179: IChoice (189)\n180: IChar '\"'\n181: Csymbol open #8\n182: IChoice (186)\n183: INotChar '\"'\n184: IAny 1\n185: IPartialCommit (183)\n186: Csymbol close #8\n187: IChar '\"'\n188: ICommit (197)\n189: IChar '\\''\n190: Csymbol open #9\n191: IChoice (195)\n192: INotChar '\\''\n193: IAny 1\n194: IPartialCommit (192)\n195: Csymbol close #9\n196: IChar '\\''\n197: IReturn\n198: IChar '['\n199: IChoice (202)\n200: IChar '^'\n201: ICommit (202)\n202: ICall (209)\n203: IChoice (207)\n204: INotChar ']'\n205: ICall (209)\n206: IPartialCommit (204)\n207: IChar ']'\n208: IReturn\n209: IChoice (212)\n210: ICall (217)\n211: ICommit (216)\n212: IChoice (215)\n213: ICall (222)\n214: ICommit (216)\n215: IAny 1\n216: IReturn\n217: IChar '%'\n218: Csymbol open #10\n219: ICall (43)\n220: Csymbol close #10\n221: IReturn\n222: Cgroup open #6\n223: Csimple open #5\n224: IAny 1\n225: Csimple close #5\n226: IChar '-'\n227: INotChar ']'\n228: Csimple open #4\n229: IAny 1\n230: Csimple close #4\n231: Cgroup close #6\n232: IReturn\n233: Csymbol open #7\n234: ISet (235) {0-9}\n235: ILeadSet (235) {0-9}\n236: Csymbol close #7\n237: IReturn\n238: IEnd"
    end
end
