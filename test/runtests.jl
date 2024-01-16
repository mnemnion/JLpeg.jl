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
        setopt = S"+-"^-1
        @test match(setopt, "")[1] == ""
        @test match(setopt, "+")[1] == "+"
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
        @test match(nums, "1234abc")[:nums].captures == ["1234"]
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
        @test match(namegroup, "abc123def456")[:numbers].captures == ["123", "456"]
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
        @test match(capABC, "abcBCAzyzCCCd")[:capABC].captures == ["BCA", "CCC"]
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
        @test match(re, "[a-z]")[:range].captures == ["a", "z"]
        @test match(re, "sym")[:call] == "sym"
        @test match(re, "a <- b c*")[:grammar] isa PegMatch
        @test_skip match(re, "a <- c^1") isa PegMatch
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
        @test_skip repr("text/plain",re.code) == "001: ICall (3)\n002: IEnd\n003: ICall (8)\n004: IPredChoice (7)\n005: IAny 1\n006: IFailTwice\n007: IReturn\n008: ICall (14)\n009: IChoice (12)\n010: ICall (26)\n011: ICommit (13)\n012: IJump (53)\n013: IReturn\n014: IChoice (25)\n015: IChoice (18)\n016: ISet (17) {\\t-\\xb,\\r,\" \"}\n017: ICommit (24)\n018: IChar '#'\n019: IChoice (23)\n020: INotChar '\\n'\n021: IAny 1\n022: IPartialCommit (20)\n023: IChar '\\n'\n024: IPartialCommit (15)\n025: IReturn\n026: Cgroup open #16\n027: ICall (33)\n028: IChoice (31)\n029: ICall (33)\n030: IPartialCommit (29)\n031: Cgroup close #16\n032: IReturn\n033: Cgroup open #19\n034: Csymbol open #17\n035: ICall (44)\n036: Csymbol close #17\n037: ICall (14)\n038: ICall (47)\n039: Cgroup open #18\n040: ICall (8)\n041: Cgroup close #18\n042: Cgroup close #19\n043: IReturn\n044: ISet (45) {A-Z,_,a-z}\n045: ILeadSet (45) {A-Z,_,a-z}\n046: IReturn\n047: IChoice (51)\n048: IChar '<'\n049: IChar '-'\n050: ICommit (52)\n051: IChar '←'\n052: IReturn\n053: IChoice (67)\n054: Cgroup open #1\n055: ICall (88)\n056: ICall (14)\n057: IChar '|'\n058: ICall (14)\n059: ICall (53)\n060: IChoice (65)\n061: IChar '|'\n062: ICall (14)\n063: ICall (53)\n064: IPartialCommit (61)\n065: Cgroup close #1\n066: ICommit (69)\n067: ICall (88)\n068: ICall (14)\n069: IChoice (87)\n070: IChoice (84)\n071: Cgroup open #1\n072: ICall (88)\n073: ICall (14)\n074: IChar '|'\n075: ICall (14)\n076: ICall (53)\n077: IChoice (82)\n078: IChar '|'\n079: ICall (14)\n080: ICall (53)\n081: IPartialCommit (78)\n082: Cgroup close #1\n083: ICommit (86)\n084: ICall (88)\n085: ICall (14)\n086: IPartialCommit (70)\n087: IReturn\n088: Cgroup open #2\n089: ICall (97)\n090: ICall (14)\n091: IChoice (95)\n092: ICall (97)\n093: ICall (14)\n094: IPartialCommit (92)\n095: Cgroup close #2\n096: IReturn\n097: IChoice (102)\n098: IChar '&'\n099: ICall (14)\n100: ICall (97)\n101: ICommit (108)\n102: IChoice (107)\n103: IChar '!'\n104: ICall (14)\n105: ICall (97)\n106: ICommit (108)\n107: IJump (109)\n108: IReturn\n109: IChoice (114)\n110: ICall (191)\n111: ICall (14)\n112: INotSet (113) {*,+,:,<,?,^,|}\n113: ICommit (190)\n114: Cgroup open #9\n115: ICall (191)\n116: ICall (14)\n117: IChoice (122)\n118: Csymbol open #3\n119: ISet (120) {*,+,?}\n120: Csymbol close #3\n121: ICommit (152)\n122: IChoice (133)\n123: IChar '^'\n124: Csymbol open #6\n125: Csimple open #4\n126: ICommit (127)\n127: Csimple close #4\n128: Csimple open #5\n129: ICall (299)\n130: Csimple close #5\n131: Csymbol close #6\n132: ICommit (152)\n133: IChoice (145)\n134: IChoice (138)\n135: IChar '|'\n136: IChar '>'\n137: ICommit (140)\n138: IChar '<'\n139: IChar '|'\n140: ICall (14)\n141: Csymbol open #7\n142: ICall (44)\n143: Csymbol close #7\n144: ICommit (152)\n145: IChar '>'\n146: IChar ':'\n147: ICall (14)\n148: Csymbol open #8\n149: ICall (44)\n150: Csymbol close #8\n151: ICall (14)\n152: IChoice (189)\n153: IChoice (158)\n154: Csymbol open #3\n155: ISet (156) {*,+,?}\n156: Csymbol close #3\n157: ICommit (188)\n158: IChoice (169)\n159: IChar '^'\n160: Csymbol open #6\n161: Csimple open #4\n162: ICommit (163)\n163: Csimple close #4\n164: Csimple open #5\n165: ICall (299)\n166: Csimple close #5\n167: Csymbol close #6\n168: ICommit (188)\n169: IChoice (181)\n170: IChoice (174)\n171: IChar '|'\n172: IChar '>'\n173: ICommit (176)\n174: IChar '<'\n175: IChar '|'\n176: ICall (14)\n177: Csymbol open #7\n178: ICall (44)\n179: Csymbol close #7\n180: ICommit (188)\n181: IChar '>'\n182: IChar ':'\n183: ICall (14)\n184: Csymbol open #8\n185: ICall (44)\n186: Csymbol close #8\n187: ICall (14)\n188: IPartialCommit (153)\n189: Cgroup close #9\n190: IReturn\n191: IChoice (198)\n192: IChar '('\n193: Cgroup open #10\n194: ICall (8)\n195: Cgroup close #10\n196: IChar ')'\n197: ICommit (244)\n198: IChoice (201)\n199: ICall (245)\n200: ICommit (244)\n201: IChoice (204)\n202: ICall (264)\n203: ICommit (244)\n204: IChoice (207)\n205: ICall (283)\n206: ICommit (244)\n207: IChoice (219)\n208: Cgroup open #11\n209: IChar '{'\n210: IChar ':'\n211: ICall (8)\n212: IChar ':'\n213: IChoice (216)\n214: ICall (44)\n215: ICommit (216)\n216: IChar '}'\n217: Cgroup close #11\n218: ICommit (244)\n219: IChoice (225)\n220: Csymbol open #12\n221: IChar '{'\n222: IChar '}'\n223: Csymbol close #12\n224: ICommit (244)\n225: IChoice (232)\n226: Cgroup open #13\n227: IChar '{'\n228: ICall (8)\n229: IChar '}'\n230: Cgroup close #13\n231: ICommit (244)\n232: IChoice (237)\n233: Csymbol open #14\n234: IChar '.'\n235: Csymbol close #14\n236: ICommit (244)\n237: Csymbol open #15\n238: ICall (44)\n239: ICall (14)\n240: IPredChoice (243)\n241: ICall (47)\n242: IFailTwice\n243: Csymbol close #15\n244: IReturn\n245: IChoice (255)\n246: IChar '\"'\n247: Csymbol open #24\n248: IChoice (252)\n249: INotChar '\"'\n250: IAny 1\n251: IPartialCommit (249)\n252: Csymbol close #24\n253: IChar '\"'\n254: ICommit (263)\n255: IChar '\\''\n256: Csymbol open #25\n257: IChoice (261)\n258: INotChar '\\''\n259: IAny 1\n260: IPartialCommit (258)\n261: Csymbol close #25\n262: IChar '\\''\n263: IReturn\n264: IChar '['\n265: IChoice (268)\n266: IChar '^'\n267: ICommit (268)\n268: ICall (275)\n269: IChoice (273)\n270: INotChar ']'\n271: ICall (275)\n272: IPartialCommit (270)\n273: IChar ']'\n274: IReturn\n275: IChoice (278)\n276: ICall (283)\n277: ICommit (282)\n278: IChoice (281)\n279: ICall (288)\n280: ICommit (282)\n281: IAny 1\n282: IReturn\n283: IChar '%'\n284: Csymbol open #26\n285: ICall (44)\n286: Csymbol close #26\n287: IReturn\n288: Cgroup open #22\n289: Csimple open #21\n290: IAny 1\n291: Csimple close #21\n292: IChar '-'\n293: INotChar ']'\n294: Csimple open #20\n295: IAny 1\n296: Csimple close #20\n297: Cgroup close #22\n298: IReturn\n299: Csymbol open #23\n300: ISet (301) {0-9}\n301: ILeadSet (301) {0-9}\n302: Csymbol close #23\n303: IReturn\n304: IEnd"
    end
    @testset "PegMatch Offset Property" begin
        capn = ((P"a",) | R"az")^1
        @test match(capn, "sdfdasfarewaaawerwr").offsets == [5, 8, 12, 13, 14]
    end
    @testset "Preface Checks" begin
        prefix1 = P"abc" | P"abd" | P"abf"
        @test match(prefix1, "abc")[1] == "abc"
        @test match(prefix1, "abd")[1] == "abd"
        @test match(prefix1, "abf")[1] == "abf"
        @test match(prefix1, "abg") isa PegFail
        @test repr(prefix1.code) == "JLpeg.Instruction[JLpeg.ChoiceInst(5, JLpeg.IChoice), JLpeg.CharInst('a', JLpeg.IChar), JLpeg.CharInst('b', JLpeg.IChar), JLpeg.CharInst('c', JLpeg.IChar), JLpeg.LabelInst(9, JLpeg.ICommit), JLpeg.ChoiceInst(5, JLpeg.IChoice), JLpeg.CharInst('a', JLpeg.IChar), JLpeg.CharInst('b', JLpeg.IChar), JLpeg.CharInst('d', JLpeg.IChar), JLpeg.LabelInst(4, JLpeg.ICommit), JLpeg.CharInst('a', JLpeg.IChar), JLpeg.CharInst('b', JLpeg.IChar), JLpeg.CharInst('f', JLpeg.IChar), JLpeg.MereInst(JLpeg.IEnd)]"
        presetfix = P"12" * S"ab" * P"xy" | P"12" * S"ab" * P"xz"
        @test match(presetfix, "12axy")[1] == "12axy"
        @test match(presetfix, "12bxy")[1] == "12bxy"
        @test match(presetfix, "12bxz")[1] == "12bxz"
        @test match(presetfix, "12axz")[1] == "12axz"
        @test repr(presetfix.code) ==  "JLpeg.Instruction[JLpeg.ChoiceInst(7, JLpeg.IChoice), JLpeg.CharInst('1', JLpeg.IChar), JLpeg.CharInst('2', JLpeg.IChar), JLpeg.SetInst(475368975085586025561263702016, 1, JLpeg.ISet), JLpeg.CharInst('x', JLpeg.IChar), JLpeg.CharInst('y', JLpeg.IChar), JLpeg.LabelInst(6, JLpeg.ICommit), JLpeg.CharInst('1', JLpeg.IChar), JLpeg.CharInst('2', JLpeg.IChar), JLpeg.SetInst(475368975085586025561263702016, 1, JLpeg.ISet), JLpeg.CharInst('x', JLpeg.IChar), JLpeg.CharInst('z', JLpeg.IChar), JLpeg.MereInst(JLpeg.IEnd)]"
    end
end
