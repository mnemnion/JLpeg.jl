if false include("../src/JLpeg.jl") end

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
        # The empty sequence
        ptrue = P""
        @test match(ptrue, "")[1] == ""
        pback = P"\x79"  # edge case...
        @test match(pback, "\x79") isa PegMatch
        @test match(P"\x00", "\x00") isa PegMatch
    end
    @testset "Any" begin
        @test match(P(3), "abcd")[1] == "abc"
        @test match(P(5), "abcd") isa PegFail
        @test match(P(0), "")[1] == ""
    end
    @testset "Byte" begin
        pb = U8(0xff)
        @test match(pb, "\xff") isa PegMatch
        @test match(pb, "\xfe") isa PegFail
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
        diffset = R"az" - R"di"
        @test match(diffset, "c") == ["c"]
        @test match(diffset, "i") isa PegFail
        edgeset = S"\x00\x39\x40\x79\x80\x81"^1
        @test match(edgeset, "\x00\x39\x79\x80\x81") isa PegMatch
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
        @rule :afail ← "a" | ∅
        @test match(afail, "b") isa PegFail
        @rule :asucceed ← "a" | ε
        @test match(asucceed, "b") == [""]
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

    @testset "Actions" begin
        func = A("" >> P"func", uppercase)
        @test match(func, "Make my func the Pfunc")[1] == "MAKE MY FUNC"
        funky = "" >> C("func") <| uppercase
        @test match(funky, "make my fun the Pfunc")[1] == "FUNC"
        bythree(str) = parse(Int, str) % 3 == 0
        pthree = Q(R"09"^1, bythree)
        @test match(pthree, "12") isa PegMatch
        @test match(pthree, "13") isa PegFail
        vmc(vm) = (vm.s = 1; return true)
        vmset = (P"123", :a) * Avm!(P"", vmc) * (P"123", :b)
        @test match(vmset, "123") == [:a => "123", :b => "123"]
    end

    @testset "PegMatch Interface" begin
        @rule :pmix <-- ("a",) * ("b", :b) * ("c",)
        mix = match(pmix, "abc")
        @test collect(enumerate(mix)) == [(1, "a"), (2, "b"), (3, "c")]
        @test collect(pairs(mix)) == [1 => "a", :b => "b", 3 => "c"]
        @test collect(mix) == ["a", "b", "c"]
        @test haskey(mix, :b) === true
        @test haskey(mix, pmix) === false
        @test haskey(mix, 2) === true
        @test haskey(mix, 5) === false
        mxcap = mix.captures
        @test haskey(mxcap, :b) === true
        @test haskey(mxcap, match) === false
        @test haskey(mxcap, 0) === false
        @test haskey(mxcap, 3) === true
        @test haskey(mxcap, 2) === true
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
        @test match(re, "'string'")[:pattern][:string] == "string"
        @test match(re, "<a-z>")[:pattern][:range]== ["a", "z"]
        @test match(re, "sym")[:pattern][:call] == "sym"
        @test match(re, "a <- b c*")[:grammar] isa PegCapture
        @test match(re, "a <- c^1") isa PegMatch
        @test match(re, "a <- 'a'+ 'b'* c?") isa PegMatch
        @test match(re, "exp <- S grammar | alternative") isa PegMatch
        @test match(re, "a <- '123'^[1:3] |> action ") isa PegMatch
        @test match(re, "a <- '123'^[1:3] |? run_action") isa PegMatch
        @test match(re, "a <- '123' * ('4'+, :fours)") isa PegMatch

        lpegre = """
        pattern         <- exp !.
        exp             <- S (grammar | alternative)

        alternative     <- seq ('/' S seq)*
        seq             <- prefix*
        prefix          <- '&' S prefix / '!' S prefix / suffix
        suffix          <- primary S (([+*?]
                                    / '^' [+-]? num
                                    / '->' S (string / '{}' / name)
                                    / '>>' S name
                                    / '=>' S name) S)*

        primary         <- '(' exp ')' / string / class / defined
                         / '{:' (name ':')? exp ':}'
                         / '=' name
                         / '{}'
                         / '{~' exp '~}'
                         / '{|' exp '|}'
                         / '{' exp '}'
                         / '.'
                         / name S !arrow
                         / '<' name '>'         # old-style non terminals

        grammar         <- definition+
        definition      <- name S arrow exp

        class           <- '[' '^'? item (!']' item)* ']'
        item            <- defined / range / .
        range           <- . '-' [^]]

        S               <- (%s / '--' [^%nl]*)*   # spaces and comments
        name            <- [A-Za-z_][A-Za-z0-9_]*
        arrow           <- '<-'
        num             <- [0-9]+
        string          <- '"' [^"]* '"' / "'" [^']* "'"
        defined         <- '%' name
        """
        @test_skip match(re, lpegre) isa PegMatch
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
        @test match(sphinx0, "sphinx of black quartz judge my vow").full == true
    end

    @testset "Type tests" begin
        for I in subtypes(J.Instruction)
            if hasfield(I, :vec)
                @test I === J.InstructionVec
                @test length(methods(iterate, (I, Integer))) > 0
            end
        end
        function offsetof(T::Type, sym::Symbol)
            for i = 1:fieldcount(T)
                if fieldname(T, i) == sym
                    return fieldoffset(T, i)
                end
            end
        end
        for I in subtypes(J.Instruction)
            if I === J.OpenCallInst
                continue
            end
            @test isbitstype(I)
            if hasfield(I, :op)
                if offsetof(I, :op) ≠ 7
                    println(I)
                end
                @test offsetof(I, :op) == 7
            end
        end
    end

    @testset "Code tests" begin
        # Tests for code optimization purposes go here
        setinst = compile!(S"123").code[1]
        @test isbits(setinst)
        allrange = compile!((R"az" | R"αω" | R"ሀሏ" | R"👆👏")^1 * !P(1))
        @test repr("text/plain", allrange.code) == "01: ILeadSet (25)\n02:   Vector: {}\n03:   Vector: {22-3b}\n04: ILeadMulti (9)\n05:   Vector: {0f,10,22,31}\n06: IByte cf (17)\n07: IByte f0 (11)\n08: IByte ce (19)\n09: IByte e1 (13)\n10: IFail\n11: IByte 9f (15)\n12: IFail\n13: IByte 88 (21)\n14: IFail\n15: IByte 91 (23)\n16: IFail\n17: IMultiVec (25)\n18:   Vector: {01-0a}\n19: IMultiVec (25)\n20:   Vector: {32-40}\n21: IMultiVec (25)\n22:   Vector: {01-10}\n23: IMultiVec (25)\n24:   Vector: {07-10}\n25: IChoice (51)\n26: ILeadSet (50)\n27:   Vector: {}\n28:   Vector: {22-3b}\n29: ILeadMulti (34)\n30:   Vector: {0f,10,22,31}\n31: IByte cf (42)\n32: IByte f0 (36)\n33: IByte ce (44)\n34: IByte e1 (38)\n35: IFail\n36: IByte 9f (40)\n37: IFail\n38: IByte 88 (46)\n39: IFail\n40: IByte 91 (48)\n41: IFail\n42: IMultiVec (50)\n43:   Vector: {01-0a}\n44: IMultiVec (50)\n45:   Vector: {32-40}\n46: IMultiVec (50)\n47:   Vector: {01-10}\n48: IMultiVec (50)\n49:   Vector: {07-10}\n50: IPartialCommit (26)\n51: IPredChoice (54)\n52: IAny 1\n53: IFailTwice\n54: IEnd"
    end

    @testset "PegMatch Offset Property" begin
        capn = ((P"a",) | R"az")^1
        @test match(capn, "sdfdasfarewaaawerwr").offsets == [5, 8, 12, 13, 14]
    end

    @testset "Mark and Check" begin
        mark1 = M(R"09"^1, :numsame) * P":" * K(R"09"^1, :numsame)
        # Note: the builtin tags are hardcoded into the VM,
        # so we must test that they stay stable
        @test mark1[2][2].check_tag == 0x0001
        @test match(mark1, "123:123") isa PegMatch
        @test match(mark1, "123:124") isa PegFail
        mark2 = M(R"09"^1, :nums) * M(R"az"^1, :lets) * K(R"09"^1, :nums) * K(R"az"^1, :lets)
        @test match(mark2, "012abc012abc") isa PegMatch
        @test match(mark2, "012abc012abd") isa PegFail
        @test match(mark2, "012abc013abc") isa PegFail
        mlen = M(R"az"^1, :letter) * K(R"AZ"^1, :letter, :length)
        @test mlen[2].check_tag == 0x0002
        @test match(mlen, "abcABC") isa PegMatch
        @test match(mlen, "abcABCD") isa PegFail
        @test match(mlen, "abcAB") isa PegFail
        mclose = (R"az"^1 | M(R"09"^1, :num)) * K(P"Moe", :num, :close)
        @test mclose[2].check_tag == 0x0003
        @test match(mclose, "123Moe") isa PegMatch
        @test match(mclose, "abcMoe") isa PegFail
        malways = M(R"09", :d1) * M(R"09", :digit) * K(R"09", :digit, :always) * K(R"09", :d1)
        @test match(malways, "1991") isa PegMatch
        @test match(malways, "1971") isa PegMatch
        mall2 = R"09" * K(R"09", :noproblem, :always)
        @test mall2[2].check_tag == 0x0004
        @test match(mall2, "22") isa PegMatch
        p0, p1 = P"0"^1, P"1"^1
        m_gt = M(p0, :bin) * K(p1, :bin, :gt)
        @test m_gt[2].check_tag == 0x0005
        @test match(m_gt, "0001111") isa PegMatch
        @test match(m_gt, "000111") isa PegFail
        m_lt = M(p0, :bin) * K(p1, :bin, :lt)
        @test m_lt[2].check_tag == 0x0006
        @test match(m_lt, "0000111") isa PegMatch
        @test match(m_lt, "000111") isa PegFail
        m_gte = M(p0, :bin) * K(p1, :bin, :gte)
        @test m_gte[2].check_tag == 0x0007
        @test match(m_gte, "0001111") isa PegMatch
        @test match(m_gte, "000111") isa PegMatch
        @test match(m_gte, "00011") isa PegFail
        m_lte = M(p0, :bin) * K(p1, :bin, :lte)
        @test m_lte[2].check_tag == 0x0008
        @test match(m_lte, "000111") isa PegMatch
        @test match(m_lte, "00011") isa PegMatch
        @test match(m_lte, "0001111") isa PegFail
        markfn = M(R"az"^1, :alphas) * P":" * K(R"az"^1, :alphas, (s1,s2) -> occursin("a", s1) && occursin("z", s2))
        @test match(markfn, "dieda:dzzbs") isa PegMatch
        @test match(markfn, "diedo:dzzbs") isa PegFail
    end
    @testset "Preface Checks" begin
        prefix1 = P"abc" | P"abd" | P"abf"
        @test match(prefix1, "abc")[1] == "abc"
        @test match(prefix1, "abd")[1] == "abd"
        @test match(prefix1, "abf")[1] == "abf"
        @test match(prefix1, "abg") isa PegFail
        @test repr(prefix1.code) == "JLpeg.Instruction[JLpeg.ChoiceInst(5, 0xffff, 0xff, JLpeg.IChoice), JLpeg.CharInst(0x61, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.CharInst(0x62, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.CharInst(0x63, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.LabelInst(9, 0xffff, 0xff, JLpeg.ICommit), JLpeg.ChoiceInst(5, 0xffff, 0xff, JLpeg.IChoice), JLpeg.CharInst(0x61, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.CharInst(0x62, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.CharInst(0x64, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.LabelInst(4, 0xffff, 0xff, JLpeg.ICommit), JLpeg.CharInst(0x61, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.CharInst(0x62, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.CharInst(0x66, 0x00, 0x00, 0x00, 0xffff, 0x01, JLpeg.IChar), JLpeg.MereInst(0xffffffff, 0xffff, 0xff, JLpeg.IEnd)]"
        presetfix = P"12" * S"ab" * P"xy" | P"12" * S"ab" * P"xz"
        @test match(presetfix, "12axy")[1] == "12axy"
        @test match(presetfix, "12bxy")[1] == "12bxy"
        @test match(presetfix, "12bxz")[1] == "12bxz"
        @test match(presetfix, "12axz")[1] == "12axz"
        @test repr("text/plain", presetfix.code) == "01: IChoice (10)\n02: IChar 1\n03: IChar 2\n04: ISet (7)\n05:   Vector: {}\n06:   Vector: {22,23}\n07: IChar x\n08: IChar y\n09: ICommit (17)\n10: IChar 1\n11: IChar 2\n12: ISet (15)\n13:   Vector: {}\n14:   Vector: {22,23}\n15: IChar x\n16: IChar z\n17: IEnd"
    end
end
