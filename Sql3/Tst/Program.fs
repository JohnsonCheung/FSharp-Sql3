namespace Tst
open Microsoft.VisualStudio.TestTools.UnitTesting
open Library
[<TestClass>]
type Sql3() = 
    [<TestMethod>]
    member x.isRmk() =
        let run lin exp = 
            let act = isRmk lin
            assert (act = exp)
        run "---sdsd" true
        run "   dfldf" false
    [<TestMethod>]
    member x.isL1() =
        let run lin exp = assert ((isL1 lin) = exp)
        run "sdlf sd fdf" true
        run " sldkfds fdf" false
        run "    sdfkdlf"  false
    [<TestMethod>]
    member x.brkNTerm() =
        let run lin atMost exp = 
            let act = brkNTerm atMost lin
            assert(act=exp)
        run "a b c " 2 [|"a";"b"|]
        run "a b c " 3 [|"a";"b";"c"|]
        run "a b c " 4 [|"a";"b";"c"|]
        run "  a b c " 2 [|"a";"b"|]
        run "  a b c " 3 [|"a";"b";"c"|]
        run "  a b c " 4 [|"a";"b";"c"|]
    [<TestMethod>]
    member x.sql3'3Lvl() = sql3'3Lvl (splitCrLf sql3) |> brwAy

    [<TestMethod>]
    member x.is1Term() =
        let run lin exp = assert((is1Term lin)=exp)
        run "sdlsdf " true
        run " kdf " true
        run "sdfdf" true
        run "a a" false
        run " a b" false
    [<TestMethod>]
    member x.isL1Only() =
        let run lin exp = assert ((isL1Only lin) = exp)
        run "sdlf sd fdf" false
        run "sldkfdsfdf" true
        run "  sdfkdlf"  false
[<TestClass>]
type Core() =
    [<TestMethod>]
    member x.tmpFt() = ()
        
        
