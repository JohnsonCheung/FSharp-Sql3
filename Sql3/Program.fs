namespace rec Library
[<AutoOpen>]
module Sql3 =
    open Microsoft.FSharp.Collections
    type linTy = |L1Only|L12Only|L123Only|L2Only|L23Only|L3Only|RmkOnly|Impossible|LvlIndtEr
    type private map = Map<string,string>
    let  isL3     s =  (isPfx (spc 8) s) && ((mid 9 1 s) <> " ")
    let  isL2     s =  (isPfx (spc 4) s) && ((mid 5 1 s) <> " ")
    let  isL1     s =  (not(isRmk s))    && ((fstChr s)  <> " ")
    let  isL1Er   s =  (not(isL1Only s))||(not(isL12Only s))
    let  isL2Er   s =  (not(isL2Only s))||(not(isL23Only s))
    let  isL3Er   s =  not(isL3Only s)
    let  isL1Only s =  (isL1 s) && (is1Term s)
    let  isL12Only s = true
    let  isL2Only s = true
    let  isL23Only s = true
    let  isL3Only  s  = (isL3 s) && (is1Term s)
    let  is1Term  s = sz (brkNTerm 2 s) = 1
    let  is2Term  s = sz (brkNTerm 3 s) = 2
    let  nTerm    s =  s |> rmvDdRmk |> brkNTerm 3 |> sz
    let  isRmk    (s:string) =  isPfx "--" s
    let  isIndtEr s =  not((isRmk s)||(isL1 s)||(isL2 s)||(isL3 s))
    let  sql3Ly'rootVal sql3Ly =  let map = sql3Ly |> vdtSql3 |> sql3'3Lvl |> sql3Map in let root = map |> sql3Map'root in map.Item root
    let  sql3Map'root   sql3Map = ""
    let  sql3Map sql3Ly:map = Map.empty<string,string>
    let  addExprErMsg s = s
    let  addIndtErMsg s =  appendIf "--- ident error" (isIndtEr s) s
    let  addLvlErMsg (s:string) = 
        if     (isL1Er s) then s + " --- Lvl1 should have 1 or 3 terms only"
        else if(isL2Er s) then s + " --- Lvl2 should have 1 or 2 terms only"
        else if(isL3Er s) then s + " --- Lvl3 should have 1 term only"
        else s                                
    let  vdtSql3   sql3Ly = sql3Ly |> rmvErMsg |> vdtIndt |> vdtLvl |> vdtExpr |> filterSql3Er
    let  filterSql3Er sql3Ly = if(Array.exists (isInStr "---") sql3Ly) then sql3Ly else emptySy()
    let  rmvErMsg  sql3Ly = sql3Ly |> Array.map rmvDddRmk
    let  vdtIndt   sql3Ly = sql3Ly |> Array.map addIndtErMsg
    let  vdtLvl    sql3Ly = sql3Ly |> Array.map addLvlErMsg
    let  vdtExpr   sql3Ly = sql3Ly |> Array.map addExprErMsg
    let  sql3'3Lvl sql3Ly = seq {
                                let l1 = ""
                                let l2 = ""
                                for s in sql3Ly do 
                                    let l1,l2,l3 = lvl123 s l1 l2
                                    if(l3<>"") then yield jnSpc [l1;l2;l3]
                            } |> Seq.toArray
    let linTy lin =
        let s = rmvDdRmk lin
        if(isL1 s) then
            match brkNTerm 3 lin |> sz with
            | 1 -> L1Only
            | 2 -> L12Only
            | 3 -> L123Only
            | _ -> Impossible
        else if(isL2 s) then
            match brkNTerm 3 lin |> sz with
            | 1 -> L2Only
            | 2 -> L23Only
            | _ -> Impossible
        else if (isL3 s) then L3Only
        else if (isRmk s) then RmkOnly
        else LvlIndtEr
    let  lvl123 s l1 l2 = 
        let t = brkNTerm 3 s
        match linTy s with
        | L1Only   -> t.[0],"",""
        | L12Only  -> t.[0], t.[1], ""
        | L123Only -> t.[0], t.[1], t.[2]
        | L2Only   -> l1, t.[0], ""
        | L23Only  -> l1, t.[0], t.[1]
        | L3Only   -> l1, l2, t.[0]
        | RmkOnly  -> l1, l2, ""
        | _ -> failwith "Impossible"
    let sql3 = """
-- Rmk: -- is remark
-- 3Lvl: alway 3 level
-- Lvl1: is namespace
-- Lvl2: is name.  That means is always under a namespace
-- Root Ns: fst non remark line is root ns
-- Lvl3: is expression
-- Lvl2Nm-?: can be have optional ? in front which means its value can be empty string
-- Lvl2Nm-?-Fst-term-of- expression: It must belong with ?
-- ?: namepace-? is for 
Sql
     Drp  .Drp Tx TxMbr MbrDta Div Sto Crd Cnt Oup MbrWs
    T @ Tx TxMbr ?MbrDta Div Sto Crd
    O @ Cnt Oup ?MbrWs
Sql.T
    Tx
        .Sel@ Crd Amt Qty Cnt ?Mbr ?Div ?Sto ?Dte
        .Into
        .Fm SalesHistory
        .Wh SHSDate between '@P.Fm' and '@P.To'
        .And@ ?Div ?Sto ?Dte
        .Gp@ Crd ?Mbr ?Div ?Sto ?Dte
    Tx .Upd .Set@ TxWD
    TxMbr
        .SelDis Mbr
        .Into
        .Fm #Tx
    ?MbrDta ?BrkMbr 
        .Sel@ Mbr Age Sex Sts Dist Area 
        .Into
        .Fm JCMember
        .Wh JCMCode (Select Mbr From #TxMbr)
    Div .Sel@ Div Nm Seq Sts .Fm Division
    Sto .Sel@ Sto Nm CNm .Fm LocTbl
    Crd .Sel@ Crd Nm .Fm JR_FrqMbrLis_#CrdTy()
Sql.O
    Cnt .Sel@ ?MbrCnt RecCnt TxCnt Qty Amt .Into .Fm #Tx
    Oup
        .Sel@ Crd ?Mbr ?Sto ?Div ?Dte Amt Qty TxCnt
        .Into
        .Fm #Tx x
        .Jn@ Crd ?Div ?Sto ?Mbr
    ?MbrWs ?SelMbr 
        .Sel@ Mbr ?Nm ?Adr ?Mail ?Phone 
        .Into
        .Fm JCMember 
        .Wh JCMCode in (Select Mbr From #TxMbr)
Sql.T.Tx.Set
    TxWD ...
Sql.T.Tx.Sel
    Crd @ CasewhenThen Else End
    Amt Sum(SHAmount)
    Qty Sum(SHQty)
    Cnt Count(SHInvoice+SHSDate+SHRef)
    ?Mbr ?BrkMbr JCMMCode
    ?Div ?BrkDiv @Expr
    ?Sto ?BrkSto @Expr
    ?Dte @Expr
@Sql.T.Tx.Sel.Crd
    CasewhenThen ...
    Else ...
    :NEnd .Repeat :N END~
    End | :NEnd
@Sql.T.Tx.And
    ?Div ?SelDiv And .Fld@ in (.List@)
    ?Crd ?SelCrd And .Fld@ in (.List@)
    ?Sto ?SelSto And .Fld@ in (.List@)
Sql.T.Tx.And.?Div Fld @Expr.Div
Sql.T.Tx.And.?Crd Fld @Expr.Crd
Sql.T.Tx.And.?Sto Fld @Expr.Sto
Sql.T.Tx.And.?Div List @In.Div
Sql.T.Tx.And.?Sto List @In.Sto
Sql.T.Tx.And.?Crd List @In.Crd
Sql.T.Tx.Gp
    Crd @Expr.Crd
    ?Mbr ?BrkMbr SHMCode
    ?Div ?BrkDiv @Expr.Div
    ?Sto ?BrkSto @Expr.Sto
    ?Dte ?BrkDte @Expr.Dte
Expr
    Div
    Sto
    Dte
        ?SumY @Expr. TxY
        ?SumM @Expr. TxY TxM
        ?SumW @Expr. TxY TxM TxW
        ?SumD @Expr. TxY TxM TxW TxD TxWD TxDte
    TxY
    TxM
    TxW
    TxD
    TxWD
    TxDte
Sql.T.?MbrDta.Sel
    Mbr JCMCode
    Age DATEDIFF(YEAR,CONVERT(DATETIME ,JCMDOB,112),GETDATE())
    Sex JCMSex
    Sts JCMStatus
    Dist JCMDist
    Area JCMArea
Sql.T.Div.Sel
    Div Dept + Division
    Nm LongDesc
    Seq Seq
    Sts Status
Sql.T.Sto.Sel
    Sto '0'+Loc_Code
    Nm Loc_Name
    CNm Loc_CName
Sql.T.Crd.Sel
    Crd CrdTyId
    Nm CrdTyNm
?
    SelDiv .Ne @P.DivLis *Blank
    SelCrd .Ne @P.CrdLis *Blank
    SelSto .Ne @P.StoLis *Blank
    BrkDiv .Eq @P.BrkSto 1
    BrkSto .Eq @P.BrkSto 1
    BrkSto .Eq @P.BrkSto 1
    Y .Eq @P.SumLvl Y
    M .Eq @P.SumLvl M
    W .Eq @P.SumLvl W
    D .Eq @P.SumLvl D
    Dte .Or Y M W D
    AnyMbrInf @P !Or .InclAdr .InclPhone .InclMail
    Mbr .And BrkMbr AnyMbrInf
Sql.O.Oup.Sel
    ?Mbr ?BrkMbr Mbr
    ?Sto ?BrkSto Sto
    ?Div ?BrkDiv Div
Sql.O.Oup.Sel.Dte
    Y TxY
    M TxY TxM
    W TxY TxM TxW
    D TxY TxM TxW TxD TxWD TxDte
Sql.O.Cnt.Sel
    MbrCnt 
    RecCnt Count(*)
    TxCnt Sum(TxCnt)
    Qty Sum(Qty)
    Amt Sum(Amt)
Sql.O.Oup.Jn
    Crd | Left Join #Crd a #Crd on a.Crd=x.Crd
    ?Mbr ?BrkMbr | Left Join #MbrDta b on a.Mbr = x.Mbr
    ?Div ?BrkDiv | Left Join #Div on c.Div = x.Div
    ?Sto ?BrkSto | Left Join #Sto on d.Sto = x.Sto ---aaa
"""

[<AutoOpen>]
module rec Core =
    open Microsoft.VisualBasic
    open System.IO
    open System
    type allBuilder() = member x.Return m = m; member x.Zero() = true; member x.Combine(a,b) = if(a) then true else b; member x.Delay(f) = f()
    type anyBuilder() = member x.Return m = m; member x.Zero() = false; member x.Combine(a,b) = if(a)then true else b; member x.Delay(f) = f()
    type File         = System.IO.File
    type FileSystem   = FileIO.FileSystem
    type Strings      = Microsoft.VisualBasic.Strings
    type sty          = AppWinStyle
    let  any                     = anyBuilder()
    let  all                     = allBuilder()
    let  anyTerm   s             = let t,_ = shiftTerm s in isNonEmpty t
    let  appendIf  pfx if' s     = if if' then (s+pfx) else s
    let  aySy                    = Array.map toStr
    let  brk       sep s         = let p = instr sep s in if(p=0) then failwith "no sep[sep] in s[s]" else brkAt p (len sep) s
    let  brk1      sep s         = let s = trim s in let p=instr sep s in if(p=0) then s,"" else brkAt p (len sep) s
    let  brk2      sep s         = let s = trim s in let p=instr sep s in if(p=0) then "",s else brkAt p (len sep) s
    let  brkAt     pos sepLen s  = s |> left pos |> trim,  s|> mid pos sepLen |> trim
    let  brkSpc                  = brk  " " 
    let  brkSpc1                 = brk1 " "
    let  brkSpc2                 = brk2 " "
    let  brwAy     ay            = let t = tmpFt() in wrtAy t ay; brwFt t
    let  brwFt     ft            = shell(sprintf """notepad.exe "%A" """ ft)(sty.NormalFocus )
    let  brwPth    pth           = if(pthIsExist pth) then shell(sprintf """explorer "%A" """ pth)  sty.NormalFocus
    let  brwTmpPth               = brwPth(tmpPth())
    let  crtPth    pth           = ()
    let  emptySy   ()            = Array.empty<string>
    let  ensPth    pth           = if(not(pthIsExist pth)) then crtPth pth
    let  env       nm            = ""
    let  ffnExt    ffn           = let p = instrRev "."    ffn in if(p=0) then "" else substr p ffn
    let  ffnFn     ffn           = let p = instrRev pthSep ffn in if(p=0) then ffn else substr (p+1) ffn
    let  ffnPth    ffn           = let p = instrRev pthSep ffn in if(p=0) then "" else left p ffn
    let  fstChr    s             = left 1 s
    let  ftLy      ft            = File.ReadAllLines ft
    let  ftStr     ft            = File.ReadAllText  ft
    let  hasDd                   = isInStr "--"
    let  hasDdd                  = isInStr "---"
    let  hasSpc                  = isInStr " "
    let  isInAy                    = Array.contains
    let  isInLis                   = List.contains
    let  isInStr  sub s          = instr sub s > 0
    let  instr    sub s          = Strings.InStr(s,sub,CompareMethod.Binary)
    let  instrRev sub s          = Strings.InStrRev(s,sub,1,CompareMethod.Binary)
    let  is1term    s            = (brkNTerm 2 s |> sz) = 1
    let  is3Term    s            = (brkNTerm 4 s |> sz) = 3
    let  isEmpty    s            = if(System.String.IsNullOrEmpty s) then true else false //' not(trim s) match "\S+") 
    let  isNonEmpty s            = not(isEmpty s)
    let  isPfx      pfx (s:string) = s.StartsWith pfx
    let  isSfx      sfx (s:string) = s.EndsWith sfx
    let  jn         sep lis = let sy:string[] = lstSy lis in Strings.Join(sy,sep)
    let  jnCrLf = jn "\r\n"
    let  jnSpc  = jn " "
    let  jnPth  (lis:string list) = (lis |>List.map rmvPthSfx |> (jn pthSep)) + pthSep
    let  left   len s          = Strings.Left(s,len)
    let  len    s              = Strings.Len(string s)
    let  mid    pos len s      = let s:string = s in Strings.Mid(s,len,pos)
    let  minusLis  lis1 lis2 = lis1|>Array.filter(fun i->isInLis i lis2)
    let  noTerm     s            = not(anyTerm s) 
    let  opnAppFt   ft           = FileSystem.OpenTextFileWriter(ft,true)
    let  opnFt      ft           = FileSystem.OpenTextFileReader ft
    let  opnWrtFt   ft           = FileSystem.OpenTextFileWriter(ft,false)
    let  pthIsExist pth          = true
    let  pthSep                  = Path.DirectorySeparatorChar.ToString() 
    let  right      len s        = Strings.Right(s,len)
    let  rmvDddRmk  s            = rmvRmk "---" s
    let  rmvDdRmk   s            = rmvRmk "--"  s
    let  rmvEle     pfx ay       = Array.filter(fun i->isPfx pfx i)
    let  rmvEmpty                = Array.filter isNonEmpty
    let  rmvRmk     rmkPfx s:string     = takFstOrAll rmkPfx s
    let  rmvSfx     sfx s        = if(isSfx sfx s) then (left ((len s) - (len sfx)) s) else s
    let  rmvPthSfx               = rmvSfx pthSep
    let  shell     cmd (sty:sty) = Interaction.Shell(cmd,sty,false,-1) |> ignore
    let  shiftTerm s             = brkSpc1 s
    let  spc       nSpc          = Strings.Space nSpc
    let  splitCrLf (lines:string) = Strings.Split(lines,"\r\n")
    let  srtFt      ft   = ft |> ftLy |> Array.sort |> wrtAy ft
    let  str        (s:string)   = s
    let  substr   pos s          = Strings.Mid(s,pos)
    let  sz       (ay:'a[])      = ay.Length 
    let  brkNTerm'folder      (lis,s) _ = let t,s=shiftTerm s in if(isEmpty t) then lis,s else lis@[t],s
    let  brkNTerm    atMost s= seq {1..atMost} |> Seq.fold brkNTerm'folder (List.empty<string>,s) |> fst |> List.toArray
    let  takFst      sep s = brk1 sep s |> fst
    let  takFstOrAll sep s = brk1 sep s |> fst
    let  takSnd      sep s = brk2 sep s |> snd 
    let  takSndOrAll sep s = brk2 sep s |> snd
    let  terms3   s     = let s = rmvDdRmk s in let l1,s = shiftTerm s in let l2,s = shiftTerm s in l1,l2,s
    let  tmpfdr   fdr   = let p = jnPth[tmpPth();fdr;tmpNm()] in ensPth p; p 
    let  tmpFn    ext   = tmpNm() + ext
    let  tmpFt    ()    = tmpPth() + tmpFtFn()
    let  tmpFtFn  ()    = tmpFn ".txt" 
    let  tmpNm    ()    = "T" + DateTime.Now.ToString("yyyy_MM_Dd_HHmmss")
    let  tmpPth   ()    = let p = jnPth[env "tmp";"mypowershell"] in ensPth p; p
    let  toStr    i     = i.ToString()
    let  trim     s     = Strings.Trim s
    let  ub       ay    = (sz ay) - 1
    let  wrtAy    ft ay = use f=(opnWrtFt ft) in ay |> Array.iter(fun(i)->f.WriteLine(box i))
    let lstStrLis       = List.map toStr
    let lstSy     lis   = lstStrLis lis |> List.toArray |> aySy
    // # srtFt MyInvocation.InvocationName
    [<EntryPoint>]
    let main args = 0
    ()
//        printfn "%A" argv
//        0 // return an integer exit code