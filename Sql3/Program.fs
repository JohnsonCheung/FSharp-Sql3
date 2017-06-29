module rec Core =
    open System
    type File                    = IO.File
    type Strings                 = Microsoft.VisualBasic.Strings
    let  ftLy ft                 = File.ReadAllLines ft
    let  ftStr ft                = File.ReadAllText ft
    let  rmvEle   pfx ay         = Array.filter(fun i>isPfx pfx i)
    let  isPfx    pfx (s:string) = s.StartsWith pfx
    let  sz       (ay:'a[])      = ay.Length 
    let  ub       ay             = (sz ay) - 1
    let  splitCrLf(lines:string) = Strings.Split(lines,"\r\n")
    let  trim       s            = Strings.Trim s
    let  left     len s          = Strings.Left(s,len)
    let  right    len s          = Strings.Right(s,len)
    let  mid      pos len s      = Strings.Mid(s,len,pos)
    let  brkAt    pos sepLen s   = s |> left pos |> trim |> mid pos sepLen |> trim 
    let  instr    sub (s:string) = Strings.InStr(1,s,sub,Microsoft.VisualBasic.CompareMethod.Binary)
    let  instrRev sub (s:string) = Strings.InStrRev(1,s,sub,Microsoft.VisualBasic.CompareMethod.Binary)
    let  len      s              = Strings.Len(string s)
    let  brk      sep s          = let p = instr sep s in if(p=0) then throws "no sep[sep] in s[s]" in brkAt p (len sep) s
    let  brkSpc   s        = brk s " "
    let  spc      nSpc     = Strings.Space nSpc
    let  brk2       sep s = let s=trim s in let p=instr sep s in if(p=0) then "",s else brkAt p (len sep) s
    let  brk1       sep s = let s=trim s in let p=instr sep s in if(p=0) then s,"" else brkAt p (len sep) s
    let  takFst      sep s = brk1 sep s |> fst
    let  takSnd      sep s = brk1 sep s |> snd 
    let  takFstOrAll sep = brk1 sep >> fst
    let  takSndOrAll sep = brk1 sep >> snd
    let  fstChr          = left 1
    let  pthSep          = Path.DirectorySeparatorChar.ToString() 
    let  appendIf s pfx if' = if if' then s+pfx else s
    let  ffnPth   ffn       = let p = instrRev ffn pthSep in if(p=0) then "" else left ffn p
    let  ffnFn    ffn       = let p = instrRev ffn pthSep in if(p=0) then ffn else mid ffn (p+1)
    let  ffnExt   ffn       = let p = instrRev ffn "." in if(p=0) then "" else mid ffn p
    let  is1term  s         = any,s = anyTerm s; return any}
    let  anyTerm  s         = t,s = shiftTerm s; (not(isempty t)),s }
    let  noTerm   s         = t,s = shiftTerm s; (isempty t),s }
    let  is3Term  s         = let no,s=noTerm s
                              if no then false else no,s = onTerm s
                              if no then false else no,s = onTerm s 
                              if no then false else no,s = onTerm s
                              no
    let  isNonEmpty s =     not(isempty s)
    let  isEmpty    s =     if(System.String.IsNullOrEmpty s) then true else false //' not(trim s) match "\S+")}} 
    let  hasSpc     s =     strHas " "
    let  brkSpc1      =     Brk1 " "
    let  brkSpc2      =     Brk2 " "
    let  brkSpc       =     Brk  " " 
    let  inAy           = Array.Contains
    let  inLis          = List.Contains
    let  inStr    sub s = instr sub s > 0
    let  hasDd           = has s "--"
    let  hasDdd          = has s "---"
    let  shiftTerm  s    = brkSpc1 s
    let  srtFt      ft   = ft |> ftLy |> Array.sort | wrtAy ft 
    let  WrtObj          = let f=OpnWrtFt ft in ay |> Array.each(fun i->f.WriteLine i) in f.Close()
    let  jn         sep lis = Strings.Join(List.toArray lis,sep)
    let  jnCrLf     = jn "\r\n"
    let  opnWrtFt   = Microsoft.VisualBasic.FileIO.FileSystem.OpenTextFileReader(ft,false)
    let  opnAppFt   = Microsoft.VisualBasic.FileIO.FileSystem.OpenTextFileReader(ft,true)
    let  opnFt   ft = Microsoft.VisualBasic.FileIO.FileSystem.OpenTextFileRead ft
    let  rmvSfxPthSep = rmvSfx pthSep
    let  rmvSfx    sfx s = if(isSfx sfx s) then left ((len s) - (len sfx)) s else s
    let  jnPth     lis = List.map rmvSfxPthSep |> (jn pthSep) + pthSep
    let  tmpfdr     fdr = let p = JnPth ((TmpPth)+fdr),(tmpnm) in ensPth p in p 
    let  tmpPth     ()  = let p = jnPth(env tmp) "mypowershell" in ensPth p in p
    let  ensPth     pth = if(not(pthIsExist pth)) then crtPth pth
    let  tmpNm          = "T" + System.DateTime.Now.ToString("yyyy_MM_Dd_HHmmss")
    let  tmpFn      ext = tmpNm() + ext
    let  tmpFtFn        = tmpFn() ".txt" 
    let  tmpFt          = tmpPth() + tmpFtFn()
    let  rmvRmk         = taks1OrAll
    let  rmvDdRmk       = rmvRmk "--"
    let  rmvDddRmk      = rmvRmk "---"
    let  rmvEmpty       = Array.filter isNonEmpty
    let  brwAy    ay    = let t= tmpFt() in wrtAy t ay in brwFt t
    let  terms3    s    = let s = rmvRmk s in let l1,s = shiftTerm s in let l2,s = shiftTerm s in l1,l2,s
    let  brwPth   pth   = if(pthIsExist pth) then shell "explorer ""Pth""" NormalFocus
    let  brwTmpPth      = brwPth(tmpPth())
    let  brwFt     ft   = shell "notepad.exe ""ft""" sty Microsoft.VisualBasic.Interaction.WinAppStyle.NormalFocus }}
    type sty = Microsoft.VisualBasic.AppWinStyle
    let  shell     cmd (sty:sty) = Microsoft.VisualBasic.Interaction.Shell(cmd,Style.VisiualBasic.AppWinStyle,false,-1)
    let  minusLis    lis1 lis2 = lis1|>Array.filter(fun i->inLis lis1 i)
    l
    // # srtFt MyInvocation.InvocationName

    [<EntryPoint>]
    let main argv = 
        printfn "%A" argv
        0 // return an integer exit code
