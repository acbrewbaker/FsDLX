namespace FsDLX.Tomasulo

open FsDLX.Common


type RegisterFile private () =
    static let instance = RegisterFile()
    let regs = Array.init Config.Registers.RegCount Register.Init //Register.ArrayInit 64

    member rf.Item
        with    get i = regs.[i]
        and     set i v = regs.[i].Contents <- v
    
//    member rf.Item
//        with get(i, idx) =
//            let r = (Convert.int2bin i).[idx..idx+5] |> Convert.bin2int
//            regs.[r]

    member rf.Update(cdb) = regs |> Array.iter (fun reg -> reg.Update(cdb))

    static member GetInstance = instance

    static member HasContent (regs:Register[]) =
        not(regs |> Array.forall (fun reg -> (reg.Contents = 0) && (reg.Qi.IsNone)))

and GPR private () =
    
    static let instance = GPR()

    member gpr.Item
        with get i = 
            if i > 31 then failwith "invalid GPR index"
            else RegisterFile.GetInstance.[i]
      

    member gpr.Regs() = [| for i = 0 to 31 do yield gpr.[i] |]

    member gpr.R0toR7() = RegisterSet("R0-R7", [| for i = 0 to 7 do yield gpr.[i] |])
    member gpr.R8toR15() = RegisterSet("R8-R15", [| for i = 8 to 15 do yield gpr.[i] |])
    member gpr.R16toR23() = RegisterSet("R16-R23", [| for i = 16 to 23 do yield gpr.[i] |])
    member gpr.R24toR31() = RegisterSet("R24-R31", [| for i = 24 to 31 do yield gpr.[i] |])


    override gpr.ToString() =
        (sprintf "%O\n%O\n%O\n%O" (gpr.R0toR7()) (gpr.R8toR15()) (gpr.R16toR23()) (gpr.R24toR31()))
            .Trim()

    member gpr.Dump() = 
        (sprintf "\n%s\n%s\n%s\n%s\n" 
            (gpr.R0toR7().Dump()) 
            (gpr.R8toR15().Dump()) 
            (gpr.R16toR23().Dump()) 
            (gpr.R24toR31().Dump()))
            //.Trim()       

    static member GetInstance = instance

and FPR private () =

    static let instance = FPR()

    member fpr.Item
        with get i = 
            if i > 31 then failwith "invalid FPR index"
            else RegisterFile.GetInstance.[i + 32]
            
    member fpr.F0toF7() = RegisterSet("F0-F7", [| for i = 0 to 7 do yield fpr.[i] |])
    member fpr.F8toF15() = RegisterSet("F8-F15", [| for i = 8 to 15 do yield fpr.[i] |])
    member fpr.F16toF23() = RegisterSet("F16-F23", [| for i = 16 to 23 do yield fpr.[i] |])
    member fpr.F24toF31() = RegisterSet("F24-F31", [| for i = 24 to 31 do yield fpr.[i] |])
    
    member fpr.Dump() = 
        (sprintf "%s\n%s\n%s\n%s" 
            (fpr.F0toF7().Dump()) 
            (fpr.F8toF15().Dump()) 
            (fpr.F16toF23().Dump()) 
            (fpr.F24toF31().Dump()))
            .Trim()
    
    override fpr.ToString() =
        (sprintf "%O\n%O\n%O\n%O" (fpr.F0toF7()) (fpr.F8toF15()) (fpr.F16toF23()) (fpr.F24toF31()))
            .Trim()

    static member GetInstance = instance
    
and RegisterStat private (instruction:int) =
    static let instance i = RegisterStat i
    let reg s = Convert.int2bits2reg instruction s

    member rstat.Item
        with get (oreg:OperandReg) = oreg |> function
            | OperandReg.NONE -> Register.Init(0)
            | OperandReg.GPR s -> GPR.GetInstance.[reg s]
            | OperandReg.FPR s -> FPR.GetInstance.[reg s]

//    member rstat.Item
//        with get (i:int) = RegisterFile.GetInstance.[i]

    static member GetInstance = instance

and Regs private (instruction:int) =
    static let instance i = Regs i

    let reg s = Convert.int2bits2reg instruction s

    member regs.Item
        with get oreg = oreg |> function
            | OperandReg.NONE -> Register.Init(0).Contents
            | OperandReg.GPR s -> GPR.GetInstance.[reg s].Contents
            | OperandReg.FPR s -> FPR.GetInstance.[reg s].Contents

//    member regs.Item
//        with get i = RegisterFile.GetInstance.[i]

    static member GetInstance = instance
      
and Register =
    {
        mutable Qi          : string option
        mutable Contents    : int
    }

    member r.IsAvailable() = 
        r.Qi |> function | Some _ -> false | _ -> true

    member r.Update(cdb:CDB option) = (cdb, r.Qi) |> function
        | Some cdb, Some Qi -> if Qi = cdb.Src then r.Contents <- cdb.Result; r.Qi <- None
        | _ -> ()

    override r.ToString() = sprintf "%s" (Convert.int2hex r.Contents)
    
    static member Init _ = { Qi = None; Contents = 0 }
    static member ArrayInit n = Array.init n Register.Init


and RegisterSet(heading:string, regs:Register[]) =
    do if regs.Length <> 8 then failwith "register set must be length 8"
    member val Regs = regs with get, set

    member rs.Dump() =
        regs
        |> Array.map (sprintf "%O")
        |> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)

    override rs.ToString() =
//        regs
//        |> Array.map (sprintf "%O")
//        |> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)
        if RegisterFile.HasContent rs.Regs then
            rs.Regs
            |> Array.map (sprintf "%O")
            |> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)
        else ""


//type GPR private () =
//    static let instance = 
//        RegisterInfo.Create
//            RegisterStatus.InitGPR
//            RegisterFile.InitGPR
//
//    static member GetInstance = instance
//
//and FPR private () =
//    static let instance =
//        RegisterInfo.Create
//            RegisterStatus.InitFPR
//            RegisterFile.InitFPR
//
//and RegisterInfo =
//    {
//        RegisterStat : RegisterStatus
//        Regs : RegisterFile
//    }
//
//    override ri.ToString() =
//        let heading a b = ri.Regs |> function
//            | RegisterFile.GPR _ -> sprintf "R%dR%d" a b
//            | RegisterFile.FPR _ -> sprintf "F%dR%d" a b
//
//        let _0to7 = RegisterSet(heading 0 7, ri.RegisterStat._0to7(), ri.Regs._0to7())
//        let _8to15 = RegisterSet(heading 8 15, ri.RegisterStat._8to15(), ri.Regs._8to15())
//        let _16to23 = RegisterSet(heading 16 23, ri.RegisterStat._16to23(), ri.Regs._16to23())
//        let _24to31 = RegisterSet(heading 24 31, ri.RegisterStat._24to31(), ri.Regs._24to31())
//        sprintf "%O\n%O\n%O\n%O" _0to7 _8to15 _16to23 _24to31
//
//    static member Create registerStat regs = (registerStat, regs) |> function
//        | RegisterStatus.GPR _, RegisterFile.GPR _ -> { RegisterStat = registerStat; Regs = regs }
//        | RegisterStatus.FPR _, RegisterFile.FPR _ -> { RegisterStat = registerStat; Regs = regs }
//        | _ -> failwith "non matching register types"
//
//and RegisterFile = 
//    | GPR of int[]
//    | FPR of int[]
//
//    static member ApplyFunction (f:int[] -> 'T) = function
//        | GPR gpr -> f gpr
//        | FPR fpr -> f fpr
//
//    member rf.Item
//        with get i = rf |> function
//            | GPR gpr -> gpr.[i]
//            | FPR fpr -> fpr.[i]
//        and set i value = rf |> function
//            | GPR gpr -> gpr.[i] <- value
//            | FPR fpr -> fpr.[i] <- value
//
//    member rf.Length = rf |> RegisterFile.ApplyFunction (fun arr -> arr.Length)
//
//    member rf._0to7() = rf |> RegisterFile.ApplyFunction (fun arr -> arr.[0..7])
//    member rf._8to15() = rf |> RegisterFile.ApplyFunction (fun arr -> arr.[8..15])
//    member rf._16to23() = rf |> RegisterFile.ApplyFunction (fun arr -> arr.[16..23])
//    member rf._24to31() = rf |> RegisterFile.ApplyFunction (fun arr -> arr.[24..31])
//
//    static member InitGPR = Array.zeroCreate<int> 32 |> RegisterFile.GPR
//    static member InitFPR = Array.zeroCreate<int> 32 |> RegisterFile.FPR
//
//    static member HasContent = 
//        RegisterFile.ApplyFunction 
//            (fun arr -> arr |> Array.forall (fun x -> x = 0) |> not)
//
//
//and RegisterStatus =
//    | GPR of Qi[]
//    | FPR of Qi[]
//
//    static member ApplyFunction (f:Qi[] -> 'T) = function
//        | GPR gpr -> f gpr
//        | FPR fpr -> f fpr
//
//    member rs.Item
//        with get i = rs |> function
//            | GPR gpr -> gpr.[i]
//            | FPR fpr -> fpr.[i]
//        and set i value = rs |> function
//            | GPR gpr -> gpr.[i] <- value
//            | FPR fpr -> fpr.[i] <- value
//
//    member rf.Length = rf |> RegisterStatus.ApplyFunction (fun arr -> arr.Length)
//
//    member rs._0to7() = rs |> RegisterStatus.ApplyFunction (fun arr -> arr.[0..7])
//    member rs._8to15() = rs |> RegisterStatus.ApplyFunction (fun arr -> arr.[8..15])
//    member rs._16to23() = rs |> RegisterStatus.ApplyFunction (fun arr -> arr.[16..23])
//    member rs._24to31() = rs |> RegisterStatus.ApplyFunction (fun arr -> arr.[24..31])
//
//    static member InitGPR = 
//        Array.init 32 (fun _ -> None) |> RegisterStatus.GPR
//    
//    static member InitFPR =
//        Array.init 32 (fun _ -> None) |> RegisterStatus.FPR
//
//    static member HasContent = 
//        RegisterStatus.ApplyFunction 
//            (fun arr -> arr |> Array.forall (fun qi -> qi.IsNone) |> not)
//
//and Qi = string option
//
//
//and RegisterSet(heading:string, registerStat:Qi[], regs:int[]) =
//    do if regs.Length <> 8 then failwith "register set must be length 8"
//    
//    override rs.ToString() =
//        let choose (qi:Qi) (x:int) = if qi.IsSome then qi.Value else Convert.int2hex x
//        let haveContent (qi:Qi[]) (x:int[]) = (qi,x) ||> Array.forall2 (fun qi x -> qi.IsNone && x = 0) |> not
//        if (registerStat, regs) ||> haveContent then
//            (registerStat, regs)
//            ||> Array.map2 choose
//            |> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)
//        else ""





