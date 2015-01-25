namespace FsDLX.Tomasulo

open FsDLX.Common


type RegisterFile private () =
    static let instance = RegisterFile()
    let regs = Array.init Config.Registers.RegCount Register.Init //Register.ArrayInit 64

    let mutable info : string option = None

    member rf.Item
        with    get i = regs.[i]
        and     set i v = regs.[i].Contents <- v
    
    member rf.UpdateInfo() = info <- sprintf "%O%O" (GPR.GetInstance) (FPR.GetInstance) |> Some

    member rf.Update(cdb:CDB option) =
        regs |> Array.iter (fun reg -> reg.Update(cdb))
        match cdb with
        | Some _    -> rf.UpdateInfo()
        | None      -> info <- None

    override rf.ToString() =
        match info with
        | Some info -> info
        | None -> ""

    static member GetInstance = instance

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

    static member GetInstance = instance

and Regs private (instruction:int) =
    static let instance i = Regs i

    let reg s = Convert.int2bits2reg instruction s

    member regs.Item
        with get oreg = oreg |> function
            | OperandReg.NONE -> Register.Init(0).Contents
            | OperandReg.GPR s -> GPR.GetInstance.[reg s].Contents
            | OperandReg.FPR s -> FPR.GetInstance.[reg s].Contents

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

    override r.ToString() = (r.Qi, r.Contents) |> function
        | Some _, 0 -> sprintf "%s" (Convert.strOption2str r.Qi)
        | _ -> sprintf "%s" (Convert.int2hex r.Contents)
    
    static member Init _ = { Qi = None; Contents = 0 }
    static member ArrayInit n = Array.init n Register.Init

    static member HasContent (regs:Register[]) =
        regs |> Array.forall (fun r -> r.Contents = 0 && r.Qi.IsNone) |> not

and RegisterSet(heading:string, regs:Register[]) =
    do if regs.Length <> 8 then failwith "register set must be length 8"
    member val Regs = regs with get, set

    member rs.Dump() =
        regs
        |> Array.map (sprintf "%O")
        |> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)

    override rs.ToString() =
        if Register.HasContent rs.Regs then
            rs.Regs |> Array.map (sprintf "%O")
            |> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)
        else ""
