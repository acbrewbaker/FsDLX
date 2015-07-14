namespace FsDLX.Tomasulo

open System

type RegisterFile private () =
    static let mutable instance = RegisterFile()
    let regs = Array.init Config.Registers.RegCount Register.init
    
    member rf.Item
        with    get i = regs.[i]
        and     set i v = regs.[i].Contents <- v
    
    member rf.GetSlice(a:int option, b:int option) = 
        let a = match a with Some a -> a | _ -> 0
        let b = match b with Some b -> b | _ -> regs.Length - 1
        regs.[a..b]

    member rf.GetSlice(a:int, b:int) = rf.GetSlice(Some a, Some b)

    member rf.Length = regs.Length

    member rf.Update(cdb) = regs |> Array.iter (fun reg -> reg.Update(cdb))
    
    static member GetInstance = instance
    static member Reset() = instance <- RegisterFile()

and GPR private () =
    static let mutable instance = GPR()

    member gpr.Item
        with get i = 
            if i > 31 then failwith "invalid GPR index"
            else RegisterFile.GetInstance.[i]
      
    member gpr.GetSlice(a:int option, b:int option) =
        let a = match a with Some a -> a | _ -> 0
        let b = match b with Some b -> b | _ -> 31
        if (a>31) || (b>31) then failwith "invalid GPR index"
        else RegisterFile.GetInstance.GetSlice(a,b)

    member gpr.Regs() = [| for i = 0 to 31 do yield gpr.[i] |]

    member gpr.R0toR7() = RegisterSet("R0-R7", gpr.[0..7])
    member gpr.R8toR15() = RegisterSet("R8-R15", gpr.[8..15])
    member gpr.R16toR23() = RegisterSet("R16-R23", gpr.[16..23]) 
    member gpr.R24toR31() = RegisterSet("R24-R31", gpr.[24..31])


    override gpr.ToString() =
        (sprintf "%O\n%O\n%O\n%O" (gpr.R0toR7()) (gpr.R8toR15()) (gpr.R16toR23()) (gpr.R24toR31()))
            .Trim()

    member gpr.Dump() = 
        (sprintf "\n%s\n%s\n%s\n%s\n" 
            (gpr.R0toR7().Dump()) 
            (gpr.R8toR15().Dump()) 
            (gpr.R16toR23().Dump()) 
            (gpr.R24toR31().Dump()))

    static member GetInstance = instance
    static member Reset() = instance <- GPR()

and FPR private () =
    static let mutable instance = FPR()
    static let out heading = 
        Array.map (sprintf "%O") >> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)

    member fpr.Item
        with get i = 
            if i > 31 then failwith "invalid FPR index"
            else RegisterFile.GetInstance.[i + 32]
    
    member fpr.GetSlice(a:int option, b:int option) =
        let a = match a with Some a -> a | _ -> 0
        let b = match b with Some b -> b | _ -> RegisterFile.GetInstance.Length - 1
        if (a>31) || (b>31) then failwith "invalid FPR index"
        else RegisterFile.GetInstance.GetSlice(a+32,b+32)
    
    member fpr.F0toF7() = RegisterSet("F0-F7", fpr.[0..7])
    member fpr.F8toF15() = RegisterSet("F8-F15", fpr.[8..15])
    member fpr.F16toF23() = RegisterSet("F16-F23", fpr.[16..23])
    member fpr.F24toF31() = RegisterSet("F24-F31", fpr.[24..31])
    
    member fpr.Dump() = 
        (sprintf "%s\n%s\n%s\n%s" 
            (fpr.F0toF7().Dump()) 
            (fpr.F8toF15().Dump()) 
            (fpr.F16toF23().Dump()) 
            (fpr.F24toF31().Dump()))
    
    override fpr.ToString() =
        (sprintf "%O\n%O\n%O\n%O" (fpr.F0toF7()) (fpr.F8toF15()) (fpr.F16toF23()) (fpr.F24toF31()))
            .Trim()

    static member GetInstance = instance
    static member Reset() = instance <- FPR()
    

and RegisterStat private (instruction:int) =
    static let mutable instance = fun i -> RegisterStat(i)
    let reg s = Convert.int2bits2reg instruction s

    member rstat.Item
        with get (oreg:OperandReg) = oreg |> function
            | OperandReg.NONE -> Register.init(0)
            | OperandReg.GPR s -> GPR.GetInstance.[reg s]
            | OperandReg.FPR s -> FPR.GetInstance.[reg s]

    static member GetInstance = instance
    static member Reset() = instance <- fun i -> RegisterStat(i)

and Regs private (instruction:int) =
    static let mutable instance = fun i -> Regs(i)

    let reg s = Convert.int2bits2reg instruction s

    member regs.Item
        with get oreg = oreg |> function
            | OperandReg.NONE   -> Register.init(0).Contents
            | OperandReg.GPR s  -> GPR.GetInstance.[reg s].Contents
            | OperandReg.FPR s  -> FPR.GetInstance.[reg s].Contents

    static member GetInstance = instance
    static member Reset() = instance <- fun i -> Regs(i)
      
and Register =
    {
        mutable Qi          : string option
        mutable Contents    : int
    }

    member r.IsAvailable() = 
        r.Qi |> function | Some _ -> false | _ -> true

    member r.IsEmpty() = r.Contents = 0 && r.Qi.IsNone

    member r.Update(cdb:CDB option) = 
        match cdb, r.Qi with
        | Some cdb, Some Qi -> if Qi = cdb.Src then r.Contents <- cdb.Result; r.Qi <- None
        | _ -> ()

    override r.ToString() = (r.Qi, r.Contents) |> function
        | Some _, 0 -> sprintf "%s" (Convert.strOption2str r.Qi)
        | _ -> sprintf "%s" (Convert.int2hex r.Contents)
    
    static member init _ = { Qi = None; Contents = 0 }
    
and RegisterSet(heading:string, regs:Register[]) =
    do if regs.Length <> 8 then failwith "register set must be length 8"
    
    member rs.Dump() =
        regs
        |> Array.map (sprintf "%O")
        |> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)

    override rs.ToString() =
        if regs |> Array.forall (fun r -> r.IsEmpty()) |> not then
            regs |> Array.map (sprintf "%O")
            |> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)
        else ""