namespace FsDLX.Tomasulo

open System


type RegisterFile private () =
    static let mutable instance = RegisterFile()
    let regs = Array.init Config.Registers.RegCount Register.Init
    
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

    static member GetInstance = instance
    static member Reset() = instance <- GPR()

and FPR private () =
    static let mutable instance = FPR()

    member fpr.Item
        with get i = 
            if i > 31 then failwith "invalid FPR index"
            else RegisterFile.GetInstance.[i + 32]
    
    member fpr.GetSlice(a:int option, b:int option) =
        let a = match a with Some a -> a | _ -> 0
        let b = match b with Some b -> b | _ -> RegisterFile.GetInstance.Length - 1
        if (a>31) || (b>31) then failwith "invalid FPR index"
        else RegisterFile.GetInstance.GetSlice(a+32,b+32)

    static member GetInstance = instance
    static member Reset() = instance <- FPR()

and RegisterStat private (instruction:int) =
    static let mutable instance = fun i -> RegisterStat(i)
    let reg s = Convert.int2bits2reg instruction s

    member rstat.Item
        with get (oreg:OperandReg) = oreg |> function
            | OperandReg.NONE -> Register.Init(0)
            | OperandReg.GPR s -> GPR.GetInstance.[reg s]
            | OperandReg.FPR s -> FPR.GetInstance.[reg s]

    static member GetInstance = instance
    static member Reset() = instance <- fun i -> RegisterStat(i)

and Regs private (instruction:int) =
    static let mutable instance = fun i -> Regs(i)

    let reg s = Convert.int2bits2reg instruction s

    member regs.Item
        with get oreg = oreg |> function
            | OperandReg.NONE   -> Register.Init(0).Contents
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

    member r.Update(cdb:CDB option) = 
        match cdb, r.Qi with
        | Some cdb, Some Qi -> if Qi = cdb.Src then r.Contents <- cdb.Result; r.Qi <- None
        | _ -> ()

    override r.ToString() = (r.Qi, r.Contents) |> function
        | Some _, 0 -> sprintf "%s" (Convert.strOption2str r.Qi)
        | _ -> sprintf "%s" (Convert.int2hex r.Contents)
    
    static member Init _ = { Qi = None; Contents = 0 }
    static member ArrayInit n = Array.init n Register.Init