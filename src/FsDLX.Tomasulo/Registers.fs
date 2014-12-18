namespace FsDLX.Tomasulo

open FsDLX.Common


[<AbstractClass>]
type RegisterFile() =
    let regs = Array.init 64 Register.Init //Register.ArrayInit 64



    

    member rf.Item
        with    get i : Register = regs.[i]
        and     set i (value:int) = regs.[i].Contents <- value


    member rf.Update() =
        let cdb = CDB.GetInstance
        rf |> function
        | :? GPR -> regs.[0..31] |> Array.iter (fun reg ->
            if reg.Qi.IsSome && reg.Qi.Value = cdb.Src then
                reg.Contents <- cdb.Result.Value
                reg.Qi <- None)
        | :? FPR -> regs.[32..63] |> Array.iter (fun reg ->
            if reg.Qi.IsSome && reg.Qi.Value = cdb.Src then
                reg.Contents <- cdb.Result.Value
                reg.Qi <- None)
        | _ -> failwith ""

    static member HasContent (regs:Register[]) =
        not(regs |> Array.forall (fun reg -> reg.Contents = 0))

//    member rf.Item
//        with get i = 
//            if i > 31 then failwith "invalid index"
//            else
//                rf |> function
//                | :? GPR as gpr -> gpr.[i]
//                | :? FPR as fpr -> fpr.[i + 32]
//                | _ -> failwith "invalid register file type"
//        
//        and set i value = 
//            if i > 31 then failwith "invalid index"
//            else
//                rf |> function
//                | :? GPR as gpr -> gpr.[i].Contents <- value
//                | :? FPR as fpr -> fpr.[i + 32].Contents <- value
//                | _ -> failwith "invalid register file type"
//
//    member rf.Item
//        with get (i:string) =
//            if i.Length > 5 then failwith "binary string length must be <= 5"
//            let i = Convert.bin2int i
//            rf.Item(i)
//        and set (i:string) value =
//            if i.Length > 5 then failwith "binary string length must be <= 5"
//            let i = Convert.bin2int i
//            rf.Item(i) <- value

//    member rf.Item
//        with get (rd:DstReg) = rd |> function
//            | DstReg.GPR rd -> fun (instruction:int) -> rf.[regId instruction rd]
//            | DstReg.FPR rd -> fun (instruction:int) -> rf.[regId instruction rd]
//            | _ -> failwith ""
            



and Register =
    {
        mutable Qi          : Qi
        mutable Contents    : int
    }

    member r.IsAvailable() = 
        r.Qi |> function | Some _ -> false | _ -> true

    override r.ToString() = sprintf "%d" r.Contents

    static member Init _ = { Qi = None; Contents = 0 }
    static member ArrayInit n = Array.init n Register.Init


and Qi = string option

and GPR private () =
    inherit RegisterFile()

    static let instance = GPR()

    member gpr.Item
        with get i = 
            if i > 31 then failwith "invalid GPR index"
            else base.[i]
        
        and set i value  =
            if i > 31 then failwith "invalid GPR index"
            else base.[i].Contents <- value

    member gpr.R0toR7() = RegisterSet("R0-R7", [| for i = 0 to 7 do yield gpr.[i] |])
    member gpr.R8toR15() = RegisterSet("R8-R15", [| for i = 8 to 15 do yield gpr.[i] |])
    member gpr.R16toR23() = RegisterSet("R16-R23", [| for i = 16 to 23 do yield gpr.[i] |])
    member gpr.R24toR31() = RegisterSet("R24-R31", [| for i = 24 to 31 do yield gpr.[i] |])

    override gpr.ToString() =
        sprintf "%O\n%O\n%O\n%O" (gpr.R0toR7()) (gpr.R8toR15()) (gpr.R16toR23()) (gpr.R24toR31())
        
        
//        sprintf "R0-R7:    %s\nR8-R15:   %s\nR16-R23:  %s\nR24-R31:  %s"
//            ([for i = 0 to 7 do yield sprintf "%O, " (gpr.[i])] |> List.reduce (+))
//            ([for i = 8 to 15 do yield sprintf "%O, " (gpr.[i])] |> List.reduce (+))
//            ([for i = 16 to 23 do yield sprintf "%O, " (gpr.[i])] |> List.reduce (+))
//            ([for i = 24 to 31 do yield sprintf "%O, " (gpr.[i])] |> List.reduce (+))


    static member GetInstance = instance

and FPR private () =
    inherit RegisterFile()

    static let instance = FPR()

    member gpr.Item
        with get i = 
            if i > 31 then failwith "invalid FPR index"
            else base.[i + 32]
        
        and set i value =
            if i > 31 then failwith "invalid FPR index"
            else base.[i + 32].Contents <- value

    override gpr.ToString() =
        sprintf "F0-F7:    %s\nF8-F15:   %s\nF16-F23:  %s\nF24-F31:  %s"
            ([for i = 0 to 7 do yield sprintf "%O, " (gpr.[i])] |> List.reduce (+))
            ([for i = 8 to 15 do yield sprintf "%O, " (gpr.[i])] |> List.reduce (+))
            ([for i = 16 to 23 do yield sprintf "%O, " (gpr.[i])] |> List.reduce (+))
            ([for i = 24 to 31 do yield sprintf "%O, " (gpr.[i])] |> List.reduce (+))

    static member GetInstance = instance

and RegisterSet(heading:string, regs:Register[]) =
    do if regs.Length <> 8 then failwith "register set must be length 8"
    override rs.ToString() =
        if RegisterFile.HasContent regs then
            regs
            |> Array.map (fun reg -> 
                if reg.Qi.IsSome then sprintf "%s" reg.Qi.Value else sprintf "%s" (Convert.int2hex reg.Contents))
            |> Array.fold (fun s r -> s + " " + r) (sprintf "%s:" heading)
        else ""
//
//type RegisterInfo() =
//    member val GPR = GPR.GetInstance with get
//    member val FPR = FPR.GetInstance with get
//    member val RegisterStat = Array.init 64 (fun _ -> RegisterStatus("0"))

//
//type RegisterFile2 =
//    | GPR of int[]
//    | FPR of int[]


