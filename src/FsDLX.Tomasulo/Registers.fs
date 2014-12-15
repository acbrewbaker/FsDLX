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

    static member GetInstance() = instance

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

    static member GetInstance() = instance

//
//type RegisterInfo() =
//    member val GPR = GPR.GetInstance with get
//    member val FPR = FPR.GetInstance with get
//    member val RegisterStat = Array.init 64 (fun _ -> RegisterStatus("0"))

//
//type RegisterFile2 =
//    | GPR of int[]
//    | FPR of int[]


