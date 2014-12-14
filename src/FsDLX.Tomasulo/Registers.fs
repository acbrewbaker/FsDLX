namespace FsDLX.Tomasulo

[<AbstractClass>]
type RegisterFile() =
    member val Regs = Register.ArrayInit 64

    member rf.Item
        with get i = 
            if i > 31 then failwith "invalid index"
            else
                rf |> function
                | :? GPR as gpr -> gpr.[i]
                | :? FPR as fpr -> fpr.[i + 32]
                | _ -> failwith "invalid register file type"
        
        and set i value = 
            if i > 31 then failwith "invalid index"
            else
                rf |> function
                | :? GPR as gpr -> gpr.[i].Contents <- value
                | :? FPR as fpr -> fpr.[i + 32].Contents <- value
                | _ -> failwith "invalid register file type"

and Register =
    {
        mutable Qi          : Qi
        mutable Contents    : int    
    }

    member r.IsAvailable() = r.Qi |> function | Some _ -> false | _ -> true

    static member Init _ = { Qi = None; Contents = 0 }
    static member ArrayInit n = Array.init n Register.Init

and Qi = string option

and GPR() =
    inherit RegisterFile()

    member gpr.Item
        with get i = 
            if i > 31 then failwith "invalid GPR index"
            else gpr.Regs.[i]
        
        and set i value  =
            if i > 31 then failwith "invalid GPR index"
            else gpr.Regs.[i].Contents <- value

and FPR() =
    inherit RegisterFile()

    member gpr.Item
        with get i = 
            if i > 31 then failwith "invalid FPR index"
            else gpr.Regs.[i + 32]
        
        and set i value =
            if i > 31 then failwith "invalid FPR index"
            else gpr.Regs.[i + 32].Contents <- value


