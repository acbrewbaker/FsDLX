namespace FsDLX.Tomasulo

[<AbstractClass>]
type RegisterFile() =
    let regs = Register.ArrayInit 64

    member rf.Item
        with get i = 
            if i > 31 then failwith "invalid index"
            else
                rf |> function
                | :? GeneralPurposeRegister as gpr -> gpr.[i]
                | :? FloatingPointRegister as fpr -> fpr.[i + 32]
                | _ -> failwith "invalid register file type"
        
        and set i value = 
            if i > 31 then failwith "invalid index"
            else
                rf |> function
                | :? GeneralPurposeRegister as gpr -> gpr.[i].Contents <- value
                | :? FloatingPointRegister as fpr -> fpr.[i + 32].Contents <- value
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

and GeneralPurposeRegister() =
    inherit RegisterFile()

    member gpr.Item
        with get i = 
            if i > 31 then failwith "invalid GPR index"
            else base.[i]
        
        and set i value  =
            if i > 31 then failwith "invalid GPR index"
            else base.[i].Contents <- value

and FloatingPointRegister() =
    inherit RegisterFile()

    member gpr.Item
        with get i = 
            if i > 31 then failwith "invalid FPR index"
            else base.[i + 32]
        
        and set i value =
            if i > 31 then failwith "invalid FPR index"
            else base.[i + 32].Contents <- value


