module FsDLX.Assembler.Encoding

open System



type T =
    | Register of string
    | Opcode of string
    | Func5 of string
    | Func6 of string
    | Immediate of string
    | RRX of string

    member private t.register (s:string) =
        Convert.ToString(s.Substring(1) |> int, 2)
    
    member private t.opcode (s:string) = 
        Convert.ToString(int s, 2).PadLeft(6, '0')

    member private t.func5 (s:string) =
        Convert.ToString(int s, 2).PadLeft(5, '0')

    member private t.func6 (s:string) = t.opcode(s)

    member private t.immediate (s:string) = ""

    member private t.rrx (s:string) = 
        Convert.ToString(int s, 2).PadLeft(6, '0')
    
    member private t.itype (s:string) =
        sprintf "%s%s%s"

    override t.ToString() = t |> function
        | Register s    -> t.register s
        | Opcode s      -> t.opcode s
        | Func5 s       -> t.func5 s
        | Func6 s       -> t.func6 s
        | Immediate s   -> t.immediate s
        | RRX s         -> t.rrx s



//
//let itype opcode rs1 rd immediate = 
//    sprintf "%s%s%s%s" 
//        (opcode opcode)
//        (reg rs1)
//        (reg rd) 
//        (immediate immediate)
//
//let rtype rrx rs1 rs2 rd unused func = 
//    sprintf "%s%s%s%s%s%s" 
//        rrx rs1 rs2 rd unused func
//
//let jtype opcode name =
//    sprintf "%s%s" opcode name
//


