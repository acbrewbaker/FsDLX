namespace FsDLX.Tomasulo

open System.Collections
open FsDLX.Common

type Instruction =
    | Integer of InstructionInt * InstructionInfo
    | Trap of InstructionInt * InstructionInfo
    | Branch of InstructionInt * InstructionInfo
    | Memory of InstructionInt * InstructionInfo
    | FloatingPoint of InstructionInt * InstructionInfo

    member ins.AsInt = ins |> function
        | Integer(i,info) -> i
        | Trap(i,info) -> i
        | Branch(i,info) -> i
        | Memory(i,info) -> i
        | FloatingPoint(i,info) -> i

    member ins.AsHex = ins.AsInt |> Convert.int2hex

    member ins.AsInfo = ins |> function
        | Integer(i,info) -> info
        | Trap(i,info) -> info
        | Branch(i,info) -> info
        | Memory(i,info) -> info
        | FloatingPoint(i,info) -> info

    member ins.Opcode = let op,_,_,_,_,_ = ins.AsInfo in op
    member ins.FuncCode = let _,fc,_,_,_,_ = ins.AsInfo in fc
    member ins.DstReg = let _,_,rd,_,_,_ = ins.AsInfo in rd
    member ins.S1Reg = let _,_,_,rs,_,_ = ins.AsInfo in rs
    member ins.S2Reg = let _,_,_,_,rt,_ = ins.AsInfo in rt
    member ins.Immediate = let _,_,_,_,_,imm = ins.AsInfo in imm.GetImmVal ins

    override ins.ToString() = sprintf "%s" ins.AsHex

    static member threeGpr opcode = (Opcode.OfName opcode, FuncCode.NONE, DstReg.GPR 16, S1Reg.GPR 6, S2Reg.GPR 11, Imm.NONE)
    static member threeFpr opcode = (Opcode.OfName opcode, FuncCode.NONE, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.FPR 11, Imm.NONE)

    // IntegerUnit instructions
    static member ADDI = (Opcode.OfName "addi", FuncCode.NONE, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static member NOP = (Opcode.OfName "nop", FuncCode.NONE, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.NONE)
    static member ADD = Instruction.threeGpr "add"
    static member SUB = Instruction.threeGpr "sub"
    static member AND = Instruction.threeGpr "and"
    static member OR = Instruction.threeGpr "or"
    static member XOR = Instruction.threeGpr "xor"
    static member MOVF = (Opcode.OfName "movf", FuncCode.NONE, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static member MOVFP2I = (Opcode.OfName "movfp2i", FuncCode.NONE, DstReg.GPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static member MOVI2FP = (Opcode.OfName "movi2fp", FuncCode.NONE, DstReg.FPR 16, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    
    // TrapUnit instructions
    static member HALT = (Opcode.OfName "trap", FuncCode.HALT, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static member DUMPGPR = (Opcode.OfName "trap", FuncCode.DUMPGPR, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static member DUMPFPR = (Opcode.OfName "trap", FuncCode.DUMPFPR, DstReg.NONE, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static member DUMPSTR = (Opcode.OfName "trap", FuncCode.DUMPSTR, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)

    // BranchUnit instructions
    static member BEQZ = (Opcode.OfName "beqz", FuncCode.NONE, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16, 31))
    static member J = (Opcode.OfName "j", FuncCode.NONE, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.A(6,31))
    static member JR = (Opcode.OfName "jr", FuncCode.NONE, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static member JAL = (Opcode.OfName "jal", FuncCode.NONE, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.A(6,31))
    static member JALR = (Opcode.OfName "jalr", FuncCode.NONE, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)

    // MemoryUnit isntructions
    static member LW = (Opcode.OfName "lw", FuncCode.NONE, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static member LF = (Opcode.OfName "lf", FuncCode.NONE, DstReg.FPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static member SW = (Opcode.OfName "sw", FuncCode.NONE, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static member SF = (Opcode.OfName "sf", FuncCode.NONE, DstReg.FPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))

    // FloatingPointUnit instructions
    static member ADDF = Instruction.threeFpr "addf"
    static member SUBF = Instruction.threeFpr "subf"
    static member MULTF = Instruction.threeFpr "multf"
    static member DIVF = Instruction.threeFpr "divf"
    static member MULT = Instruction.threeFpr "mult"
    static member DIV = Instruction.threeFpr "div"
    static member CVTF2I = (Opcode.OfName "cvtf2i", FuncCode.NONE, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static member CVTI2F = (Opcode.OfName "cvti2f", FuncCode.NONE, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)

    static member OfInstructionInt(i:int) =
//        printfn "OfInstructionInt: %A\n" i
  //      printfn "hex ==> %A" (Convert.int2hex i)
        let opcode = Opcode.OfInstructionInt i
        //printfn "Opcode ===> %O" opcode
        //printfn "Opcode Name ===> %A" opcode.Name
        let funcCode = 
            if      opcode.Name = "trap" 
            then    Convert.int2bits2int i 27 31
            else    -1
        (opcode.Name, funcCode) |> function
        | "addi", _ -> Instruction.Integer(i, Instruction.ADDI)
        | "nop", _ -> Instruction.Integer(i, Instruction.NOP)
        | "add", _ -> Instruction.Integer(i, Instruction.ADD)
        | "sub", _ -> Instruction.Integer(i, Instruction.SUB)
        | "and", _ -> Instruction.Integer(i, Instruction.AND)
        | "or", _ -> Instruction.Integer(i, Instruction.OR)
        | "xor", _ -> Instruction.Integer(i, Instruction.XOR)
        | "movf", _ -> Instruction.Integer(i, Instruction.MOVF)
        | "movfp2i", _ -> Instruction.Integer(i, Instruction.MOVFP2I)
        | "movi2fp", _ -> Instruction.Integer(i, Instruction.MOVI2FP)

        | "trap", 0 -> Instruction.Trap(i, Instruction.HALT)
        | "trap", 1 -> Instruction.Trap(i, Instruction.DUMPGPR)
        | "trap", 2 -> Instruction.Trap(i, Instruction.DUMPFPR)
        | "trap", 3 -> Instruction.Trap(i, Instruction.DUMPSTR)
        
        | "beqz", _ -> Instruction.Branch(i, Instruction.BEQZ)
        | "j", _ -> Instruction.Branch(i, Instruction.J)
        | "jr", _ -> Instruction.Branch(i, Instruction.JR)
        | "jal", _ -> Instruction.Branch(i, Instruction.JAL)
        | "jalr", _ -> Instruction.Branch(i, Instruction.JALR)
        
        | "lw", _ -> Instruction.Memory(i, Instruction.LW)
        | "lf", _ -> Instruction.Memory(i, Instruction.LF)
        | "sw", _ -> Instruction.Memory(i, Instruction.SW)
        | "sf", _ -> Instruction.Memory(i, Instruction.SF)
        
        | "addf", _ -> Instruction.FloatingPoint(i, Instruction.ADDF)
        | "subf", _ -> Instruction.FloatingPoint(i, Instruction.SUBF)
        | "multf", _ -> Instruction.FloatingPoint(i, Instruction.MULTF)
        | "divf", _ -> Instruction.FloatingPoint(i, Instruction.DIVF)
        | "mult", _ -> Instruction.FloatingPoint(i, Instruction.MULT)
        | "div", _ -> Instruction.FloatingPoint(i, Instruction.DIV)
        | "cvtf2i", _ -> Instruction.FloatingPoint(i, Instruction.CVTF2I)
        | "cvti2f", _ -> Instruction.FloatingPoint(i, Instruction.CVTI2F)
        
        | op, _ -> Instruction.Trap(Convert.hex2int "44000000", Instruction.HALT)//failwith (sprintf "opcode <%s> not supported" op)

    //static member _HALT_ = Instruction.Trap(Convert.hex2int "44000000", Instruction.HALT)

and InstructionInt = int
and InstructionInfo = Opcode * FuncCode * DstReg * S1Reg * S2Reg * Imm
and FuncCode =
    | NONE = -1
    | HALT = 0
    | DUMPGPR = 1
    | DUMPFPR = 2
    | DUMPSTR = 3
and DstReg  = OperandReg
and S1Reg   = OperandReg
and S2Reg   = OperandReg
and Imm     = | NONE  | A of (int * int) with
    member this.GetImmVal(i:Instruction) =
        match this with
        | NONE -> None
        | A imm -> imm ||> Convert.int2bits2int (i.AsInt) |> Some