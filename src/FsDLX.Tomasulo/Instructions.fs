namespace FsDLX.Tomasulo

open System
open System.Collections

type Instruction =
    | Integer of InstructionInt * InstructionInfo
    | Trap of InstructionInt * InstructionInfo
    | Branch of InstructionInt * InstructionInfo
    | Memory of InstructionInt * InstructionInfo
    | FloatingPoint of InstructionInt * InstructionInfo

    member ins.AsInfo = 
        match ins with
        | Integer(i,info)
        | Trap(i,info)
        | Branch(i,info)
        | Memory(i,info)
        | FloatingPoint(i,info) -> info

    member ins.AsInt =
        match ins with
        | Integer(i,info)
        | Trap(i,info)
        | Branch(i,info)
        | Memory(i,info)
        | FloatingPoint(i,info) -> i

    member ins.AsHex = ins.AsInt |> Convert.int2hex

    member ins.Opcode = let op,_,_,_,_,_ = ins.AsInfo in op
    member ins.FuncCode = let _,fc,_,_,_,_ = ins.AsInfo in fc
    member ins.DstReg = let _,_,rd,_,_,_ = ins.AsInfo in rd
    member ins.S1Reg = let _,_,_,rs,_,_ = ins.AsInfo in rs
    member ins.S2Reg = let _,_,_,_,rt,_ = ins.AsInfo in rt
    member ins.Immediate = let _,_,_,_,_,imm = ins.AsInfo in imm.GetImmVal ins.AsInt

    override ins.ToString() = sprintf "%s" ins.AsHex

    static member threeGpr opcode = (Opcode opcode, FuncCode.NONE, DstReg.GPR 16, S1Reg.GPR 6, S2Reg.GPR 11, Imm.NONE)
    static member threeFpr opcode = (Opcode opcode, FuncCode.NONE, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.FPR 11, Imm.NONE)

    // IntegerUnit instructions
    static member ADDI = (Opcode "addi", FuncCode.NONE, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static member NOP = (Opcode "nop", FuncCode.NONE, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.NONE)
    static member ADD = Instruction.threeGpr "add"
    static member SUB = Instruction.threeGpr "sub"
    static member AND = Instruction.threeGpr "and"
    static member OR = Instruction.threeGpr "or"
    static member XOR = Instruction.threeGpr "xor"
    static member MOVF = (Opcode "movf", FuncCode.NONE, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static member MOVFP2I = (Opcode "movfp2i", FuncCode.NONE, DstReg.GPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static member MOVI2FP = (Opcode "movi2fp", FuncCode.NONE, DstReg.FPR 16, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    
    // TrapUnit instructions
    static member HALT = (Opcode "halt", FuncCode.HALT, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static member DUMPGPR = (Opcode "dumpGPR", FuncCode.DUMPGPR, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static member DUMPFPR = (Opcode "dumpFPR", FuncCode.DUMPFPR, DstReg.NONE, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static member DUMPSTR = (Opcode "dumpSTR", FuncCode.DUMPSTR, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)

    // BranchUnit instructions
    static member BEQZ = (Opcode "beqz", FuncCode.NONE, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16, 31))
    static member J = (Opcode "j", FuncCode.NONE, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.A(6,31))
    static member JR = (Opcode "jr", FuncCode.NONE, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static member JAL = (Opcode "jal", FuncCode.NONE, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.A(6,31))
    static member JALR = (Opcode "jalr", FuncCode.NONE, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)

    // MemoryUnit isntructions
    static member LW = (Opcode "lw", FuncCode.NONE, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static member LF = (Opcode "lf", FuncCode.NONE, DstReg.FPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static member SW = (Opcode "sw", FuncCode.NONE, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static member SF = (Opcode "sf", FuncCode.NONE, DstReg.FPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))

    // FloatingPointUnit instructions
    static member ADDF = Instruction.threeFpr "addf"
    static member SUBF = Instruction.threeFpr "subf"
    static member MULTF = Instruction.threeFpr "multf"
    static member DIVF = Instruction.threeFpr "divf"
    static member MULT = Instruction.threeFpr "mult"
    static member DIV = Instruction.threeFpr "div"
    static member CVTF2I = (Opcode "cvtf2i", FuncCode.NONE, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static member CVTI2F = (Opcode "cvti2f", FuncCode.NONE, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)

    static member OfInstructionInt(i:int) =
        match Opcode.OfInstructionInt(i).Name with
        | "addi" -> Integer(i, Instruction.ADDI)
        | "nop" -> Integer(i, Instruction.NOP)
        | "add" -> Integer(i, Instruction.ADD)
        | "sub" -> Integer(i, Instruction.SUB)
        | "and" -> Integer(i, Instruction.AND)
        | "or" -> Integer(i, Instruction.OR)
        | "xor" -> Integer(i, Instruction.XOR)
        | "movf" -> Integer(i, Instruction.MOVF)
        | "movfp2i" -> Integer(i, Instruction.MOVFP2I)
        | "movi2fp" -> Integer(i, Instruction.MOVI2FP)

        | "halt" -> Trap(i, Instruction.HALT)
        | "dumpGPR"-> Trap(i, Instruction.DUMPGPR)
        | "dumpFPR" -> Trap(i, Instruction.DUMPFPR)
        | "dumpSTR" -> Trap(i, Instruction.DUMPSTR)
        
        | "beqz" -> Branch(i, Instruction.BEQZ)
        | "j" -> Branch(i, Instruction.J)
        | "jr" -> Branch(i, Instruction.JR)
        | "jal" -> Branch(i, Instruction.JAL)
        | "jalr" -> Branch(i, Instruction.JALR)
        
        | "lw" -> Memory(i, Instruction.LW)
        | "lf" -> Memory(i, Instruction.LF)
        | "sw" -> Memory(i, Instruction.SW)
        | "sf" -> Memory(i, Instruction.SF)
        
        | "addf" -> FloatingPoint(i, Instruction.ADDF)
        | "subf" -> FloatingPoint(i, Instruction.SUBF)
        | "multf" -> FloatingPoint(i, Instruction.MULTF)
        | "divf" -> FloatingPoint(i, Instruction.DIVF)
        | "mult" -> FloatingPoint(i, Instruction.MULT)
        | "div" -> FloatingPoint(i, Instruction.DIV)
        | "cvtf2i" -> FloatingPoint(i, Instruction.CVTF2I)
        | "cvti2f" -> FloatingPoint(i, Instruction.CVTI2F)
        
        | op -> failwith (sprintf "opcode <%s> not supported" op)