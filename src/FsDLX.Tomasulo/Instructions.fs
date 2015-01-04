//module FsDLX.Tomasulo.Instructions
namespace FsDLX.Tomasulo

open System.Collections
open FsDLX.Common


// In addition, it is helpful to create an Instruction class that contains information 
// that can be used to issue an instruction to a reservation station and set the Qi field 
// of a register in the register file.  The Vj (Qj) and Vk (Qk) fields are initialized via 
// source registers in the instruction.  An instruction may not have a source register.  
// If it does have a source register then it might refer to a general purpose register 
// (r0-r31) or a floating point register (f0-f31).   Similarly the A field is initialized 
// by the immediate value in an instruction.  An instruction may have an immediate value 
// or may not, and the size and starting bit of the immediate value vary depending upon the 
// instruction. The fields of an instruction object can be accessed to get this information 
// and used to initialize the reservation station.  In this way, it is possible to write 
// issuing code that can be placed in the FUContainer class and used to issue any instruction.  For example, the fields of the Instruction class could be:

type Instruction(i:int) =
    let info : InstructionInfo = InstructionInfo.ofInstructionInt i
    let opcode = info.opcode

    let reg = function
        | OperandReg.NONE -> None
        | OperandReg.GPR s -> GPR.GetInstance.[Convert.int2bits2reg i s] |> Some
        | OperandReg.FPR s -> FPR.GetInstance.[Convert.int2bits2reg i s] |> Some

    let immval = function
        | Imm.NONE -> None
        | Imm.A imm -> imm ||> Convert.int2bits2int i |> Some

    member val Int = i with get
    member val Info = info with get
    member val rd = info.rd //|> reg
    member val rs = info.rs //|> reg
    member val rt = info.rt //|> reg
    member val imm = info.imm //|> immval

    new(hex:string) = Instruction(Convert.hex2int hex)


and InstructionInfo(kind:InstructionKind, opcode:string, funCode:int, rd:DstReg, rs:S1Reg, rt:S2Reg, imm:Imm) =
    
    static let threeGpr kind opcode = InstructionInfo(kind, opcode, 0, DstReg.GPR 16, S1Reg.GPR 6, S2Reg.GPR 11, Imm.NONE)
    static let threeFpr kind opcode = InstructionInfo(kind, opcode, 0, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.FPR 11, Imm.NONE)

    // IntegerUnit instructions
    static let ADDI = InstructionInfo(Integer, "addi", 0, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static let NOP = InstructionInfo(Integer, "nop", 0, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.NONE)
    static let ADD = threeGpr Integer "add"
    static let SUB = threeGpr Integer "sub"
    static let AND = threeGpr Integer "and"
    static let OR = threeGpr Integer "or"
    static let XOR = threeGpr Integer "xor"
    static let MOVF = InstructionInfo(Integer, "movf", 0, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static let MOVFP2I = InstructionInfo(Integer, "movfp2i", 0, DstReg.GPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static let MOVI2FP = InstructionInfo(Integer, "movi2fp", 0, DstReg.FPR 16, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    
    // TrapUnit instructions
    static let HALT = InstructionInfo(Trap, "trap", 0, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static let DUMPGPR = InstructionInfo(Trap, "trap", 1, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static let DUMPFPR = InstructionInfo(Trap, "trap", 2, DstReg.NONE, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static let DUMPSTR = InstructionInfo(Trap, "trap", 3, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)

    // BranchUnit instructions
    static let BEQZ = InstructionInfo(Branch, "beqz", 0, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16, 31))
    static let J = InstructionInfo(Branch, "j", 0, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.A(6,31))
    static let JR = InstructionInfo(Branch, "jr", 0, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)
    static let JAL = InstructionInfo(Branch, "jal", 0, DstReg.NONE, S1Reg.NONE, S2Reg.NONE, Imm.A(6,31))
    static let JALR = InstructionInfo(Branch, "jalr", 0, DstReg.NONE, S1Reg.GPR 6, S2Reg.NONE, Imm.NONE)

    // MemoryUnit isntructions
    static let LW = InstructionInfo(Memory, "lw", 0, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static let LF = InstructionInfo(Memory, "lf", 0, DstReg.FPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static let SW = InstructionInfo(Memory, "sw", 0, DstReg.GPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))
    static let SF = InstructionInfo(Memory, "sf", 0, DstReg.FPR 11, S1Reg.GPR 6, S2Reg.NONE, Imm.A(16,31))

    // FloatingPointUnit instructions
    static let ADDF = threeFpr FloatingPoint "addf"
    static let SUBF = threeFpr FloatingPoint "subf"
    static let MULTF = threeFpr FloatingPoint "multf"
    static let DIVF = threeFpr FloatingPoint "divf"
    static let MULT = threeFpr FloatingPoint "mult"
    static let DIV = threeFpr FloatingPoint "div"
    static let CVTF2I = InstructionInfo(FloatingPoint, "cvtf2i", 0, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)
    static let CVTI2F = InstructionInfo(FloatingPoint, "cvti2f", 0, DstReg.FPR 16, S1Reg.FPR 6, S2Reg.NONE, Imm.NONE)

    member val kind = kind with get
    member val opcode = Opcode.ofName opcode with get
    member val funCode = funCode with get
    member val rd : DstReg = rd with get
    member val rs : S1Reg = rs with get
    member val rt : S2Reg = rt with get
    member val imm : Imm = imm with get

    static member ofInstructionInt(i:int) =
        let opcode = Opcode.ofInstructionInt i
        let funcCode = 
            if      opcode.Name = "trap" 
            then    Convert.int2bits2int i 27 31
            else    -1
        (opcode.Name, funcCode) |> function
        | "addi", _ -> ADDI
        | "nop", _ -> NOP
        | "add", _ -> ADD
        | "sub", _ -> SUB
        | "and", _ -> AND
        | "or", _ -> OR
        | "xor", _ -> XOR
        | "movf", _ -> MOVF
        | "movfp2i", _ -> MOVFP2I
        | "movi2fp", _ -> MOVI2FP

        | "trap", 0 -> HALT
        | "trap", 1 -> DUMPGPR
        | "trap", 2 -> DUMPFPR
        | "trap", 3 -> DUMPSTR
        
        | "beqz", _ -> BEQZ
        | "j", _ -> J
        | "jr", _ -> JR
        | "jal", _ -> JAL
        | "jalr", _ -> JALR
        
        | "lw", _ -> LW
        | "lf", _ -> LF
        | "sw", _ -> SW
        | "sf", _ -> SF
        
        | "addf", _ -> ADDF
        | "subf", _ -> SUBF
        | "multf", _ -> MULTF
        | "divf", _ -> DIVF
        | "mult", _ -> MULT
        | "div", _ -> DIV
        | "cvtf2i", _ -> CVTF2I
        | "cvti2f", _ -> CVTI2F
        
        | op, _ -> failwith (sprintf "opcode <%s> not supported" op)

and InstructionKind = | Integer | Trap | Branch | Memory | FloatingPoint
and OperandReg = | NONE | GPR of int | FPR of int
and DstReg  = OperandReg
and S1Reg   = OperandReg //| NONE | GPR of int | FPR of int
and S2Reg   = OperandReg //| NONE | GPR of int | FPR of int
and Imm     = | NONE  | A of (int * int)


//    let (|ADDI|_|) = function
//        | DstReg.GPR rd, S1Reg.GPR rs, S2Reg.NONE, Imm.A imm ->
//            fun i ->
//                let rd, rs, imm = reg i rd, reg i rs, imm i imm
//                let Regs(i) = GPR.GetInstance.[i]
//                let RegisterStat(i) = GPR.GetInstance.[i].Qi

//    static let ofHex hex =
//        let opcode = Opcode.ofInstructionHex hex
////        printfn "hex: %A, OPCODE: %A" hex (opcode.Name)
//        let iOps, tOps =
//            Config.FunctionalUnit.IntegerUnit.instructions,
//            Config.FunctionalUnit.TrapUnit.instructions
////        let iOps, tOps, bOps, mOps, fpOps = 
////            Config.FU.IntegerUnit.Instructions,
////            Config.FU.TrapUnit.Instructions,
////            Config.FU.BranchUnit.Instructions,
////            Config.FU.MemoryUnit.Instructions,
////            Config.FU.FloatingPointUnit.Instructions
//        let foundOpcodeIn ops = 
//            (ops |> Array.tryFind (fun o -> 
//                //printfn "o, opcode.Name ==> %A, %A" o (opcode.Name)
//                o = opcode.Name)).IsSome
////        printfn "found in %s iOPs: %A" (opcode.Name) (foundOpcodeIn iOps)
////        printfn "found in %s tOPs: %A" (opcode.Name) (foundOpcodeIn tOps)
//        if      foundOpcodeIn iOps  then Integer
//        elif    foundOpcodeIn tOps  then Trap
//        elif ["halt"; "dumpgpr"; "dumpfpr"; "dumpstr"] |> List.exists (fun op -> op = opcode.Name) then Trap
////        elif    foundOpcodeIn bOps  then Branch
////        elif    foundOpcodeIn mOps  then Memory
////        elif    foundOpcodeIn fpOps then FloatingPoint
//        else failwith (sprintf "opcode (%s) not supported" (opcode.Name)) 
//    
//    static let ofInt i = InstructionKind.ofHex (Convert.int2hex i)

//type DstReg =
//    | NONE
//    | R of int * int
//    | F of int * int

//    member rd.Get (instruction:int) (regs:RegisterFile) =
//        let idx a b = (Convert.int2bin instruction).[a..b] |> Convert.bin2int
//        (rd, regs) |> function
//        | R(a,b), :? GPR -> idx a b
//        | F(a,b), :? FPR -> idx a b
//        | _ -> failwith "invalid rd or register file"

//type S1Reg =
//    | NONE
//    | R of int * int
//    | F of int * int

//    member rs.Get (instruction:int) (regs:RegisterFile) =
//        let idx a b = (Convert.int2bin instruction).[a..b] |> Convert.bin2int
//        (rs, regs) |> function
//        | R(a,b), :? GPR -> idx a b
//        | F(a,b), :? FPR -> idx a b
//        | _ -> failwith "invalid rs or register file"


//type S2Reg =
//    | NONE
//    | R of int * int
//    | F of int * int
//
//    member rt.Get (instruction:int) (regs:RegisterFile) =
//        let idx a b = (Convert.int2bin instruction).[a..b] |> Convert.bin2int
//        (rt, regs) |> function
//        | R(a,b), :? GPR -> idx a b
//        | F(a,b), :? FPR -> idx a b
//        | _ -> failwith "invalid rt or register file"
//
//type FunCode =
//    | NONE
//    | FC of int * int
//
//    member fc.Get (instruction:int) =
//        let idx a b = (Convert.int2bin instruction).[a..b] |> Convert.bin2int
//        fc |> function
//        | FC(a,b) -> idx a b
//        | _ -> failwith "invalid func code"

//type Imm =
//    | NONE
//    | A of int * int
//
//    member imm.Get (instruction:int) =
//        let idx a b = (Convert.int2bin instruction).[a..b] |> Convert.bin2int
//        imm |> function
//        | A(a,b) -> idx a b
//        | _ -> failwith "invalid immediate"
//module Patterns = 
//    type FunctionalUnitInstructionSet = Map<string, Instruction>
//    
//    let m = [ "addi", Instruction.ADDI ] |> Map.ofList
//
//    let (|Integer1|Trap1|) (opcode:Opcode) = 
//        let iOps, tOps =
//            Config.FunctionalUnit.IntegerUnit.instructions,
//            Config.FunctionalUnit.TrapUnit.instructions
//
//        let instruction = m.[opcode.Name]
//        let ret = 
//            let i = m.[opcode.Name]
//            i.rd, i.rs, i.rt, i.imm
//
//        let foundOpcodeIn ops = 
//            (ops |> Array.tryFind (fun o -> o = opcode.Name)).IsSome
//        if      foundOpcodeIn iOps  
//        then    Integer1 ret
//        
//        elif    foundOpcodeIn tOps  
//        then    Trap1 ret
//        
//        //elif ["halt"; "dumpgpr"; "dumpfpr"; "dumpstr"] |> List.exists (fun op -> op = opcode.Name) then Trap
//        else failwith (sprintf "opcode (%s) not supported" (opcode.Name)) 
//        
//    
////    let (|A|B|) (opcode:Opcode) =
//
//    let (|Integer2|_|) = function
//        | DstReg.GPR rd, S1Reg.GPR rs, S2Reg.NONE, Imm.A(a,b) -> Some(rd,rs,-1,(a,b))
//        | DstReg.GPR rd, S1Reg.GPR rs, S2Reg.GPR rt, Imm.NONE -> Some(rd,rs,rt,(-1,-1))
//        | DstReg.FPR rd, S1Reg.FPR rs, S2Reg.NONE, Imm.NONE -> Some(rd,rs,-1,(-1,-1))
//        | DstReg.GPR rd, S1Reg.FPR rs, S2Reg.NONE, Imm.NONE -> Some(rd,rs,-1,(-1,-1))
//        | DstReg.FPR rd, S1Reg.GPR rs, S2Reg.NONE, Imm.NONE -> Some(rd,rs,-1,(-1,-1))
//        | _ -> None
//
//    let (|Trap2|_|) = function
//        | DstReg.GPR rd, S1Reg.GPR rs, S2Reg.NONE, Imm.A(a,b) -> Some(rd,rs,-1,(a,b))
//        | _ -> None
//
//    let (|Derp|_|) = function
//        | Integer1 instruction -> match instruction with | Integer2 i -> Some(i) | _ -> None
//        | Trap1 instruction -> match instruction with | Trap2 i -> Some(i) | _ -> None

////module ISA =
////    let lookup = 
////        [   "addi",     Instruction.ADDI
////            "nop",      Instruction.NOP
////            "add",      Instruction.ADD
////            "sub",      Instruction.SUB
////            "and",      Instruction.AND
////            "or",       Instruction.OR
////            "xor",      Instruction.XOR
////            "movf",     Instruction.MOVF
////            "movfp2i",  Instruction.MOVFP2I
////            "movi2fp",  Instruction.MOVI2FP
////            
////            "trap0",    Instruction.TRAP0
////            "trap1",    Instruction.TRAP1
////            "trap2",    Instruction.TRAP2
////            "trap3",    Instruction.TRAP3
////            
////            "beqz",     Instruction.BEQZ
////            "j",        Instruction.J
////            "jr",       Instruction.JR
////            "jal",      Instruction.JAL
////            "jalr",     Instruction.JALR
////
////            "lw",       Instruction.LW
////            "lf",       Instruction.LF
////            "sw",       Instruction.SW
////            "sf",       Instruction.SF
////
////            "addf",     Instruction.ADDF
////            "subf",     Instruction.SUBF
////            "multf",    Instruction.MULTF
////            "divf",     Instruction.DIVF
////            "mult",     Instruction.MULT
////            "div",      Instruction.DIV
////            "cvtf2i",   Instruction.CVTF2I
////            "cvti2f",   Instruction.CVTI2F  ] |> Map.ofList
////
////
////
////    member ins.GetRs (instruction:int) (gpr:GeneralPurposeRegister) = 
////        ins.rs |> function
////        | S1Reg.GPR b -> let rs = (Convert.int2bin instruction).[b..b+4] in gpr.[Convert.bin2int rs]
////    
////
////module ISA =
////    let addi = InstructionInfo("addi", DstReg.GPR 11, S1Reg.GPR 6, Imm.A(16,31))
////    let add = InstructionInfo("add", DstReg.GPR 16, S1Reg.GPR 6, S2Reg.GPR 11)
////    let trap0 = InstructionInfo(0, S1Reg.GPR 6)
////    let trap1 = InstructionInfo(1, S1Reg.GPR 6)
////    let trap2 = InstructionInfo(2, S1Reg.FPR 6)
////    let trap3 = InstructionInfo(3, S1Reg.GPR 6)
////    
////
////    member ins.ApplyToReservationStation (i:int) (regs:RegisterFile) (r:ReservationStation) =
////        ins.rs |> function | DstReg.NONE -> r.
////    
////    member ins.GetRD (i:int) (regs:RegisterFile) = 
////        let idx a b = (Convert.int2bin i).[a..b] |> Convert.bin2int
////        (ins.rd, regs) |> function
////        | DstReg.R(a,b), :? GPR -> idx a b
////        | DstReg.F(a,b), :? FPR -> idx a b
////        | _ -> failwith "invalid rd or register file"
////
////    member ins.GetRS (i:int) (regs:RegisterFile) = 
////        let idx a b = (Convert.int2bin i).[a..b] |> Convert.bin2int
////        (ins.rs, regs) |> function
////        | S1Reg.R(a,b), :? GPR -> idx a b
////        | S1Reg.F(a,b), :? FPR -> idx a b
////        | _ -> failwith "invalid rs or register file"
////
////    member ins.GetRT (i:int) (regs:RegisterFile) = 
////        let idx a b = (Convert.int2bin i).[a..b] |> Convert.bin2int
////        (ins.rt, regs) |> function
////        | S2Reg.R(a,b), :? GPR -> idx a b
////        | S2Reg.F(a,b), :? FPR -> idx a b
////        | _ -> failwith "invalid rt or register file"
////
////    static let RS (i:Instruction) (regs:RegisterFile) =
////        (i.rs, regs) |> function
////        | S1Reg.R (a,b), :? GPR as gpr -> 
////
////    static let InitTrap rs =
////        {   opcode = Opcode.ofName "trap"; funCode = FunCode.FC(27,31)
////            rd = DstReg.NONE; rs = rs; rt = S2Reg.NONE; imm = Imm.NONE }
////
////
////    static let IntegerInstructions =
////        { opcode = Opcode.ofName "addi"; funCode = 0; rd = DstReg.GPR}
////
////
////    {
////        // string representation of the opcode (for example, "add")
////        opcode: string 
////        // numeric opcode value
////        opcodeNo: int
////        // numeric function code  value or 0 if no function code
////        funCode: int
////        // NONE, GPR, FPR depending upon whether the instruction writes 
////        // to a destination register (this info is used to set the Qi field 
////        // in one of the register files)
////        dstReg : DstReg
////        // starting bit within instruction that begins the destination register 
////        // field (11 or 16)
////        dstRegBit: int
////        // NONE, GPR, FPR (this info is used to initialize Vj and Qj)
////        s1Reg: S1Reg
////        // starting bit within instruction that begins the source1 register field
////        s1RegBit: int
////        // NONE, GPR, FPR (this info is used to initialize Vk and Qk)
////        s2Reg: S2Reg
////        // starting bit within instruction that begins the source2 register field
////        s2RegBit: int
////        // true or false depending upon whether instruction has an immediate field 
////        // that is used to initialize station.A
////        immedField: bool
////        // starting bit of immediate field
////        immedFieldSBit: int
////        // ending bit of immediate field
////        immedFieldEBit: int
////    }
////
////    static let Trap hex =
////        let bin = Convert.hex2bin hex
////        let op = Opcode.ofBin (bin.[0..Constants.nOpcodeBits - 1])
////        let reg = Convert.bin2int bin.[6..10]
////        let f = Convert.bin2int bin.[27..31]
////        { Opcode = op; FunCode = Some f; RegName = Some reg; rd = 0; rs = 0; rt = 0; imm = None }
////
////    static let IType hex =
////        let bin = Convert.hex2bin hex
////        let op  = Opcode.ofBin (bin.[0..Constants.nOpcodeBits - 1])
////        let rd  = Convert.bin2int bin.[11..15]
////        let rs  = Convert.bin2int bin.[6..10]
////        let imm = Convert.bin2int bin.[16..31]
////        { Opcode = op; FunCode = None; RegName = None; rd = rd; rs = rs; rt = 0; imm = Some imm }
////
////    static let RType hex =
////        let bin = Convert.hex2bin hex
////        let op  = Opcode.ofBin (bin.[26..31])
////        let rd = Convert.bin2int bin.[16..20]
////        let rs = Convert.bin2int bin.[6..10]
////        let rt = Convert.bin2int bin.[11..15]
////        { Opcode = op; FunCode = None; RegName = None; rd = rd; rs = rs; rt = rt; imm = None }
////    
////    static let IsIntegerType hex =
////        let opcode = Opcode.ofInstructionHex hex
////        let intOps = Config.FU.IntegerUnit.Instructions
////        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
////
////    static let IsTrapType hex =
////        let opcode = Opcode.ofInstructionHex hex
////        let intOps = Config.FU.TrapUnit.Instructions
////        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
////
////    static let IsBranchType hex =
////        let opcode = Opcode.ofInstructionHex hex
////        let intOps = Config.FU.BranchUnit.Instructions
////        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
////
////    static let IsMemoryType hex =
////        let opcode = Opcode.ofInstructionHex hex
////        let intOps = Config.FU.MemoryUnit.Instructions
////        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
////
////    static let IsFloatingPointType hex =
////        let opcode = Opcode.ofInstructionHex hex
////        let intOps = Config.FU.FloatingPointUnit.Instructions
////        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
////
////    static let ofInt i =
////        let hex = Convert.int2hex i
////        let opcode = Opcode.ofInstructionHex hex
////        if opcode.Name = "trap"
////        then Instruction.Trap hex
////        elif Opcode.IsRType hex
////        then Instruction.RType hex
////        else Instruction.IType hex
////
////
////
////type IRInstructionInfo(hex:string) =
////    let bin = Convert.hex2bin hex
////    let opcodeBits = bin.[0..Constants.nOpcodeBits - 1] 
////    let opcode = Opcode.ofBin opcodeBits
////    let remainingBits = bin.[Constants.nOpcodeBits..]
////    member val OpcodeBits = opcodeBits with get
////    member val Opcode = opcode with get
////    member val RemainingBits = remainingBits with get
////
////type IntegerInstruction =
////    | ADDI     // IType
////    | NOP       // RType
////    | ADD        // Rtype
////    | SUB       // RType
////    | AND       // RType
////    | OR        // RType
////    | XOR       // RType
////    | MOVF      // RType
////    | MOVFP2I   // RType
////    | MOVI2FP   // RType
////
////    
////    static let ofHex hex =
//////        let bin = Convert.hex2bin hex
//////        let opcodeBits = bin.[0..Constants.nOpcodeBits - 1]
//////        let opcode = Opcode.ofBin opcodeBits
////        let bin = Convert.hex2bin hex
////        
////        if not(Opcode.IsRType hex) then
////            let opcodeBits = bin.[26..31]
////            let opcode = Opcode.ofBin opcodeBits
////            opcode.Name |> function
////            | "add" -> Instruction.RType hex //ADD(IInstruction.RType hex)
////            | _ -> failwith ""
////        else
////            let opcodeBits = bin.[0..Constants.nOpcodeBits - 1]
////            let opcode = Opcode.ofBin opcodeBits
////            opcode.Name |> function
////            | "addi" -> Instruction.IType hex //ADDI(IInstruction.IType hex)
////            | _ -> failwith ""
////
////    static let addi hex = ()
////        
////
////    
////    
////
////
//////type IntegerInstruction =
//////    | ADDI of IRInstruction     // IType
//////    | NOP  of IRInstruction      // RType
//////    | ADD  of IRInstruction       // Rtype
//////    | SUB of IRInstruction       // RType
//////    | AND  of IRInstruction      // RType
//////    | OR  of IRInstruction       // RType
//////    | XOR  of IRInstruction      // RType
//////    | MOVF  of IRInstruction     // RType
//////    | MOVFP2I  of IRInstruction  // RType
//////    | MOVI2FP  of IRInstruction  // RType
//////
//////    static let ofHex hex =
//////        let irInstruction = IRInstructionInfo(hex)
//////        irInstruction.Opcode.Name |> function
//////        | "addi" -> Some(ADDI irInstruction)
//////        | "nop" -> Some(NOP irInstruction)
//////        | "add" -> Some(ADD irInstruction)
//////        | "sub" -> Some(SUB irInstruction)
//////        | "and" -> Some(AND irInstruction)
//////        | "or" -> Some(OR irInstruction)
//////        | "xor" -> Some(XOR irInstruction)
//////        | "movf" -> Some(MOVF irInstruction)
//////        | "movfp2i" -> Some(MOVFP2I irInstruction)
//////        | "movi2fp" -> Some(MOVI2FP irInstruction)
//////        | _ -> None
////
////type TrapInstruction =
////    | Trap of Instruction
////
////    static let ofHex hex =
////        let bin = Convert.hex2bin hex
////        let opcode = Opcode.ofBin (bin.[0..Constants.nOpcodeBits - 1])
////        opcode.Name |> function
////        | "trap" -> Instruction.Trap hex //Trap (IInstruction.Trap hex)
////        | _ -> failwith "failed to create trap instruction"
////
//////type TrapInstruction =
//////    | Trap of IRInstruction
//////
//////    static let ofHex hex =
//////        let irInstruction = IRInstructionInfo(hex)
//////        irInstruction.Opcode.Name |> function
//////        | "trap" -> Some(Trap irInstruction)
//////        | _ -> None
////        
////type BranchInstruction =
////    | BEQZ of IRInstruction
////    | J of IRInstruction
////    | JR of IRInstruction
////    | JAL of IRInstruction
////    | JALR of IRInstruction
////
////    static let ofHex hex =
////        let irInstruction = IRInstructionInfo(hex)
////        irInstruction.Opcode.Name |> function
////        | "beqz" -> Some(BEQZ irInstruction)
////        | "j" -> Some(J irInstruction)
////        | "jr" -> Some(JR irInstruction)
////        | "jal" -> Some(JAL irInstruction)
////        | "jalr" -> Some(JALR irInstruction)
////        | _ -> None
////
////type MemoryInstruction =
////    | LW of IRInstruction
////    | LF of IRInstruction
////    | SW of IRInstruction
////    | SF of IRInstruction
////
////    static let ofHex hex =
////        let irInstruction = IRInstructionInfo(hex)
////        irInstruction.Opcode.Name |> function
////        | "lw" -> Some(LW irInstruction)
////        | "lf" -> Some(LF irInstruction)
////        | "sw" -> Some(SW irInstruction)
////        | "sf" -> Some(SF irInstruction)
////        | _ -> None
////        
////type FloatingPointInstruction =
////    | ADDF of IRInstruction
////    | SUBF of IRInstruction
////    | MULTF of IRInstruction
////    | DIVF of IRInstruction
////    | MULT of IRInstruction
////    | DIV of IRInstruction
////    | CVTF2I of IRInstruction
////    | CVTI2F of IRInstruction
////
////    static let ofHex hex =
////        let irInstruction = IRInstructionInfo(hex)
////        irInstruction.Opcode.Name |> function
////        | "addf" -> Some(ADDF irInstruction)
////        | "subf" -> Some(SUBF irInstruction)
////        | "multf" -> Some(MULTF irInstruction)
////        | "divf" -> Some(DIVF irInstruction)
////        | "mult" -> Some(MULT irInstruction)
////        | "div" -> Some(DIV irInstruction)
////        | "cvtf2i" -> Some(CVTF2I irInstruction)
////        | "cvti2f" -> Some(CVTI2F irInstruction)
////        | _ -> None    
////
////type Instruction =
////    | IntegerInstruction of IInstruction
////    | TrapInstruction of IInstruction
//////    | TrapInstruction of TrapInstruction
//////    | BranchInstruction of BranchInstruction
//////    | MemoryInstruction of MemoryInstruction
//////    | FloatingPointInstruction of FloatingPointInstruction
////
////    static let ofInt instruction =
////        let hex = Convert.int2hex instruction
////
////        let isKind (cfg:Config.FU) (i:int) = 
////            let bin = Convert.int2bin instruction
////            let opcode = Opcode.ofBin (bin.[0..Constants.nOpcodeBits - 1])
////            cfg.Instructions |> Array.tryFind (fun i -> i = opcode.Name) 
////        
////        let isInt = (isKind Config.FU.IntegerUnit instruction).IsSome
////        let isTrap = (isKind Config.FU.TrapUnit instruction).IsSome
////        
////        if isInt then
////            IntegerInstruction.ofHex hex
////        elif isTrap then
////            TrapInstruction.ofHex hex
////        else
////            failwith ""
////
////        Array.fiopcode.Name |> function
////        | "addi" -> 
////            let rd  = Convert.bin2int bin.[11..15]
////            let rs  = Convert.bin2int bin.[6..10]
////            let imm = Convert.bin2int bin.[16..31]
////            IntegerInstruction { Opcode = opcode; rd = rd; rs = rs; rt = 0; imm = imm }
////        | "add" ->
////            let rd = Convert.bin2int bin.[16..20]
////            let rs = Convert.bin2int bin.[6..10]
////            let rt = Convert.bin2int bin.[11..15]
////            IntegerInstruction { Opcode = opcode; rd = rd; rs = rs; rt = rt; imm = 0 }
////        
////        | "trap" -> 
////            let a = Convert.bin2int bin.[6..31]
////            TrapInstruction { Opcode = opcode; rd = 0; rs = 0; rt = 0; imm = a }
////        
////        | _ -> failwith "failed to create instruction"
////
////
////        
////        | "trap" -> TrapInstructionInfo(Trap irInstruction)
////        
////        | "beqz" -> BranchInstructionInfo(BEQZ irInstruction)
////        | "j" -> BranchInstructionInfo(J irInstruction)
////        | "jr" -> BranchInstructionInfo(JR irInstruction)
////        | "jal" -> BranchInstructionInfo(JAL irInstruction)
////        | "jalr" -> BranchInstructionInfo(JALR irInstruction)
////        
////        | "lw" -> MemoryInstructionInfo(LW irInstruction)
////        | "lf" -> MemoryInstructionInfo(LF irInstruction)
////        | "sw" -> MemoryInstructionInfo(SW irInstruction)
////        | "sf" -> MemoryInstructionInfo(SF irInstruction)
////        
////        | "addf" -> FloatingPointInstructionInfo(ADDF irInstruction)
////        | "subf" -> FloatingPointInstructionInfo(SUBF irInstruction)
////        | "multf" -> FloatingPointInstructionInfo(MULTF irInstruction)
////        | "divf" -> FloatingPointInstructionInfo(DIVF irInstruction)
////        | "mult" -> FloatingPointInstructionInfo(MULT irInstruction)
////        | "div" -> FloatingPointInstructionInfo(DIV irInstruction)
////        | "cvtf2i" -> FloatingPointInstructionInfo(CVTF2I irInstruction)
////        | "cvti2f" -> FloatingPointInstructionInfo(CVTI2F irInstruction)
////        
////        | _ -> failwith "failed to create instruction"
////
////type Instruction =
////    | IntegerInstruction of IntegerInstruction
////    | TrapInstruction of TrapInstruction
////    | BranchInstruction of BranchInstruction
////    | MemoryInstruction of MemoryInstruction
////    | FloatingPointInstruction of FloatingPointInstruction
////
////    static let ofHex hex =
////        let irInstruction = IRInstructionInfo(hex)
////        irInstruction.Opcode.Name |> function
////        | "addi" -> IntegerInstructionInfo(ADDI irInstruction)
////        | "nop" -> IntegerInstructionInfo(NOP irInstruction)
////        | "add" -> IntegerInstructionInfo(ADD irInstruction)
////        | "sub" -> IntegerInstructionInfo(SUB irInstruction)
////        | "and" -> IntegerInstructionInfo(AND irInstruction)
////        | "or" -> IntegerInstructionInfo(OR irInstruction)
////        | "xor" -> IntegerInstructionInfo(XOR irInstruction)
////        | "movf" -> IntegerInstructionInfo(MOVF irInstruction)
////        | "movfp2i" -> IntegerInstructionInfo(MOVFP2I irInstruction)
////        | "movi2fp" -> IntegerInstructionInfo(MOVI2FP irInstruction)
////        
////        | "trap" -> TrapInstructionInfo(Trap irInstruction)
////        
////        | "beqz" -> BranchInstructionInfo(BEQZ irInstruction)
////        | "j" -> BranchInstructionInfo(J irInstruction)
////        | "jr" -> BranchInstructionInfo(JR irInstruction)
////        | "jal" -> BranchInstructionInfo(JAL irInstruction)
////        | "jalr" -> BranchInstructionInfo(JALR irInstruction)
////        
////        | "lw" -> MemoryInstructionInfo(LW irInstruction)
////        | "lf" -> MemoryInstructionInfo(LF irInstruction)
////        | "sw" -> MemoryInstructionInfo(SW irInstruction)
////        | "sf" -> MemoryInstructionInfo(SF irInstruction)
////        
////        | "addf" -> FloatingPointInstructionInfo(ADDF irInstruction)
////        | "subf" -> FloatingPointInstructionInfo(SUBF irInstruction)
////        | "multf" -> FloatingPointInstructionInfo(MULTF irInstruction)
////        | "divf" -> FloatingPointInstructionInfo(DIVF irInstruction)
////        | "mult" -> FloatingPointInstructionInfo(MULT irInstruction)
////        | "div" -> FloatingPointInstructionInfo(DIV irInstruction)
////        | "cvtf2i" -> FloatingPointInstructionInfo(CVTF2I irInstruction)
////        | "cvti2f" -> FloatingPointInstructionInfo(CVTI2F irInstruction)
////        
////        | _ -> failwith "failed to create instruction"
////
////
////
////type T =
////    {
////        Opcode      : Opcode
////        FunCode     : int
////        DstReg      : string
////        DstRegBit   : int
////        S1Reg       : string
////        S1RegBit    : int
////        S2Reg       : string
////        S2RegBit    : int
////        ImmedField  : bool
////        ImmedFieldStartBit  : int
////        ImmedFieldEndBit  : int
////    }
////
////
////
////type InstructionState =
////    | I of Issue
////    | X of Execute
////    | W of WriteResult
////
////and WaitCondition(cdb:CDB, RS:ReservationStation[], Buffer:ReservationStation[], lsq:Queue) =
////    member wc.WaitUntil r = function
////        | I i ->
////            i |> function 
////            | Issue.FPOperation -> RS.[r].IsEmpty 
////            | Issue.LoadOrStore -> Buffer.[r].IsEmpty | _ -> true
////        | X x ->
////            x |> function
////            | Execute.FPOperation -> RS.[r].Qj = None && RS.[r].Qk = None
////            | Execute.LoadStoreStep1 -> RS.[r].Qj = None && lsq.Peek() = (r :> obj)
////            | Execute.LoadStep2 -> true //loadstorestep1 needs to be complete
////        | W w -> 
////            w |> function
////            | WriteResult.FPOperationOrLoad -> cdb.Result = None && true //execution complete at r
////            | WriteResult.Store -> RS.[r].Qk = None && true //execution complete at r
////
////and Issue =
////    | FPOperation
////    | LoadOrStore
////    | LoadOnly
////    | StoreOnly
////
////    member i.Action rd rs rt r (waitUntil:WaitCondition) = 
////        let Regs = [|0|]
////        let RegisterStat = Register.ArrayInit 1
////        let RS = [| ReservationStation.Init "" |]
////        let imm = Some 0
////        i |> function
////        | FPOperation ->
////            RegisterStat.[rs].Qi |> function
////            | Some _ -> RS.[r].Qj <- RegisterStat.[rs].Qi
////            | None -> RS.[r].Vj <- Regs.[rs]; RS.[r].Qj <- None
////        
////            RegisterStat.[rt].Qi |> function
////            | Some _ -> RS.[r].Qk <- RegisterStat.[rt].Qi
////            | None -> RS.[r].Vk <- Regs.[rs]; RS.[r].Qk <- None
////        
////            RS.[r].Busy <- true; RegisterStat.[rd].Qi <- Some(string r)
////        
////        | LoadOrStore ->
////            RegisterStat.[rs].Qi |> function
////            | Some _ -> RS.[r].Qj <- RegisterStat.[rs].Qi
////            | None -> RS.[r].Vj <- Regs.[rs]; RS.[r].Qj <- None
////            RS.[r].A <- imm; RS.[r].Busy <- true
////
////        | LoadOnly -> RegisterStat.[rt].Qi <- Some(string r)
////
////        | StoreOnly ->
////            RegisterStat.[rt].Qi |> function
////            | Some _ -> RS.[r].Qk <- RegisterStat.[rt].Qi
////            | None -> RS.[r].Vk <- Regs.[rt]; RS.[r].Qk <- None
////
////and Execute =
////    | FPOperation
////    | LoadStoreStep1
////    | LoadStep2
////
////    member x.Action r =
////        let Mem = [|0|]
////        let RS = [| ReservationStation.Init "" |]
////        x |> function
////        | FPOperation -> 
////            let ops = RS.[r].Vj, RS.[r].Vk
////            ()
////        | LoadStoreStep1 -> RS.[r].A <- RS.[r].Vj + RS.[r].A
////
////        | LoadStep2 -> 
////            let x = Mem.[RS.[r].A]
////            ()
////
////
////and WriteResult =
////    | FPOperationOrLoad
////    | Store
////
////    member w.Action r result =
////        let Mem = [|0|]
////        let Regs = [|0|]
////        let RegisterStat = Register.ArrayInit 1
////        let RS = [| ReservationStation.Init "" |]
////        w |> function
////        | FPOperationOrLoad ->
////            let r' = Some(string r)
////            for x = 0 to Regs.Length - 1 do
////                if RegisterStat.[x].Qi = r' then Regs.[x] <- result; RegisterStat.[x].Qi <- None
////            for x = 0 to RS.Length - 1 do
////                if RS.[x].Qj = r' then  RS.[x].Vj <- result; RS.[x].Qj <- None
////            for x = 0 to RS.Length - 1 do
////                if RS.[x].Qk = r' then RS.[x].Vk <- result; RS.[x].Qk <- None
////            RS.[r].Busy <- false
////
////        | Store ->
////            Mem.[RS.[r].A] <- RS.[r].Vk
////            RS.[r].Busy <- false

        
