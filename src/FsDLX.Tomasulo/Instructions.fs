﻿//module FsDLX.Tomasulo.Instructions
namespace FsDLX.Tomasulo

open System.Collections
open FsDLX.Common

type InstructionKind =
    | Integer
    | Trap
    | Branch
    | Memory
    | FloatingPoint

    static member ofHex hex =
        let opcode = Opcode.ofInstructionHex hex
        let iOps, tOps, bOps, mOps, fpOps = 
            Config.FU.IntegerUnit.Instructions,
            Config.FU.TrapUnit.Instructions,
            Config.FU.BranchUnit.Instructions,
            Config.FU.MemoryUnit.Instructions,
            Config.FU.FloatingPointUnit.Instructions
        let foundOpcodeIn ops = (ops |> Array.tryFind (fun o -> o = opcode.Name)).IsSome
        if      foundOpcodeIn iOps  then Integer
        elif    foundOpcodeIn tOps  then Trap
        elif    foundOpcodeIn bOps  then Branch
        elif    foundOpcodeIn mOps  then Memory
        elif    foundOpcodeIn fpOps then FloatingPoint
        else failwith "opcode not supported" 
    
    static member ofInt i = InstructionKind.ofHex (Convert.int2hex i)

type DstReg =
    | NONE
    | R of int * int
    | F of int * int

//    member rd.GetReg (i:int) (regs:RegisterFile) = 
//        let rn a b = (Convert.int2bin i).[a..b] |> Convert.bin2int
//        rd |> function
//        | GPR (a,b) ->
//            regs |> function | RegisterFile.GPR gpr -> gpr.[rn a b] | _ -> failwith ""
//        | FPR (a,b) ->
//            regs |> function | RegisterFile.FPR fpr -> fpr.[rn a b] | _ -> failwith ""
//        | _ -> failwith ""

type S1Reg =
    | NONE
    | R of int * int
    | F of int * int

type S2Reg =
    | NONE
    | R of int * int
    | F of int * int

type FunCode =
    | NONE
    | FC of int * int

type Imm =
    | NONE
    | A of int * int

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
type Instruction =
    {
        opcode  : Opcode
        funCode : FunCode
        rd      : DstReg
        rs      : S1Reg
        rt      : S2Reg
        imm     : Imm
    }
    
//    member ins.GetRS (instruction:int) (regs:RegisterFile) = 
//        let idx a b = (Convert.int2bin instruction).[a..b] |> Convert.bin2int
//        ins.rs |> function
//        | S1Reg.GPR (a,b) -> regs.[idx a b]


    static member InitTrap rs =
        {   opcode = Opcode.ofName "trap"; funCode = FunCode.FC(27,31)
            rd = DstReg.NONE; rs = rs; rt = S2Reg.NONE; imm = Imm.NONE }


//    static member IntegerInstructions =
//        { opcode = Opcode.ofName "addi"; funCode = 0; rd = DstReg.GPR}


//    {
//        // string representation of the opcode (for example, "add")
//        opcode: string 
//        // numeric opcode value
//        opcodeNo: int
//        // numeric function code  value or 0 if no function code
//        funCode: int
//        // NONE, GPR, FPR depending upon whether the instruction writes 
//        // to a destination register (this info is used to set the Qi field 
//        // in one of the register files)
//        dstReg : DstReg
//        // starting bit within instruction that begins the destination register 
//        // field (11 or 16)
//        dstRegBit: int
//        // NONE, GPR, FPR (this info is used to initialize Vj and Qj)
//        s1Reg: S1Reg
//        // starting bit within instruction that begins the source1 register field
//        s1RegBit: int
//        // NONE, GPR, FPR (this info is used to initialize Vk and Qk)
//        s2Reg: S2Reg
//        // starting bit within instruction that begins the source2 register field
//        s2RegBit: int
//        // true or false depending upon whether instruction has an immediate field 
//        // that is used to initialize station.A
//        immedField: bool
//        // starting bit of immediate field
//        immedFieldSBit: int
//        // ending bit of immediate field
//        immedFieldEBit: int
//    }

//    static member Trap hex =
//        let bin = Convert.hex2bin hex
//        let op = Opcode.ofBin (bin.[0..Constants.nOpcodeBits - 1])
//        let reg = Convert.bin2int bin.[6..10]
//        let f = Convert.bin2int bin.[27..31]
//        { Opcode = op; FunCode = Some f; RegName = Some reg; rd = 0; rs = 0; rt = 0; imm = None }
//
//    static member IType hex =
//        let bin = Convert.hex2bin hex
//        let op  = Opcode.ofBin (bin.[0..Constants.nOpcodeBits - 1])
//        let rd  = Convert.bin2int bin.[11..15]
//        let rs  = Convert.bin2int bin.[6..10]
//        let imm = Convert.bin2int bin.[16..31]
//        { Opcode = op; FunCode = None; RegName = None; rd = rd; rs = rs; rt = 0; imm = Some imm }
//
//    static member RType hex =
//        let bin = Convert.hex2bin hex
//        let op  = Opcode.ofBin (bin.[26..31])
//        let rd = Convert.bin2int bin.[16..20]
//        let rs = Convert.bin2int bin.[6..10]
//        let rt = Convert.bin2int bin.[11..15]
//        { Opcode = op; FunCode = None; RegName = None; rd = rd; rs = rs; rt = rt; imm = None }
//    
//    static member IsIntegerType hex =
//        let opcode = Opcode.ofInstructionHex hex
//        let intOps = Config.FU.IntegerUnit.Instructions
//        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
//
//    static member IsTrapType hex =
//        let opcode = Opcode.ofInstructionHex hex
//        let intOps = Config.FU.TrapUnit.Instructions
//        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
//
//    static member IsBranchType hex =
//        let opcode = Opcode.ofInstructionHex hex
//        let intOps = Config.FU.BranchUnit.Instructions
//        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
//
//    static member IsMemoryType hex =
//        let opcode = Opcode.ofInstructionHex hex
//        let intOps = Config.FU.MemoryUnit.Instructions
//        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
//
//    static member IsFloatingPointType hex =
//        let opcode = Opcode.ofInstructionHex hex
//        let intOps = Config.FU.FloatingPointUnit.Instructions
//        (intOps |> Array.filter (fun o -> o = opcode.Name)).Length <> 0
//
//    static member ofInt i =
//        let hex = Convert.int2hex i
//        let opcode = Opcode.ofInstructionHex hex
//        if opcode.Name = "trap"
//        then Instruction.Trap hex
//        elif Opcode.IsRType hex
//        then Instruction.RType hex
//        else Instruction.IType hex



//type IRInstruction(hex:string) =
//    let bin = Convert.hex2bin hex
//    let opcodeBits = bin.[0..Constants.nOpcodeBits - 1] 
//    let opcode = Opcode.ofBin opcodeBits
//    let remainingBits = bin.[Constants.nOpcodeBits..]
//    member val OpcodeBits = opcodeBits with get
//    member val Opcode = opcode with get
//    member val RemainingBits = remainingBits with get
//
//type IntegerInstruction =
//    | ADDI     // IType
//    | NOP       // RType
//    | ADD        // Rtype
//    | SUB       // RType
//    | AND       // RType
//    | OR        // RType
//    | XOR       // RType
//    | MOVF      // RType
//    | MOVFP2I   // RType
//    | MOVI2FP   // RType
//
//    
//    static member ofHex hex =
////        let bin = Convert.hex2bin hex
////        let opcodeBits = bin.[0..Constants.nOpcodeBits - 1]
////        let opcode = Opcode.ofBin opcodeBits
//        let bin = Convert.hex2bin hex
//        
//        if not(Opcode.IsRType hex) then
//            let opcodeBits = bin.[26..31]
//            let opcode = Opcode.ofBin opcodeBits
//            opcode.Name |> function
//            | "add" -> Instruction.RType hex //ADD(IInstruction.RType hex)
//            | _ -> failwith ""
//        else
//            let opcodeBits = bin.[0..Constants.nOpcodeBits - 1]
//            let opcode = Opcode.ofBin opcodeBits
//            opcode.Name |> function
//            | "addi" -> Instruction.IType hex //ADDI(IInstruction.IType hex)
//            | _ -> failwith ""
//
//    static member addi hex = ()
//        
//
//    
//    
//
//
////type IntegerInstruction =
////    | ADDI of IRInstruction     // IType
////    | NOP  of IRInstruction      // RType
////    | ADD  of IRInstruction       // Rtype
////    | SUB of IRInstruction       // RType
////    | AND  of IRInstruction      // RType
////    | OR  of IRInstruction       // RType
////    | XOR  of IRInstruction      // RType
////    | MOVF  of IRInstruction     // RType
////    | MOVFP2I  of IRInstruction  // RType
////    | MOVI2FP  of IRInstruction  // RType
////
////    static member ofHex hex =
////        let irInstruction = IRInstruction(hex)
////        irInstruction.Opcode.Name |> function
////        | "addi" -> Some(ADDI irInstruction)
////        | "nop" -> Some(NOP irInstruction)
////        | "add" -> Some(ADD irInstruction)
////        | "sub" -> Some(SUB irInstruction)
////        | "and" -> Some(AND irInstruction)
////        | "or" -> Some(OR irInstruction)
////        | "xor" -> Some(XOR irInstruction)
////        | "movf" -> Some(MOVF irInstruction)
////        | "movfp2i" -> Some(MOVFP2I irInstruction)
////        | "movi2fp" -> Some(MOVI2FP irInstruction)
////        | _ -> None
//
//type TrapInstruction =
//    | Trap of Instruction
//
//    static member ofHex hex =
//        let bin = Convert.hex2bin hex
//        let opcode = Opcode.ofBin (bin.[0..Constants.nOpcodeBits - 1])
//        opcode.Name |> function
//        | "trap" -> Instruction.Trap hex //Trap (IInstruction.Trap hex)
//        | _ -> failwith "failed to create trap instruction"
//
////type TrapInstruction =
////    | Trap of IRInstruction
////
////    static member ofHex hex =
////        let irInstruction = IRInstruction(hex)
////        irInstruction.Opcode.Name |> function
////        | "trap" -> Some(Trap irInstruction)
////        | _ -> None
//        
//type BranchInstruction =
//    | BEQZ of IRInstruction
//    | J of IRInstruction
//    | JR of IRInstruction
//    | JAL of IRInstruction
//    | JALR of IRInstruction
//
//    static member ofHex hex =
//        let irInstruction = IRInstruction(hex)
//        irInstruction.Opcode.Name |> function
//        | "beqz" -> Some(BEQZ irInstruction)
//        | "j" -> Some(J irInstruction)
//        | "jr" -> Some(JR irInstruction)
//        | "jal" -> Some(JAL irInstruction)
//        | "jalr" -> Some(JALR irInstruction)
//        | _ -> None
//
//type MemoryInstruction =
//    | LW of IRInstruction
//    | LF of IRInstruction
//    | SW of IRInstruction
//    | SF of IRInstruction
//
//    static member ofHex hex =
//        let irInstruction = IRInstruction(hex)
//        irInstruction.Opcode.Name |> function
//        | "lw" -> Some(LW irInstruction)
//        | "lf" -> Some(LF irInstruction)
//        | "sw" -> Some(SW irInstruction)
//        | "sf" -> Some(SF irInstruction)
//        | _ -> None
//        
//type FloatingPointInstruction =
//    | ADDF of IRInstruction
//    | SUBF of IRInstruction
//    | MULTF of IRInstruction
//    | DIVF of IRInstruction
//    | MULT of IRInstruction
//    | DIV of IRInstruction
//    | CVTF2I of IRInstruction
//    | CVTI2F of IRInstruction
//
//    static member ofHex hex =
//        let irInstruction = IRInstruction(hex)
//        irInstruction.Opcode.Name |> function
//        | "addf" -> Some(ADDF irInstruction)
//        | "subf" -> Some(SUBF irInstruction)
//        | "multf" -> Some(MULTF irInstruction)
//        | "divf" -> Some(DIVF irInstruction)
//        | "mult" -> Some(MULT irInstruction)
//        | "div" -> Some(DIV irInstruction)
//        | "cvtf2i" -> Some(CVTF2I irInstruction)
//        | "cvti2f" -> Some(CVTI2F irInstruction)
//        | _ -> None    

//type Instruction =
//    | IntegerInstruction of IInstruction
//    | TrapInstruction of IInstruction
////    | TrapInstruction of TrapInstruction
////    | BranchInstruction of BranchInstruction
////    | MemoryInstruction of MemoryInstruction
////    | FloatingPointInstruction of FloatingPointInstruction
//
//    static member ofInt instruction =
//        let hex = Convert.int2hex instruction
//
//        let isKind (cfg:Config.FU) (i:int) = 
//            let bin = Convert.int2bin instruction
//            let opcode = Opcode.ofBin (bin.[0..Constants.nOpcodeBits - 1])
//            cfg.Instructions |> Array.tryFind (fun i -> i = opcode.Name) 
//        
//        let isInt = (isKind Config.FU.IntegerUnit instruction).IsSome
//        let isTrap = (isKind Config.FU.TrapUnit instruction).IsSome
//        
//        if isInt then
//            IntegerInstruction.ofHex hex
//        elif isTrap then
//            TrapInstruction.ofHex hex
//        else
//            failwith ""

//        Array.fiopcode.Name |> function
//        | "addi" -> 
//            let rd  = Convert.bin2int bin.[11..15]
//            let rs  = Convert.bin2int bin.[6..10]
//            let imm = Convert.bin2int bin.[16..31]
//            IntegerInstruction { Opcode = opcode; rd = rd; rs = rs; rt = 0; imm = imm }
//        | "add" ->
//            let rd = Convert.bin2int bin.[16..20]
//            let rs = Convert.bin2int bin.[6..10]
//            let rt = Convert.bin2int bin.[11..15]
//            IntegerInstruction { Opcode = opcode; rd = rd; rs = rs; rt = rt; imm = 0 }
//        
//        | "trap" -> 
//            let a = Convert.bin2int bin.[6..31]
//            TrapInstruction { Opcode = opcode; rd = 0; rs = 0; rt = 0; imm = a }
//        
//        | _ -> failwith "failed to create instruction"


        
//        | "trap" -> TrapInstruction(Trap irInstruction)
//        
//        | "beqz" -> BranchInstruction(BEQZ irInstruction)
//        | "j" -> BranchInstruction(J irInstruction)
//        | "jr" -> BranchInstruction(JR irInstruction)
//        | "jal" -> BranchInstruction(JAL irInstruction)
//        | "jalr" -> BranchInstruction(JALR irInstruction)
//        
//        | "lw" -> MemoryInstruction(LW irInstruction)
//        | "lf" -> MemoryInstruction(LF irInstruction)
//        | "sw" -> MemoryInstruction(SW irInstruction)
//        | "sf" -> MemoryInstruction(SF irInstruction)
//        
//        | "addf" -> FloatingPointInstruction(ADDF irInstruction)
//        | "subf" -> FloatingPointInstruction(SUBF irInstruction)
//        | "multf" -> FloatingPointInstruction(MULTF irInstruction)
//        | "divf" -> FloatingPointInstruction(DIVF irInstruction)
//        | "mult" -> FloatingPointInstruction(MULT irInstruction)
//        | "div" -> FloatingPointInstruction(DIV irInstruction)
//        | "cvtf2i" -> FloatingPointInstruction(CVTF2I irInstruction)
//        | "cvti2f" -> FloatingPointInstruction(CVTI2F irInstruction)
        
//        | _ -> failwith "failed to create instruction"

//type Instruction =
//    | IntegerInstruction of IntegerInstruction
//    | TrapInstruction of TrapInstruction
//    | BranchInstruction of BranchInstruction
//    | MemoryInstruction of MemoryInstruction
//    | FloatingPointInstruction of FloatingPointInstruction
//
//    static member ofHex hex =
//        let irInstruction = IRInstruction(hex)
//        irInstruction.Opcode.Name |> function
//        | "addi" -> IntegerInstruction(ADDI irInstruction)
//        | "nop" -> IntegerInstruction(NOP irInstruction)
//        | "add" -> IntegerInstruction(ADD irInstruction)
//        | "sub" -> IntegerInstruction(SUB irInstruction)
//        | "and" -> IntegerInstruction(AND irInstruction)
//        | "or" -> IntegerInstruction(OR irInstruction)
//        | "xor" -> IntegerInstruction(XOR irInstruction)
//        | "movf" -> IntegerInstruction(MOVF irInstruction)
//        | "movfp2i" -> IntegerInstruction(MOVFP2I irInstruction)
//        | "movi2fp" -> IntegerInstruction(MOVI2FP irInstruction)
//        
//        | "trap" -> TrapInstruction(Trap irInstruction)
//        
//        | "beqz" -> BranchInstruction(BEQZ irInstruction)
//        | "j" -> BranchInstruction(J irInstruction)
//        | "jr" -> BranchInstruction(JR irInstruction)
//        | "jal" -> BranchInstruction(JAL irInstruction)
//        | "jalr" -> BranchInstruction(JALR irInstruction)
//        
//        | "lw" -> MemoryInstruction(LW irInstruction)
//        | "lf" -> MemoryInstruction(LF irInstruction)
//        | "sw" -> MemoryInstruction(SW irInstruction)
//        | "sf" -> MemoryInstruction(SF irInstruction)
//        
//        | "addf" -> FloatingPointInstruction(ADDF irInstruction)
//        | "subf" -> FloatingPointInstruction(SUBF irInstruction)
//        | "multf" -> FloatingPointInstruction(MULTF irInstruction)
//        | "divf" -> FloatingPointInstruction(DIVF irInstruction)
//        | "mult" -> FloatingPointInstruction(MULT irInstruction)
//        | "div" -> FloatingPointInstruction(DIV irInstruction)
//        | "cvtf2i" -> FloatingPointInstruction(CVTF2I irInstruction)
//        | "cvti2f" -> FloatingPointInstruction(CVTI2F irInstruction)
//        
//        | _ -> failwith "failed to create instruction"


//
//type T =
//    {
//        Opcode      : Opcode
//        FunCode     : int
//        DstReg      : string
//        DstRegBit   : int
//        S1Reg       : string
//        S1RegBit    : int
//        S2Reg       : string
//        S2RegBit    : int
//        ImmedField  : bool
//        ImmedFieldStartBit  : int
//        ImmedFieldEndBit  : int
//    }
//
//
//
//type InstructionState =
//    | I of Issue
//    | X of Execute
//    | W of WriteResult
//
//and WaitCondition(cdb:CDB, RS:ReservationStation[], Buffer:ReservationStation[], lsq:Queue) =
//    member wc.WaitUntil r = function
//        | I i ->
//            i |> function 
//            | Issue.FPOperation -> RS.[r].IsEmpty 
//            | Issue.LoadOrStore -> Buffer.[r].IsEmpty | _ -> true
//        | X x ->
//            x |> function
//            | Execute.FPOperation -> RS.[r].Qj = None && RS.[r].Qk = None
//            | Execute.LoadStoreStep1 -> RS.[r].Qj = None && lsq.Peek() = (r :> obj)
//            | Execute.LoadStep2 -> true //loadstorestep1 needs to be complete
//        | W w -> 
//            w |> function
//            | WriteResult.FPOperationOrLoad -> cdb.Result = None && true //execution complete at r
//            | WriteResult.Store -> RS.[r].Qk = None && true //execution complete at r
//
//and Issue =
//    | FPOperation
//    | LoadOrStore
//    | LoadOnly
//    | StoreOnly
//
//    member i.Action rd rs rt r (waitUntil:WaitCondition) = 
//        let Regs = [|0|]
//        let RegisterStat = Register.ArrayInit 1
//        let RS = [| ReservationStation.Init "" |]
//        let imm = Some 0
//        i |> function
//        | FPOperation ->
//            RegisterStat.[rs].Qi |> function
//            | Some _ -> RS.[r].Qj <- RegisterStat.[rs].Qi
//            | None -> RS.[r].Vj <- Regs.[rs]; RS.[r].Qj <- None
//        
//            RegisterStat.[rt].Qi |> function
//            | Some _ -> RS.[r].Qk <- RegisterStat.[rt].Qi
//            | None -> RS.[r].Vk <- Regs.[rs]; RS.[r].Qk <- None
//        
//            RS.[r].Busy <- true; RegisterStat.[rd].Qi <- Some(string r)
//        
//        | LoadOrStore ->
//            RegisterStat.[rs].Qi |> function
//            | Some _ -> RS.[r].Qj <- RegisterStat.[rs].Qi
//            | None -> RS.[r].Vj <- Regs.[rs]; RS.[r].Qj <- None
//            RS.[r].A <- imm; RS.[r].Busy <- true
//
//        | LoadOnly -> RegisterStat.[rt].Qi <- Some(string r)
//
//        | StoreOnly ->
//            RegisterStat.[rt].Qi |> function
//            | Some _ -> RS.[r].Qk <- RegisterStat.[rt].Qi
//            | None -> RS.[r].Vk <- Regs.[rt]; RS.[r].Qk <- None
//
//and Execute =
//    | FPOperation
//    | LoadStoreStep1
//    | LoadStep2
//
//    member x.Action r =
//        let Mem = [|0|]
//        let RS = [| ReservationStation.Init "" |]
//        x |> function
//        | FPOperation -> 
//            let ops = RS.[r].Vj, RS.[r].Vk
//            ()
//        | LoadStoreStep1 -> RS.[r].A <- RS.[r].Vj + RS.[r].A
//
//        | LoadStep2 -> 
//            let x = Mem.[RS.[r].A]
//            ()
//
//
//and WriteResult =
//    | FPOperationOrLoad
//    | Store
//
//    member w.Action r result =
//        let Mem = [|0|]
//        let Regs = [|0|]
//        let RegisterStat = Register.ArrayInit 1
//        let RS = [| ReservationStation.Init "" |]
//        w |> function
//        | FPOperationOrLoad ->
//            let r' = Some(string r)
//            for x = 0 to Regs.Length - 1 do
//                if RegisterStat.[x].Qi = r' then Regs.[x] <- result; RegisterStat.[x].Qi <- None
//            for x = 0 to RS.Length - 1 do
//                if RS.[x].Qj = r' then  RS.[x].Vj <- result; RS.[x].Qj <- None
//            for x = 0 to RS.Length - 1 do
//                if RS.[x].Qk = r' then RS.[x].Vk <- result; RS.[x].Qk <- None
//            RS.[r].Busy <- false
//
//        | Store ->
//            Mem.[RS.[r].A] <- RS.[r].Vk
//            RS.[r].Busy <- false

        
