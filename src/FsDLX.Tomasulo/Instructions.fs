//module FsDLX.Tomasulo.Instructions
namespace FsDLX.Tomasulo

open System.Collections

type IntegerInstruction =
    | ADDI      // IType
    | NOP       // RType
    | ADD       // Rtype
    | SUB       // RType
    | AND       // RType
    | OR        // RType
    | XOR       // RType
    | MOVF      // RType
    | MOVFP2I   // RType
    | MOVI2FP   // RType

//    member ii.Op(rs, rd, imm) = ()
//    member ii.Op(rs, rt, rd) = ()

type TrapInstruction =
    | Trap

type BranchInstruction =
    | BEQZ
    | J
    | JR
    | JAL
    | JALR

type MemoryInstruction =
    | LW
    | LF
    | SW
    | SF

type FloatingPointInstruction =
    | ADDF
    | SUBF
    | MULTF
    | DIVF
    | MULT
    | DIV
    | CVTF2I
    | CVTI2F
    

type Instruction =
    | IntegerInstruction
    | TrapInstruction
    | BranchInstruction
    | MemoryInstruction
    | FloatingPointInstruction

type T =
    {
        Opcode  : Opcode
        FunCode : int option
        rs      : string option
        rt      : string option
        rd      : string option
        imm     : string option
    }


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



type InstructionState =
    | I of Issue
    | X of Execute
    | W of WriteResult

and WaitCondition(cdb:CDB, RS:ReservationStation[], Buffer:ReservationStation[], lsq:Queue) =
    member wc.WaitUntil r = function
        | I i ->
            i |> function 
            | Issue.FPOperation -> RS.[r].IsEmpty 
            | Issue.LoadOrStore -> Buffer.[r].IsEmpty | _ -> true
        | X x ->
            x |> function
            | Execute.FPOperation -> RS.[r].Qj = None && RS.[r].Qk = None
            | Execute.LoadStoreStep1 -> RS.[r].Qj = None && lsq.Peek() = (r :> obj)
            | Execute.LoadStep2 -> true //loadstorestep1 needs to be complete
        | W w -> 
            w |> function
            | WriteResult.FPOperationOrLoad -> cdb.Result = None && true //execution complete at r
            | WriteResult.Store -> RS.[r].Qk = None && true //execution complete at r

and Issue =
    | FPOperation
    | LoadOrStore
    | LoadOnly
    | StoreOnly

    member i.Action rd rs rt r (waitUntil:WaitCondition) = 
        let Regs = [|0|]
        let RegisterStat = Register.ArrayInit 1
        let RS = [| ReservationStation.Init "" |]
        let imm = 0
        i |> function
        | FPOperation ->
            RegisterStat.[rs].Qi |> function
            | Some _ -> RS.[r].Qj <- RegisterStat.[rs].Qi
            | None -> RS.[r].Vj <- Regs.[rs]; RS.[r].Qj <- None
        
            RegisterStat.[rt].Qi |> function
            | Some _ -> RS.[r].Qk <- RegisterStat.[rt].Qi
            | None -> RS.[r].Vk <- Regs.[rs]; RS.[r].Qk <- None
        
            RS.[r].Busy <- true; RegisterStat.[rd].Qi <- Some(string r)
        
        | LoadOrStore ->
            RegisterStat.[rs].Qi |> function
            | Some _ -> RS.[r].Qj <- RegisterStat.[rs].Qi
            | None -> RS.[r].Vj <- Regs.[rs]; RS.[r].Qj <- None
            RS.[r].A <- imm; RS.[r].Busy <- true

        | LoadOnly -> RegisterStat.[rt].Qi <- Some(string r)

        | StoreOnly ->
            RegisterStat.[rt].Qi |> function
            | Some _ -> RS.[r].Qk <- RegisterStat.[rt].Qi
            | None -> RS.[r].Vk <- Regs.[rt]; RS.[r].Qk <- None

and Execute =
    | FPOperation
    | LoadStoreStep1
    | LoadStep2

    member x.Action r =
        let Mem = [|0|]
        let RS = [| ReservationStation.Init "" |]
        x |> function
        | FPOperation -> 
            let ops = RS.[r].Vj, RS.[r].Vk
            ()
        | LoadStoreStep1 -> RS.[r].A <- RS.[r].Vj + RS.[r].A

        | LoadStep2 -> 
            let x = Mem.[RS.[r].A]
            ()


and WriteResult =
    | FPOperationOrLoad
    | Store

    member w.Action r result =
        let Mem = [|0|]
        let Regs = [|0|]
        let RegisterStat = Register.ArrayInit 1
        let RS = [| ReservationStation.Init "" |]
        w |> function
        | FPOperationOrLoad ->
            let r' = Some(string r)
            for x = 0 to Regs.Length - 1 do
                if RegisterStat.[x].Qi = r' then Regs.[x] <- result; RegisterStat.[x].Qi <- None
            for x = 0 to RS.Length - 1 do
                if RS.[x].Qj = r' then  RS.[x].Vj <- result; RS.[x].Qj <- None
            for x = 0 to RS.Length - 1 do
                if RS.[x].Qk = r' then RS.[x].Vk <- result; RS.[x].Qk <- None
            RS.[r].Busy <- false

        | Store ->
            Mem.[RS.[r].A] <- RS.[r].Vk
            RS.[r].Busy <- false

        
