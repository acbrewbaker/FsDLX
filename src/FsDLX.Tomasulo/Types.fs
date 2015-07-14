namespace FsDLX.Tomasulo

open System

exception ComputeException of string


type InstructionInt = int

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
    member this.GetImmVal(i:InstructionInt) =
        match this with
        | NONE -> None
        | A imm -> 
            let a = imm ||> Convert.int2bits i
            Convert.ToInt32(a.PadLeft(32,a.[0]),2) |> Some

and OperandReg = | NONE | GPR of int | FPR of int

type CDB private () =
    static let mutable instance = CDB()
    member val Src = "" with get, set
    member val Result = 0 with get, set
    override cdb.ToString() = 
        sprintf "CDB: result: %s station: %s" (Convert.int2hex cdb.Result) cdb.Src
    static member Opt2String (cdb:CDB option) =
        match cdb with Some cdb -> sprintf "%O" cdb | None -> ""
    static member GetInstance = instance
    static member Reset() = instance <- CDB()

type PC private () =
    static let mutable instance = PC()
    member val Value = 0 with get, set
    member pc.Increment() = pc.Value <- pc.Value + 4
    static member GetInstance = instance
    static member Reset() = instance <- PC()
    override pc.ToString() = sprintf "Program Counter: %s" (Convert.int2hex pc.Value)

type Clock private () =
    static let mutable instance = Clock()
    member val Cycles = 0 with get, set
    member c.Tic() = c.Cycles <- c.Cycles + 1
    static member GetInstance = instance
    static member Reset() = instance <- Clock()
    override c.ToString() = sprintf "Clock cycle: %d" c.Cycles

type HALT() =
    member val Issued = false with get,set
    member val Fetched = false with get,set
    override h.ToString() = sprintf "HALT - Issued(%A), Fetched(%A)" (h.Issued) (h.Fetched)