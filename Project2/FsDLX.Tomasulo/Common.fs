[<AutoOpen>]
module FsDLX.Tomasulo.Common

type FunctionalUnitConfig =
    {
        nReservationStations : int
        nExecutionUnits : int
        nExecutionTime : int
        Instructions : string[]
    }

    static member IntegerUnit =
        { nReservationStations = 8; nExecutionUnits = 3; nExecutionTime = 1;
            Instructions = [| "addi"; "nop"; "add"; "sub"; "and"; "or"; "xor"; "movf"; "movfp2i"; "movi2fp" |]}

    static member TrapUnit =
        { nReservationStations = 4; nExecutionUnits = 1; nExecutionTime = 1;
            Instructions = [| "trap" |]}

    static member BranchUnit =
        { nReservationStations = 1; nExecutionUnits = 1; nExecutionTime = 1;
            Instructions = [| "beqqz"; "j"; "jr"; "jal"; "jalr" |]}

    static member MemoryUnit =
        { nReservationStations = 8; nExecutionUnits = 1; nExecutionTime = 2;
            Instructions = [| "lw"; "lf"; "sw"; "sf" |]}

    static member FloatingPointUnit =
        { nReservationStations = 8; nExecutionUnits = 2; nExecutionTime = 4;
            Instructions = [| "addf"; "subf"; "multf"; "divf"; "mult"; "div"; "cvtf2i"; "cvti2f" |]}



type Opcode = 
    { Name : string; Code : int }
    static member Null = { Name = ""; Code = 0 }
    override o.ToString() = o.Name

type ReservationStation =
    {
        Name                    : string
        mutable Busy            : bool
        mutable Op              : Opcode
        mutable Vj              : int
        mutable Vk              : int
        mutable Qj              : string option
        mutable Qk              : string option
        mutable A               : int
        mutable ResultReady     : bool
        mutable ResultWritten   : bool
        mutable Result          : int
    }

    member rs.Clear() =
        rs.Busy <- false
        rs.Op <- Opcode.Null
        rs.Vj <- 0; rs.Vk <- 0
        rs.Qj <- None; rs.Qk <- None
        rs.A <- 0

    member rs.IsEmpty = 
        rs.Busy = false         &&
        rs.Op   = Opcode.Null   &&
        rs.Vj   = 0             &&
        rs.Vk   = 0             &&
        rs.Qj   = None          &&
        rs.Qk   = None          &&
        rs.A    = 0


    override rs.ToString() =
        sprintf "
Name    Busy    Op    Vj    Vk    Qj    Qk    A    ResultReady    ResultWritten    Result
%s      %A      %O    %d    %d    %O    %O    %d   %A             %A               %d\n"
            rs.Name rs.Busy rs.Op rs.Vj rs.Vk rs.Qj rs.Qk rs.A
            rs.ResultReady rs.ResultWritten rs.Result

    static member Init name =
        {   Name = name; Busy = false; Op = Opcode.Null; 
            Vj = 0; Vk = 0; Qj = None; Qk = None; A = 0
            ResultReady = false;
            ResultWritten = false;
            Result = 0 }


type Buffer() =
    member val IsEmpty = true with get, set

type CDB = int option

type RegisterFile =
    | GPR of Register[]
    | FPR of Register[]

    static member InitGPR n =
        let regs = Register.ArrayInit n
        RegisterFile.GPR regs

    static member InitFPR n =
        let regs = Register.ArrayInit n
        RegisterFile.FPR regs

and Register =
    {
        mutable Qi          : Qi
        mutable Contents    : int    
    }

    static member Init _ = { Qi = None; Contents = 0 }
    static member ArrayInit n = Array.init n Register.Init

and Qi = string option


type Clock private () =
    static let instance = Clock()
    member val Cycles = 0 with get, set
    member c.Tic() = c.Cycles <- c.Cycles + 1
    static member GetInstance = instance

