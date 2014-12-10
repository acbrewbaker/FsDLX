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
        mutable Qj              : string
        mutable Qk              : string
        mutable A               : int
        mutable ResultReady     : bool
        mutable ResultWritten   : bool
        mutable Result          : int
    }

    member rs.Clear() =
        rs.Busy <- false
        rs.Op <- Opcode.Null
        rs.Vj <- 0; rs.Vk <- 0
        rs.Qj <- ""; rs.Qk <- ""
        rs.A <- 0

    override rs.ToString() =
        sprintf "
Name    Busy    Op    Vj    Vk    Qj    Qk    A    ResultReady    ResultWritten    Result
%s      %A      %O    %d    %d    %s    %s    %d   %A             %A               %d\n"
            rs.Name rs.Busy rs.Op rs.Vj rs.Vk rs.Qj rs.Qk rs.A
            rs.ResultReady rs.ResultWritten rs.Result

    static member Init name =
        {   Name = name; Busy = false; Op = Opcode.Null; 
            Vj = 0; Vk = 0; Qj = ""; Qk = ""; A = 0
            ResultReady = false;
            ResultWritten = false;
            Result = 0 }


[<AbstractClass>]
type FunctionalUnit(cfg:FunctionalUnitConfig) =
    
    abstract RS : ReservationStation[]

    member fu.MaxCycles = cfg.nExecutionTime
    member fu.RSCount   = cfg.nReservationStations
     

    member val CyclesRemaining = 0 with get, set
    member val Busy = false with get, set


type RegisterFile =
    | GPR of Register[]
    | FPR of Register[]

    static member InitGPR n =
        let regs = Array.init<Register> n (Register.Init())
        RegisterFile.GPR regs

    static member InitFPR n =
        let regs = Array.init<Register> n (Register.Init())
        RegisterFile.FPR regs

and Register =
    {
        mutable Qi          : Qi
        mutable Contents    : int    
    }

    static member Init _ _ = { Qi = Qi.Blank; Contents = 0 }

and Qi =
    | Blank
    | Contents of int


type Clock private () =
    static let instance = Clock()
    member val Cycles = 0 with get, set
    member c.Tic() = c.Cycles <- c.Cycles + 1
    static member GetInstance = instance

