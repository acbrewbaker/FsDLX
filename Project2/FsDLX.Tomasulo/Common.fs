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


type ReservationStation =
    {
        Name            : string
        mutable Busy    : bool
        mutable Op      : string
        mutable Vj      : int
        mutable Vk      : int
        mutable Qj      : string
        mutable Qk      : string
        mutable A       : int
    }

    member rs.Clear() =
        rs.Busy <- false
        rs.Op <- ""
        rs.Vj <- 0; rs.Vk <- 0
        rs.Qj <- ""; rs.Qk <- ""
        rs.A <- 0

    override rs.ToString() =
        sprintf "
Name    Busy    Op    Vj    Vk    Qj    Qk    A
%s      %A      %s    %d    %d    %s    %s    %d\n"
            rs.Name rs.Busy rs.Op rs.Vj rs.Vk rs.Qj rs.Qk rs.A

    static member Init name =
        { Name = name; Busy = false; Op = ""; Vj = 0; Vk = 0; Qj = ""; Qk = ""; A = 0}



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