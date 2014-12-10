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

