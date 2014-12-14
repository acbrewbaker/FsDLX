//[<AutoOpen>]
//module FsDLX.Tomasulo.Common
namespace FsDLX.Tomasulo

open System
open System.Linq

open FsDLX.Common


module Config =
    let nOpcodeBits = 6
    let nCharsInHexInstruction = 8
    let nGPRregisters = 32
    let nFPRregisters = 32
    let DefaultMemorySize = 1000

    type FU =
        {
            RSPrefix    : string
            RSCount     : int
            XUnitCount  : int
            XCycles     : int
            Instructions: string[]
        }

        static member IntegerUnit =
            { RSPrefix = "Integer"; RSCount = 8; XUnitCount = 3; XCycles = 1;
                Instructions = [| "addi"; "nop"; "add"; "sub"; "and"; "or"; "xor"; "movf"; "movfp2i"; "movi2fp" |]}

        static member TrapUnit =
            { RSPrefix = "Trap"; RSCount = 4; XUnitCount = 1; XCycles = 1;
                Instructions = [| "trap" |]}

        static member BranchUnit =
            { RSPrefix = "Branch"; RSCount = 1; XUnitCount = 1; XCycles = 1;
                Instructions = [| "beqqz"; "j"; "jr"; "jal"; "jalr" |]}

        static member MemoryUnit =
            { RSPrefix = "Memory"; RSCount = 8; XUnitCount = 1; XCycles = 2;
                Instructions = [| "lw"; "lf"; "sw"; "sf" |]}

        static member FloatingPointUnit =
            { RSPrefix = "FloatingPoint"; RSCount = 8; XUnitCount = 2; XCycles = 4;
                Instructions = [| "addf"; "subf"; "multf"; "divf"; "mult"; "div"; "cvtf2i"; "cvti2f" |]}
      


// The ReservationStation class contains the fields of an individual reservation station: 
// name, busy, opcode, Vj, Vk, Qj, Qk, A, result, resultReady, resultWritten.  It also 
// contains methods to access or modify an individual reservation station.
type ReservationStation =
    {
        Name                    : string
        mutable Busy            : bool
        mutable Op              : Opcode option
        mutable Vj              : int
        mutable Vk              : int
        mutable Qj              : string option
        mutable Qk              : string option
        mutable A               : int option
        mutable ResultReady     : bool
        mutable ResultWritten   : bool
        mutable Result          : int
    }

    member rs.Clear() =
        rs.Busy <- false
        rs.Op <- None
        rs.Vj <- 0; rs.Vk <- 0
        rs.Qj <- None; rs.Qk <- None
        rs.A <- None

    member rs.ClearIfResultWritten() = if rs.ResultWritten then rs.Clear()

    member rs.IsEmpty() = 
        rs.Busy = false         &&
        rs.Op.IsNone            &&
        rs.Vj   = 0             &&
        rs.Vk   = 0             &&
        rs.Qj   = None          &&
        rs.Qk   = None          &&
        rs.A.IsNone

    override rs.ToString() =
        sprintf "
Name    Busy    Op    Vj    Vk    Qj    Qk    A    ResultReady    ResultWritten    Result
%s      %A      %O    %d    %d    %O    %O    %O   %A             %A               %d\n"
            rs.Name rs.Busy rs.Op rs.Vj rs.Vk rs.Qj rs.Qk rs.A
            rs.ResultReady rs.ResultWritten rs.Result

    static member Init name =
        {   Name = name; Busy = false; Op = None; 
            Vj = 0; Vk = 0; Qj = None; Qk = None; A = None
            ResultReady = false;
            ResultWritten = false;
            Result = 0 }

    static member ArrayInit(n, namePrefix) =
        Array.init n (fun i -> ReservationStation.Init (namePrefix + string i))

    static member ArrayInit(cfg:Config.FU) =
        ReservationStation.ArrayInit(cfg.RSCount, cfg.RSPrefix)

    static member Clear (r:ReservationStation) = r.Clear()
    static member ClearIfResultWritten (r:ReservationStation) = r.ClearIfResultWritten()


type CDB() =
    member val Src      = "" with get, set
    member val Result   = Some 0 with get, set


type PC private () =
    static let instance = PC()
    member val Value = 0 with get, set
    static member GetInstance = instance

type Clock private () =
    static let instance = Clock()
    member val Cycles = 0 with get, set
    member c.Tic() = c.Cycles <- c.Cycles + 1
    static member GetInstance = instance

