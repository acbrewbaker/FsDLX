//[<AutoOpen>]
//module FsDLX.Tomasulo.Common
namespace FsDLX.Tomasulo

open System
open System.Linq
open FsDLX.Assembler


module Convert =
    let hex2bytes (hex:string) = hex.Length |> function
        | 8 -> 
            Enumerable.Range(0, hex.Length)
                      .Where(fun x -> x % 2 = 0)
                      .Select(fun x -> Convert.ToByte(hex.Substring(x,2), 16))
                      .ToArray()
        | _ -> failwith "Instruction length greater than 8 bytes"

    let hex2int hex = Convert.ToInt32(hex, 16)

    let int2hex (hex:int) = hex.ToString("x8")


module Config =
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



type Opcode(code:int) =
    member val asInt = code with get 
    member val asHex = Convert.int2hex code with get
    override o.ToString() = o.asHex
    new(code:string) = Opcode(Convert.hex2int code)

type ReservationStation =
    {
        Name                    : string
        mutable Busy            : bool
        mutable Op              : Opcode option
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
        rs.Op <- None
        rs.Vj <- 0; rs.Vk <- 0
        rs.Qj <- None; rs.Qk <- None
        rs.A <- 0

    member rs.ClearIfResultWritten() = if rs.ResultWritten then rs.Clear()

    member rs.IsEmpty = 
        rs.Busy = false         &&
        rs.Op.IsNone            &&
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
        {   Name = name; Busy = false; Op = None; 
            Vj = 0; Vk = 0; Qj = None; Qk = None; A = 0
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

type RegisterFile =
    | GPR of Register[]
    | FPR of Register[]

    member rf.Item
        with get(reg) = rf |> function 
            | GPR gpr -> gpr.[reg] 
            | FPR fpr -> fpr.[reg]
        
        and set reg value = rf |> function
            | GPR gpr -> gpr.[reg] <- value
            | FPR fpr -> fpr.[reg] <- value
        

    static member InitGPR() =
        let regs = Register.ArrayInit Config.nGPRregisters
        RegisterFile.GPR regs

    static member InitFPR() =
        let regs = Register.ArrayInit Config.nFPRregisters
        RegisterFile.FPR regs

and Register =
    {
        mutable Qi          : Qi
        mutable Contents    : int    
    }

    static member Init _ = { Qi = None; Contents = 0 }
    static member ArrayInit n = Array.init n Register.Init

and Qi = string option


type PC private () =
    static let instance = PC()
    member val Value = 0 with get, set
    static member GetInstance = instance

type Clock private () =
    static let instance = Clock()
    member val Cycles = 0 with get, set
    member c.Tic() = c.Cycles <- c.Cycles + 1
    static member GetInstance = instance

