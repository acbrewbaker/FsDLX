namespace FsDLX.Tomasulo

open System
open System.Threading
open System.Linq

open FsDLX.Common


type SimulatorOutputLevel =
    | Regular
    | Verbose
    | Debug

type InsertFunc = Opcode -> int * int * int * int -> bool

type OperandReg = | NONE | GPR of int | FPR of int

type CDB private () =
    static let mutable instance = CDB()
    member val Src = "" with get, set
    member val Result = 0 with get, set
    override cdb.ToString() =
        sprintf "CDB: result: %s station: %s" (Convert.int2hex cdb.Result) cdb.Src
    static member GetInstance = instance
    static member Reset() = instance <- CDB()
    static member Opt2String (cdb:CDB option) = 
        match cdb with Some cdb -> sprintf "%O" cdb | None -> ""

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


module InstructionHex =
    let toOpcodeBits hex = (Convert.hex2bin hex).[0..Constants.nOpcodeBits - 1]


module Config =
    module Registers = 
        let nGPR = 32
        let nFPR = 32
        let RegCount = nGPR + nFPR

    module Memory =
        let outputLevel = SimulatorOutputLevel.Regular
        let DefaultMemorySize = 1000

    module FunctionalUnits =
        let outputLevel = SimulatorOutputLevel.Debug

    module Simulator =
        let outputLevel = SimulatorOutputLevel.Regular


    let nCharsInHexInstruction = 8
    
    type FunctionalUnit =
        {
            rsPrefix    : string
            rsCount     : int
            unitCount  : int
            maxCycles     : int
            instructions: string[]
        }

        static member IntegerUnit =
            { 
                rsPrefix = "IntUnit"
                rsCount = 8
                unitCount = 3
                maxCycles = 1
                instructions = [| "addi"; "nop"; "add"; "sub"; "and"; "or"; "xor"; "movf"; "movfp2i"; "movi2fp" |]
            }

        static member TrapUnit =
            {
                rsPrefix = "TrapUnit"
                rsCount = 4
                unitCount = 1
                maxCycles = 1
                instructions = [| "trap" |]
            }

        static member BranchUnit =
            {
                rsPrefix = "Branch"
                rsCount = 1
                unitCount = 1
                maxCycles = 1
                instructions = [| "beqz"; "j"; "jr"; "jal"; "jalr" |]
            }

        static member MemoryUnit =
            {
                rsPrefix = "Memory"
                rsCount = 8
                unitCount = 1
                maxCycles = 2
                instructions = [| "lw"; "lf"; "sw"; "sf" |]
            }

        static member FloatingPointUnit =
            {
                rsPrefix = "FloatingPoint"
                rsCount = 8
                unitCount = 2
                maxCycles = 4
                instructions = [| "addf"; "subf"; "multf"; "divf"; "mult"; "div"; "cvtf2i"; "cvti2f" |]
            }

        static member All =
            [|  FunctionalUnit.IntegerUnit;
                FunctionalUnit.TrapUnit;
                FunctionalUnit.BranchUnit;
                FunctionalUnit.MemoryUnit;
                FunctionalUnit.FloatingPointUnit |]

        static member RSTotal =
            FunctionalUnit.IntegerUnit.rsCount +
            FunctionalUnit.TrapUnit.rsCount +
            FunctionalUnit.BranchUnit.rsCount +
            FunctionalUnit.MemoryUnit.rsCount +
            FunctionalUnit.FloatingPointUnit.rsCount





    


