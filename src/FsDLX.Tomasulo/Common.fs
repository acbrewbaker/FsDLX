//[<AutoOpen>]
//module FsDLX.Tomasulo.Common
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
    static let instance = new CDB()

    let mutable src, result = "", 0
    member cdb.Src
        with get() = src
        and set(v) = src <- v

    member cdb.Result
        with get() = result
        and set(v) = result <- v

    override cdb.ToString() =
        sprintf "CDB: result: %s station: %s"
            (Convert.int2hex cdb.Result)
            cdb.Src
            
    static member GetInstance = instance
    interface IDisposable with member this.Dispose() = ()

    static member Opt2String (cdb:CDB option) = cdb |> function
        | Some cdb -> sprintf "%O" cdb
        | None -> ""

type PC private () =
    static let instance = new PC()
    member val Value = 0 with get, set
    member pc.Increment() = pc.Value <- pc.Value + 4
    static member GetInstance = instance
    interface IDisposable with member this.Dispose() = ()

type Clock private () =
    static let instance = new Clock()
    member val Cycles = 0 with get, set
    member c.Tic() = c.Cycles <- c.Cycles + 1
    static member GetInstance = instance
    interface IDisposable with member this.Dispose() = ()
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
        let outputLevel = SimulatorOutputLevel.Debug


    let nCharsInHexInstruction = 8
    
//    module IntegerUnit =
//        let rsPrefix = "Int" 
//        let rsCount = 8
//        let unitCount = 3
//        let maxCycles = 1
//        let instructions = [| "addi"; "nop"; "add"; "sub"; "and"; "or"; "xor"; "movf"; "movfp2i"; "movi2fp" |]
//    
//    module TrapUnit =
//        let rsPrefix = "Trap" 
//        let rsCount = 4
//        let unitCount = 1
//        let maxCycles = 1
//        let instructions = [| "trap" |]
//
//    module BranchUnit =
//        let rsPrefix = "Branch" 
//        let rsCount = 1
//        let unitCount = 1
//        let maxCycles = 1
//        let instructions = [| "beqz"; "j"; "jr"; "jal"; "jalr" |]
//
//    module MemoryUnit =
//        let rsPrefix = "Memory" 
//        let rsCount = 8
//        let unitCount = 1
//        let maxCycles = 1
//        let instructions = [| "lw"; "lf"; "sw"; "sf" |]
//
//    module FloatingPointUnit =
//        let rsPrefix = "FP" 
//        let rsCount = 8
//        let unitCount = 2
//        let maxCycles = 4
//        let instructions = [| "addf"; "subf"; "multf"; "divf"; "mult"; "div"; "cvtf2i"; "cvti2f" |]

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

        static member RSTotal =
            FunctionalUnit.IntegerUnit.rsCount +
            FunctionalUnit.TrapUnit.rsCount +
            FunctionalUnit.BranchUnit.rsCount +
            FunctionalUnit.MemoryUnit.rsCount +
            FunctionalUnit.FloatingPointUnit.rsCount





    


