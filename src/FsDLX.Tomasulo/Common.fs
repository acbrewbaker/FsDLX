namespace FsDLX.Tomasulo

open System
open System.Linq


module Config =
    type SimulatorOutputLevel =
        | Regular
        | Verbose
        | Debug

    module Opcodes =
        let nOpcodeBits = 6

    module Registers = 
        let nGPR = 32
        let nFPR = 32
        let RegCount = nGPR + nFPR

    module Memory =
        let outputLevel = SimulatorOutputLevel.Regular
        let DefaultMemorySize = 1280

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
                rsPrefix = "Int"
                rsCount = 8
                unitCount = 3
                maxCycles = 1
                instructions = [| "addi"; "nop"; "add"; "sub"; "and"; "or"; "xor"; "movf"; "movfp2i"; "movi2fp" |]
            }

        static member TrapUnit =
            {
                rsPrefix = "Trap"
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
                rsPrefix = "Mem"
                rsCount = 8
                unitCount = 1
                maxCycles = 2
                instructions = [| "lw"; "lf"; "sw"; "sf" |]
            }

        static member FloatingPointUnit =
            {
                rsPrefix = "FP"
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





    


