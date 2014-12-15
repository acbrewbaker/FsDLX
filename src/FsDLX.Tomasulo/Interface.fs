﻿//[<AutoOpen>]
//module FsDLX.Tomasulo.Interface
namespace FsDLX.Tomasulo

open System.Collections
open FsDLX.Common



type SimulatorState =
    {
        Clock               : Clock
        PC                  : string
        Memory              : string
        GPR                 : string
        FPR                 : string
        CurrentFUnit        : string
        Executing           : string
    }

//    member ss.Update (clock:Clock) fu mem =
//        ss.ClockCycle <- clock.Cycles
//        ss.CurrentFUnit <- fu
//        ss.Memory <- mem

    override ss.ToString() =
        [   sprintf "%O\n" ss.Clock
            sprintf "Memory:\n%s\n" (ss.Memory)
            //sprintf "GPR:\n%s\n" (ss.GPR)
            //sprintf "FPR:\n%s\n" (ss.FPR)
            sprintf "EXECUTING:\n%s" (ss.CurrentFUnit) ]
        |> List.reduce (+)

    static member TakeSnapShot (clock:Clock) (pc:int) (mem:Memory) (gpr:GPR) (fpr:FPR) (fu:FU) =
        {   Clock = clock; PC = string pc
            Memory = mem.ToString()
            GPR = gpr.ToString(); FPR = fpr.ToString()
            CurrentFUnit = fu.ToString()
            Executing = "" }

type Simulator(input:string, verbose:bool) =
    let cdb = CDB()
    let clock = Clock.GetInstance
    let mutable PC = 0
    let memory = Memory.GetInstance Config.Memory.DefaultMemorySize
    let gpr = GPR.GetInstance()
    let fpr = FPR.GetInstance()
    let funits = FunctionalUnits() //FU.InitAll()
    
    let mutable log = List.empty<SimulatorState>
    
    let mutable halt = false
    

    let finished() = 
        if clock.Cycles = 0 then false else funits.Finished()
        

    let updateReservationStations() = funits.UpdateReservationStations(cdb)
    
    let branchInBranchUnit() = false


    // The write step examines each reservation station for an instruction whose result 
    // is ready to be written to the CDB. More than one instruction may be ready to write 
    // in the same clock cycle. Only one instruction will be permitted to write using the 
    // CDB and others will be stalled. (However, a store or PC update could occur in the 
    // same clock cycle as a write using the CDB since those don't use the CDB.) Notice 
    // that the reservation stations are not updated using the result on the CDB until 
    // after the issue. This is in order to properly simulate the time in which these steps 
    // would occur.
    let write() = None


    // The execute step examines each group of reservation stations. When applied to a group 
    // of reservation stations, the execute will do one of the following:
    //   - begin execution of an instruction if its operands are available and a corresponding 
    //     functional unit is available.
    //   - decrease the execution count of an instruction that is already being executed by 
    //     the corresponding functional unit
    //   - compute the result of an executing instruction if the execution count is 0 and set 
    //     the result and result ready fields of the reservation station.
    let execute() = ()

    // The issue step will examine the opcode of  the instruction and issue the instruction 
    // to the appropriate unit. If each reservation station in the unit is busy, the issue 
    // fails and is reattempted in the next clock cycle.
    let issue (instruction:int) = 
        let k = InstructionKind.ofInt instruction
        let opcode = (Opcode.ofInstructionInt instruction).Name
        

        let stall = 
            InstructionKind.ofInt instruction |> function
            | Integer ->
                funits.IntegerUnits |> Array.tryFindIndex (fun u -> not(u.Busy)) |> function
                | Some u -> 
                    printfn "insert int instruction"
                    funits.IntegerUnits.[u].Insert instruction
//                    let iu = funits.IntegerUnits.[unitId].Issue instruction 
//                    
//
//                    instruction.rs |> function 
//                    | S1Reg.GPR rs ->
//                        if gpr.[rs].Qi.IsSome then
                | _ -> false
            | Trap -> 
                funits.TrapUnits |> Array.tryFindIndex (fun u -> not(u.Busy)) |> function
                | Some u -> 
                    printfn "insert trap instruction"
                    funits.TrapUnits.[u].Insert instruction
                | _ -> false
            | Branch -> false
            | Memory -> false
            | FloatingPoint -> false
        
        stall

    
    let showLog() = for l in log do printfn "%O" l
    let log() = log <- SimulatorState.TakeSnapShot clock PC memory gpr fpr funits.All.[0] :: log

    let initialize() =
        memory.Load(input)
        log()    

    let runRegular() =
        initialize()

        while not(halt) && not(finished()) do
            // get name of RS writing to CDB and the value to be written
            cdb.Result <- write()
            execute()
            if not(halt) && not(branchInBranchUnit()) then
                let instruction = memory.[PC]
                // stall set to true if issue fails
                let stall = issue(instruction)
                if not(halt) && not(stall) then PC <- PC + 4
            // update RSs using name and value
            updateReservationStations()
            //log()
            

    let runVerbose() =
        initialize()

        while not(halt) && not(finished()) do
            // get name of RS writing to CDB and the value to be written
            cdb.Result <- write()
            execute()
            if not(halt) && not(branchInBranchUnit()) then
                let instruction = memory.[PC]
                // stall set to true if issue fails
                let stall = issue(instruction)
                if not(halt) && not(stall) then PC <- PC + 4
            // update RSs using name and value
            updateReservationStations()
            log()
            

    let runDebug() =
        initialize()
        //printfn "gpr %A" (gpr.[0])
        while not(halt) && not(finished()) do
            // get name of RS writing to CDB and the value to be written
            cdb.Result <- write()
            execute()
            if not(halt) && not(branchInBranchUnit()) then
                let instruction = memory.[PC]
                // stall set to true if issue fails
                let stall = issue(instruction)
                if not(halt) && not(stall) then PC <- PC + 4
            // update RSs using name and value
            updateReservationStations()
            clock.Tic()
            log()
            showLog()

        printfn "Done"
        printfn "Displaying final log state..."
        showLog()

    member s.Run() = Config.Simulator.outputLevel |> function
        | Regular -> runRegular()
        | Verbose -> runVerbose()
        | Debug -> runDebug()


        



