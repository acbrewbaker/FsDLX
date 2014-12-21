//[<AutoOpen>]
//module FsDLX.Tomasulo.Interface
namespace FsDLX.Tomasulo

open System.Collections
open FsDLX.Common



type SimulatorState =
    {
        ClockCycles         : int
        PC                  : string
        Memory              : string
        GPR                 : string
        FPR                 : string
        CurrentFUnit        : string
        Executing           : string
    }


    override ss.ToString() =
        [   sprintf "Clock cycles: %d\n" ss.ClockCycles
            sprintf "%s" (if ss.ClockCycles = 0 then sprintf "Memory:\n%s" (ss.Memory) else "")
            sprintf "EXECUTING:\n%s" (ss.CurrentFUnit)
            sprintf "%s\n" (ss.GPR.ToString().Trim())  ]
        |> List.reduce (+)

    static member TakeSnapShot (cc:int) (pc:int) (mem:string) (gpr:GPR) (fpr:FPR) (funits:FunctionalUnits) =
        {   ClockCycles = cc
            PC = string pc
            Memory = mem
            GPR = gpr.ToString(); FPR = fpr.ToString()
            CurrentFUnit = if cc <> 0 then funits.ToString() else ""
            Executing = "" }

type Simulator(input:string, verbose:bool) =
    let mutable cdb : CDB option = None
//    let clock = Clock.GetInstance
    let mutable PC = 0
    let memory = Memory.GetInstance
//    let gpr = GPR.GetInstance
//    let fpr = FPR.GetInstance
    let funits = FunctionalUnits() //FU.InitAll()
    
    let mutable logEntries = List.empty<SimulatorState>
    
    let mutable halt = false
    

    let finished() = 
        if Clock.GetInstance.Cycles = 0 then false else funits.AllFinished()
        

    let updateReservationStations() = cdb |> function
        | Some _ ->
            funits.UpdateReservationStations()
            GPR.GetInstance.Update()
            FPR.GetInstance.Update()
        | None -> ()

    let clearReservationStations() = funits.ClearReservationStations()
    
    let branchInBranchUnit() = false


    // The write step examines each reservation station for an instruction whose result 
    // is ready to be written to the CDB. More than one instruction may be ready to write 
    // in the same clock cycle. Only one instruction will be permitted to write using the 
    // CDB and others will be stalled. (However, a store or PC update could occur in the 
    // same clock cycle as a write using the CDB since those don't use the CDB.) Notice 
    // that the reservation stations are not updated using the result on the CDB until 
    // after the issue. This is in order to properly simulate the time in which these steps 
    // would occur.
    let write() = 
        
        funits.All |> Array.tryPick (fun u -> u.Write())


    // The execute step examines each group of reservation stations. When applied to a group 
    // of reservation stations, the execute will do one of the following:
    //   - begin execution of an instruction if its operands are available and a corresponding 
    //     functional unit is available.
    //   - decrease the execution count of an instruction that is already being executed by 
    //     the corresponding functional unit
    //   - compute the result of an executing instruction if the execution count is 0 and set 
    //     the result and result ready fields of the reservation station.
    let execute() = funits.All |> Array.iter (fun u -> u.Execute())
//        funits.All |> Array.tryFind (fun u -> u.Execute()) |> function
//        | Some _ -> halt <- true
//        | None -> ()

    // The issue step will examine the opcode of  the instruction and issue the instruction 
    // to the appropriate unit. If each reservation station in the unit is busy, the issue 
    // fails and is reattempted in the next clock cycle.
    let issue (instruction:int) = 
        let k = InstructionKind.ofInt instruction
        let opcode = (Opcode.ofInstructionInt instruction).Name
//        printfn "Issuing: %O" opcode
        let stall = 
            InstructionKind.ofInt instruction |> function
            | Integer ->
                printfn "Issuing: %O" opcode
                funits.IntegerUnit |> Array.iter (fun u -> printfn "%s" (u.Dump()))
                funits.IntegerUnit |> Array.tryFindIndex (fun u -> not(u.Busy)) |> function
                | Some u -> 
                    printfn "unit id: %d" u
                    funits.IntegerUnit.[u].Insert instruction
                | _ -> 
                    printfn "stall in int issue"
                    true
            | Trap -> 
                funits.TrapUnit |> Array.tryFindIndex (fun u -> not(u.Busy)) |> function
                | Some u -> 
                    
                    //true
                    funits.TrapUnit.[u].Insert instruction |> ignore
                    false
                | _ -> true
            | Branch -> false
            | Memory -> false
            | FloatingPoint -> false
        
        stall

    
    let showLog() = for l in logEntries do printfn "%O" l
    let log() = 
        let entry = SimulatorState.TakeSnapShot Clock.GetInstance.Cycles PC (memory.Dump()) GPR.GetInstance FPR.GetInstance funits
        printfn "%O" entry
        //logEntries <- logEntries @ [entry]

    let initialize() =
        memory.Load(input)

    let runRegular() = ()
            
    let runVerbose() = ()
    

    let display() =
        printfn "%s" (funits.GetStationIssued())
        Clock.GetInstance.Cycles |> function
        | 0 ->
                
            printfn "%s" (funits.IntegerUnitReservationStations.[0].ToString())
            printfn "%O" memory
        | 1 ->
            printfn "%s" (funits.IntegerUnitReservationStations.[0].ToString())
            printfn "%s" (funits.IntegerUnitReservationStations.[1].ToString())
            printfn "%s" (funits.GetExecuting())
        | 2 ->
            printfn "%s" (funits.IntegerUnitReservationStations.[0].ToString())
            printfn "%s" (funits.IntegerUnitReservationStations.[1].ToString())
            printfn "%s" (funits.GetExecuting())
            printfn "%O" CDB.GetInstance
            printfn "%s" (GPR.GetInstance.Dump())
        | 3 ->
            printfn "%s" (funits.TrapUnitReservationStations.[0].ToString())
            printf "%O" CDB.GetInstance
            printfn "%s" (GPR.GetInstance.Dump())
        | 4 ->
            printfn "%s" (funits.TrapUnitReservationStations.[0].ToString())
            printfn "%s" (funits.TrapUnitReservationStations.[1].ToString())
            printfn "%s" (funits.GetExecuting())
        | 5 ->
            printfn "%O" CDB.GetInstance
            printfn "%s" (GPR.GetInstance.Dump())
        | 6 ->
            printfn "%s" (funits.GetExecuting())
        | 7 ->
            printfn "%s" (funits.GetExecuting())
        | 8 ->
            printfn "should be done"
        | _ -> failwith "not enough clock cycles"
        printfn ""

    let runDebug() =
        initialize()
        //printfn "gpr %A" (gpr.[0])
        while not(halt) && not(finished()) do
            
//            funits.IntegerUnits |> Array.iter (fun iu -> iu.RS |> Array.iter (printfn "%O"))
            printfn "%O" Clock.GetInstance
            // get name of RS writing to CDB and the value to be written
            cdb <- write()
            execute()
            if not(halt) && not(branchInBranchUnit()) then
                let instruction = memory.[PC]
                // stall set to true if issue fails
                let stall = issue(instruction)
                
                if not(halt) && not(stall) then PC <- PC + 4
                
            // update RSs using name and value
            updateReservationStations()
            //printfn "%s" (funits.IntegerUnit.[0].RS.Dump())
            //printfn "%s" (GPR.GetInstance.Dump())
            
            //log()
            //clearReservationStations()
            
            display()
            Clock.GetInstance.Tic()
            
            
            
            //showLog()

        

    member s.Run() = Config.Simulator.outputLevel |> function
        | Regular -> runRegular()
        | Verbose -> runVerbose()
        | Debug -> runDebug()


        



