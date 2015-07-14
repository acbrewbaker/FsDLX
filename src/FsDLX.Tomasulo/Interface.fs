namespace FsDLX.Tomasulo

open System.Collections

type Simulator(input:string, verbose:bool) =
    let cdb : CDB option ref = ref None
    let Clock = Clock.GetInstance
    let PC = PC.GetInstance
    let RegisterFile = RegisterFile.GetInstance
    let Mem = Memory.GetInstance
    let FunctionalUnits = FunctionalUnits.Reset(); FunctionalUnits.GetInstance

    let finished() = if Clock.Cycles = 0 then false else FunctionalUnits.Finished()
    
    let update(cdb) =
        FunctionalUnits.UpdateReservationStations(cdb)
        RegisterFile.Update(cdb)
        FunctionalUnits.ClearReservationStations()

    let branchInBranchUnit() = FunctionalUnits.BranchInBranchUnit()

    let halt = HALT()

    let write = FunctionalUnits.Write
    let execute = FunctionalUnits.Execute
    let issue i = 
        let stall = FunctionalUnits.Issue i
        halt.Issued <- FunctionalUnits.Halt()
        stall

    let fetch i = 
        let instruction = Instruction.OfInstructionInt i
        if instruction.FuncCode = FuncCode.HALT then halt.Fetched <- true
        instruction
        
    let runRegular() =
        Mem.Load(input)
        let mutable stall = false
        
        while (not(halt.Issued) || not(finished())) do
            cdb := write()
            execute()
            if not(halt.Issued) && not(branchInBranchUnit()) then
                stall <- Mem.[PC.Value] |> fetch |> issue
                if not(halt.Fetched) && not(stall) then PC.Increment()
            update(!cdb)
            Clock.Tic()
                            
    let runVerbose() =
        let log = Log()
        Mem.Load(input)
        let mutable stall = false
        
        while (not(halt.Issued) || not(finished())) do
            cdb := write()
            execute()
            if not(halt.Issued) && not(branchInBranchUnit()) then
                stall <- Mem.[PC.Value] |> fetch |> issue
                if not(halt.Fetched) && not(stall) then PC.Increment()
            update(!cdb)
            log.AddEntry(cdb)
            Clock.Tic()
        log.Dump()

    let runDebug() = ()
    
    member s.Run() = 
        if      verbose 
        then    runVerbose()
        else 
            match Config.Simulator.outputLevel with
            | Config.SimulatorOutputLevel.Regular -> runRegular()
            | Config.SimulatorOutputLevel.Verbose -> runVerbose()
            | Config.SimulatorOutputLevel.Debug -> runDebug()

and Log() =
    let mutable log = List.empty<string list>
    member l.AddEntry(cdb:CDB option ref) = 
        let memStr = if Clock.GetInstance.Cycles = 0 then Memory.GetInstance.ToString() else ""
        log <- log @
            [[
                sprintf "%O" Clock.GetInstance
                sprintf "%O" FunctionalUnits.GetInstance
                CDB.Opt2String !cdb
                sprintf "%O" RegisterFile.GetInstance
                memStr        
            ] |> List.choose (fun s -> if s.Length > 1 then Some s else None) ]
    
    override l.ToString() =
        let lines2str = Array.ofList >> Convert.lines2str 
        log |> List.map lines2str |> List.map ((+) "\n") |> lines2str
    
    member l.Dump() = printfn "%O" l
