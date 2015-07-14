namespace FsDLX.Tomasulo

open System.Collections

type Simulator(input:string, verbose:bool) =
    let cdb : CDB option ref = ref None
    let Clock = Clock.GetInstance
    let PC = PC.GetInstance
    let RegisterFile = RegisterFile.GetInstance
    let Mem = Memory.GetInstance
    let FunctionalUnits = FunctionalUnits.Reset(); FunctionalUnits.GetInstance
    
//    let getDisplayStrings () =
//        let memStr = if Clock.Cycles = 0 then Mem.ToString() else ""
//        [[
//            sprintf "%O" Clock
//            sprintf "%O" FunctionalUnits
//            CDB.Opt2String !cdb        
//            sprintf "%O" RegisterFile
//            memStr        
//        ] |> List.choose (fun s -> if s.Length > 1 then Some s else None) ]
    
    let output = ref List.empty<string list>
    
    let finished() = if Clock.Cycles = 0 then false else FunctionalUnits.Finished()
    
    let update(cdb) =
        FunctionalUnits.UpdateReservationStations(cdb)
        RegisterFile.Update(cdb)
        FunctionalUnits.ClearReservationStations()
//        output := !output @ getDisplayStrings()

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

    let dump() =
        printfn "\n%O\n%O" Clock PC
        printfn "%O" halt
        printfn "Finished? %A" (finished())
//        FunctionalUnits.DumpFU() |> printfn "%s"
//        FunctionalUnits.DumpRS() |> printfn "%s"
        
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
            log.AddEntry(sprintf "%O" Clock)
            Clock.Tic()
        log.Dump()

    let display() = ()

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
    let mutable log = List.empty<LogEntry>
    member l.AddEntry(?heading:string) = 
        let heading = defaultArg heading "\n"
        log <- log @ [LogEntry.MakeEntry]
    
    override l.ToString() =
        log |> List.map (sprintf "%O") |> Array.ofList |> Convert.lines2str
    
    member l.Dump() = printfn "%O" l

and LogEntry = LogEntry of ReservationStations' * Memory' * Registers' * Instructions' * CDB' with
    override x.ToString() = let rs,m,r,i,cdb = (LogEntry.Value x) in sprintf "\n%O\n%O\n%O\n%O\n%O\n%O" (Clock.GetInstance) rs m r i cdb
    static member Value (LogEntry(rs,m,r,i,cdb)) = rs,m,r,i,cdb
    static member MakeEntry =
        LogEntry(   ReservationStations'.MakeEntry,
                    Memory'.MakeEntry(),
                    Registers'.MakeEntry,
                    Instructions'.MakeEntry,
                    CDB'.MakeEntry)

and ReservationStations' = ReservationStations' of Entry with
    override rs.ToString() = match rs with ReservationStations' rs -> rs.ToString()
    static member MakeEntry = Display.FunctionalUnits.dumpReservationStations |> Entry.Create |> ReservationStations'

and Memory' = Memory' of Entry with
    override m.ToString() = "memory"
    static member MakeEntry = Display.Memory.dump8 >> Entry.Create >> Memory'

and Registers' = Registers' of Entry with
    override x.ToString() = "registers"
    static member MakeEntry = "" |> Entry.Create |> Registers'

and Instructions' = Instructions' of Entry with
    override x.ToString() = "executing instructions"
    static member MakeEntry = "" |> Entry.Create |> Instructions'

and CDB' = CDB' of Entry with
    override x.ToString() = "cdb"
    static member MakeEntry = "" |> Entry.Create |> CDB'

and Entry = Entry of string option with 
    override e.ToString() = match e with Entry e -> Convert.strOption2str e
    static member Create = Some >> Entry
