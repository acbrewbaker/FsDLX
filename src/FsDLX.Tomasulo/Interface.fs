namespace FsDLX.Tomasulo

open System.Collections
open Config

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
//        while (not(halt.Issued) || not(finished())) && PC.Value < (Convert.hex2int "00000084") do
            cdb := write()
            execute()
            if not(halt.Issued) && not(branchInBranchUnit()) then
                stall <- Mem.[PC.Value] |> fetch |> issue
                if not(halt.Fetched) && not(stall) then PC.Increment()
            update(!cdb)
            //dump()
            Clock.Tic()
                            
    let runVerbose() = ()
    
    let display() = ()

    let runDebug() = ()
    
    member s.Run() = 
        match Config.Simulator.outputLevel with
        | Regular -> runRegular()
        | Verbose -> runVerbose()
        | Debug -> runDebug()


        



