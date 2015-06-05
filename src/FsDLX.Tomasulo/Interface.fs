namespace FsDLX.Tomasulo

open System.Collections
open FsDLX.Common


type Simulator(input:string, verbose:bool) =
    let cdb : CDB option ref = ref None
    let Clock = Clock.GetInstance
    let PC = PC.GetInstance
    let RegisterFile = RegisterFile.GetInstance
    let Mem = Memory.GetInstance
    let FunctionalUnits = FunctionalUnits.GetInstance
    
    let getDisplayStrings () =
        let memStr = if Clock.Cycles = 0 then Mem.ToString() else ""
        [[
            sprintf "%O" Clock
            sprintf "%O" FunctionalUnits
            CDB.Opt2String !cdb        
            sprintf "%O" RegisterFile
            memStr        
        ] |> List.choose (fun s -> if s.Length > 1 then Some s else None) ]
    
    let output = ref List.empty<string list>
    
    let finished() = 
        if Clock.Cycles = 0 then false else FunctionalUnits.Finished()

    let updateReservationStations(cdb) = 
        FunctionalUnits.UpdateReservationStations()
        RegisterFile.Update(cdb)

    let clearReservationStations() = FunctionalUnits.ClearReservationStations()
    
    let update(cdb) =
        updateReservationStations(cdb)
        clearReservationStations()
        output := !output @ getDisplayStrings()

    let branchInBranchUnit() = false

    let write, execute, issue =
        FunctionalUnits.Write,
        FunctionalUnits.Execute,
        FunctionalUnits.Issue

    //Mem.Load(inputdir @@ "add.hex")
        
    let runRegular() =
        Mem.Load(input)
        let mutable halt, stall = false, false
        
        let fetch halt = 
            let __HALT__ = Instruction.Trap(Convert.hex2int "44000000", Instruction.HALT)
            if not(halt) then Mem.[PC.Value] |> Instruction.OfInstructionInt else __HALT__
        
        while not(halt) && not(finished()) do
            let gpr = GPR.GetInstance.Regs().[0..15]
            let rs = FunctionalUnits.ReservationStations
            cdb := write()
            halt <- execute()
            if not(halt) && not(branchInBranchUnit()) then
                let instruction = fetch halt //Mem.[PC.Value] |> Instruction.OfInstructionInt
                let h = instruction.AsHex
                printfn "\n*****  Instruction: %O  *****\n" instruction
                stall <- issue(instruction)
                if not(halt) && not(stall) then PC.Increment()
            update(!cdb)
            
            Clock.Tic()
                    
    let runVerbose() = ()
    
    let display() = ()

    let runDebug() = ()
    
    member s.Run() = Config.Simulator.outputLevel |> function
        | Regular -> runRegular()
        | Verbose -> runVerbose()
        | Debug -> runDebug()


        



