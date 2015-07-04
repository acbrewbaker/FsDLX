module Test.FsDLX.Tomasulo.FunctionalUnits

open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

let lines() =
    [
        "00000000: 20010003      #    addi r1, r0, 3"
        "00000004: 20020006      #    addi r2, r0, 6"
        "00000008: 00221820      #    add r3, r1, r2"
        "0000000c: 44600001      #    trap r3, 1              ;dump register"
        "00000010: 44000000      #    trap r0, 0"
    ]

let expectedOutput() =
    [
        expectedCycle0
        expectedCycle1
        expectedCycle2
        expectedCycle3
        expectedCycle4
        expectedCycle5
        expectedCycle6
        expectedCycle7
        expectedCycle8
    ]

let run (stopCycle:int) =
    resetSingletons()
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
    let mutable halt = false

    let finished() = 
        if Clock.Cycles = 0 then false else FunctionalUnits.Finished()

    let updateReservationStations(cdb) = 
        FunctionalUnits.UpdateReservationStations(cdb)
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

    Mem.Load(inputdir @@ "add.hex")
    let mutable stall = false
    while not(halt) && not(finished()) do
        cdb := write()
        execute()
        if not(halt) && not(branchInBranchUnit()) then
            let instruction = Mem.[PC.Value] |> Instruction.OfInstructionInt
            //stall <- issue(instruction)
            if not(halt) && not(stall) then PC.Increment()
        update(!cdb)
        halt <- Clock.Cycles = stopCycle
        Clock.Tic()
        
    !output
    

[<Test>]
let ``cycle0`` () =
    let stopCycle = 0
    let output = run stopCycle
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle1`` () =
    let stopCycle = 1
    let output = run stopCycle
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle2`` () =
    let stopCycle = 2
    let output = run stopCycle
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert


[<Test>]
let ``cycle3`` () =
    let expectedOutput = expectedOutput()
    let stopCycle = 3
    let output = run stopCycle
    (expectedOutput.[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle4`` () =
    let stopCycle = 4
    let output = run stopCycle
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle5`` () =
    let stopCycle = 5
    let output = run stopCycle
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle6`` () =
    let stopCycle = 6
    let output = run stopCycle
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle7`` () =
    let stopCycle = 7
    let output = run stopCycle
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle8`` () =
    let stopCycle = 8
    let output = run stopCycle
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert