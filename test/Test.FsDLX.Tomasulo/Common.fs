[<AutoOpen>]
module Test.FsDLX.Tomasulo.Common

open System
open System.IO
open NUnit.Framework
open NCrunch.Framework
open FsDLX.Tomasulo


let inline (@@) (a:string) (b:string) = Path.Combine(a,b)

let srcdir = 
    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
        Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
    else 
        Environment.CurrentDirectory

let inputdir = 
    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then
        srcdir @@ "../../Project2/Inputs"
    else
        srcdir @@ "../../../../Project2/Inputs"

let resetSingletons() =
    CDB.Reset()
    Clock.Reset()
    PC.Reset()
    Memory.Reset()
    RegisterFile.Reset()
    FPR.Reset()
    GPR.Reset()
    FunctionalUnits.Reset()



let strListToStr (l:string list) = (l |> List.map (fun l -> l.Trim()) |> List.fold (fun s r -> s + r + "\n") ("")).Trim()

let displayThenAssert (expected:string list) (actual:string list) =
    let e, a = strListToStr expected, strListToStr actual
    printfn "==== Expected ===="
    printfn "%s\n" e
    printfn "==== Actual ===="
    printfn "%s\n" a
    Assert.AreEqual(e,a)


let expectedCycle0 =
    [
        "Clock cycle: 0"
        "INTUNIT RESERVATION STATIONS"
        "IntUnit0  True  addi  00000000  00000000  <null>  <null>  00000003"
        "MEMORY"
        "0000:   20010003 20020006 00221820 44600001 44000000 00000000 00000000 00000000"
        "0020:   00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"  
    ]

let expectedCycle1 =
    [
        "Clock cycle: 1"
        "INTUNIT RESERVATION STATIONS"
        "IntUnit0  True  addi  00000000  00000000  <null>  <null>  00000003"
        "IntUnit1  True  addi  00000000  00000000  <null>  <null>  00000006"
        "EXECUTING: instruction in station IntUnit0"
    ]

let expectedCycle2 =
    [
        "Clock cycle: 2"
        "INTUNIT RESERVATION STATIONS"
        "IntUnit1  True  addi  00000000  00000000  <null>  <null>  00000006"
        "IntUnit2  True  add  00000003  00000000  <null>  IntUnit1  00000000"
        "EXECUTING: instruction in station IntUnit1"
        "CDB: result: 00000003 station: IntUnit0"
        "R0-R7:  00000000 00000003 IntUnit1 IntUnit2 00000000 00000000 00000000 00000000"
    ]

let expectedCycle3 =
    [
        "Clock cycle: 3"
        "TRAPUNIT RESERVATION STATIONS"
        "TrapUnit0  True  dumpGPR  00000000  00000000  IntUnit2  <null>  00000000"
        "CDB: result: 00000006 station: IntUnit1"
        "R0-R7:  00000000 00000003 00000006 IntUnit2 00000000 00000000 00000000 00000000"
    ]

let expectedCycle4 =
    [
        "Clock cycle: 4"
        "TRAPUNIT RESERVATION STATIONS"
        "TrapUnit0  True  dumpGPR  00000000  00000000  IntUnit2  <null>  00000000"
        "TrapUnit1  True  halt  00000000  00000000  <null>  <null>  00000000"
        "EXECUTING: instruction in station IntUnit2"
    ]

let expectedCycle5 =
    [
        "Clock cycle: 5"
        "CDB: result: 00000009 station: IntUnit2"
        "R0-R7:  00000000 00000003 00000006 00000009 00000000 00000000 00000000 00000000"
    ]

let expectedCycle6 =
    [
        "Clock cycle: 6"
        "EXECUTING: instruction in station TrapUnit0"
    ]

let expectedCycle7 =
    [
        "Clock cycle: 7"
        "EXECUTING: instruction in station TrapUnit1"
    ]

let expectedCycle8 =
    [
        "Clock cycle: 8"
    ]

