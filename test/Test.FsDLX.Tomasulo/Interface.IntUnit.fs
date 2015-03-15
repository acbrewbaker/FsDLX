module Test.FsDLX.Tomasulo.Interface

open NUnit.Framework
open FsUnit

open FsDLX.Tomasulo


///<IntUnitTests>
let [<Test>] ``intUnit1.hex`` () =
    let simulator = Simulator(inputdir @@ @"intUnit1.hex",false)
    simulator.Run()

let [<Test>] ``intUnit2.hex`` () =
    let simulator = Simulator(inputdir @@ @"intUnit2.hex",false)
    simulator.Run()

let [<Test>] ``intUnit3.hex`` () =
    let simulator = Simulator(inputdir @@ @"intUnit3.hex",false)
    simulator.Run()

let [<Test>] ``intUnit4.hex`` () =
    let simulator = Simulator(inputdir @@ @"intUnit4.hex",false)
    simulator.Run()

let [<Test>] ``intUnit5.hex`` () =
    let simulator = Simulator(inputdir @@ @"intUnit5.hex",false)
    simulator.Run()

let [<Test>] ``intUnit6.hex`` () =
    let simulator = Simulator(inputdir @@ @"intUnit6.hex",false)
    simulator.Run()

let [<Test>] ``intUnit7.hex`` () =
    let simulator = Simulator(inputdir @@ @"intUnit7.hex",false)
    simulator.Run()

let [<Test>] ``intUnit8.hex`` () =
    let simulator = Simulator(inputdir @@ @"intUnit8.hex",false)
    simulator.Run()

///</IntUnitTests>
