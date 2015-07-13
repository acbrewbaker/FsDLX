module Test.FsDLX.Tomasulo.VerboseOutput


open System
open System.IO
open NUnit.Framework
open FsDLX.Tomasulo

[<Test>]
let ``foo`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit6.hex",false)
    simulator.Run()