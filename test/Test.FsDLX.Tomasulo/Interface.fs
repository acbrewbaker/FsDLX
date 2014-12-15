module Test.FsDLX.Tomasulo.Interface

open NUnit.Framework
open FsUnit

open FsDLX.Tomasulo


[<Test>]
let ``simple add.hex`` () =
    let input, verbose = 
        inputdir @@ "add.hex",
        true
    let simulator = Simulator(input, verbose)

    simulator.Run()
