module Test.FsDLX.Tomasulo.Memory

open NUnit.Framework
open NCrunch.Framework
open FsUnit

open FsDLX.Tomasulo

let size = 1000

[<Test>]
let ``simple load test`` () =
    let file = inputdir @@ "add.hex"
    let memory = Memory.GetInstance size
    memory.Load(file)
    memory.Dump()
