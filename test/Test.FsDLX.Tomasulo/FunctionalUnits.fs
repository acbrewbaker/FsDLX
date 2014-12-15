module Test.FsDLX.Tomasulo.FunctionalUnits

open NUnit.Framework
open FsDLX.Tomasulo

[<Test>]
let ``init test`` () =
    let fu = FU.ArrayInit Config.FU.IntegerUnit (fun _ -> IntegerUnit() :> FU)
    //let iu = Array.init Config.FU.IntegerUnit.XUnitCount (fun _ -> IntegerUnit())

    printfn "%A" fu

