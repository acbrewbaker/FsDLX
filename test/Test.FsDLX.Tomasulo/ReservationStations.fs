module Test.FsDLX.Tomasulo.ReservationStations

open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

//[<Test>]
//let ``rs try find ready`` () =
//    let RS' =
//        Array.init 2 (fun i -> ReservationStation.Init ("RS" + string i))
//        |> ref
//
//    let RS = RS' |> RS.IntegerUnit
//
//    RS.TryFindReady() |> function
//    | Some r -> printfn "ready: %d" r
//    | None -> printfn "none dood"


//[<Test>]
//let ``rs try find not busy`` () =
//    let RS' =
//        Array.init 2 (fun i -> ReservationStation.Init ("RS" + string i))
//        |> ref
//
//    let RS = RS' |> RS.IntegerUnit
//
//    RS.TryFindNotBusy() |> function
//    | Some r -> printfn "===READY===\n%O" RS.[r]
//    | None -> printfn "none dood"