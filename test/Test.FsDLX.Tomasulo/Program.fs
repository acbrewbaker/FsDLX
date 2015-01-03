// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open FsDLX.Common
open FsDLX.Tomasulo
open Test.FsDLX.Tomasulo
//
[<EntryPoint>]
let main argv = 
//    let input, verbose = 
//        inputdir @@ "add.hex",
//        true
//    let simulator = Simulator(input, verbose)
//
//    simulator.Run()
    FunctionalUnits.``intUnit2``()
    0 // return an integer exit code
