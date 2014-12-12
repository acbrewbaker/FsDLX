[<AutoOpen>]
module FsDLX.Tomasulo.Interface


type Simulator() =
    let clock = Clock.GetInstance
    member val x = 0 with get, set


    member s.Run() =



