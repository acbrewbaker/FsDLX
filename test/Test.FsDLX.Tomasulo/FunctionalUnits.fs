module Test.FsDLX.Tomasulo.FunctionalUnits

open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

let lines =
    [
        "00000000: 20010003      #    addi r1, r0, 3"
        "00000004: 20020006      #    addi r2, r0, 6"
        "00000008: 00221820      #    add r3, r1, r2"
        "0000000c: 44600001      #    trap r3, 1              ;dump register"
        "00000010: 44000000      #    trap r0, 0"
    ]

let hex = lines |> List.map splitForHex

let ints = hex |> List.map (Convert.hex2int)

type XUnit =
    {
        MaxCycles : int
        mutable RemainingCycles : int
        mutable Busy : bool
        RSId : int
    }

    static member Create(maxCycles) =
        { MaxCycles = maxCycles; RemainingCycles = maxCycles; Busy = false; RSId = -1 }

type FuncUnit(rsRef:RSGroupRef) =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let xunits = Array.init cfg.unitCount (fun _ -> XUnit(cfg.maxCycles))

    let RS = RS.IntegerUnit rsRef

[<Test>]
let ``intUnit1`` () =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let xunits = Array.init cfg.unitCount (fun _ -> XUnit(cfg.maxCycles))
    let RS' = ReservationStation.ArrayInit cfg |> ref   
    let RS = RS' |> RS.IntegerUnit

    RS.TryFindReady() |> function
    | Some r -> printfn "%d" r
    | None -> printfn "none"

[<Test>]
let ``intUnit2`` () =
    let RS' = ReservationStation.ArrayInit Config.FunctionalUnit.IntegerUnit
    let rsRef = ref RS'
    let intUnit = IntegerUnit.GetInstance rsRef
    let iuRS = [| rsRef |> RS.IntegerUnit |]

    let i0 = Instruction(ints.[0])
    let i1 = Instruction(ints.[1])

    let uid = ref 0

    let mutable cdb : CDB option = None

    let updateReservationStations() = iuRS |> RS.Update

    let write() = intUnit.Write() // |> Array.tryPick (fun u -> u.Write())

    let execute() = intUnit.Execute() //|> Array.iter (fun u -> u.Execute())

    let issue (instruction:Instruction) =
        intUnit.IsBusy() |> function
        | false ->
            intUnit.Insert i0
        | _ -> true
        

    cdb <- write()
    printfn "cdb: %O" cdb
    execute()
    printfn "%s" (intUnit.Dump())
    let stall = issue(i0)
    printfn "stall?? : %A" stall



    cdb <- write()
    printfn "cdb: %O" cdb
    execute()
    printfn "%s" (intUnit.Dump())
    let stall = issue(i1)
    printfn "stall?? : %A" stall





