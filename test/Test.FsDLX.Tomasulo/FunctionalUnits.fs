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

let regId i s = Convert.int2bits2reg i s
let immVal i a b = Convert.int2bits2int i a b

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

    let i0 = Instruction(ints.[0])
    let i1 = Instruction(ints.[1])

    let insert (opcode:Opcode) (rs:int) (rt:int) (rd:int) =
        let gpr = GPR.GetInstance
        RS.TryFindNotBusy() |> function
        | Some r ->
            RS.[r].Busy <- true
            RS.[r].Op <- Some opcode
            RS.[r].A <- Some rd

            if gpr.[rs].IsAvailable() then
                RS.[r].Vj <- gpr.[rs].Contents
            else
                RS.[r].Qj <- gpr.[rs].Qi
            
            gpr.[rt].Qi <- Some(cfg.rsPrefix + string r)

            false
        | _ -> true

    let rs, rt, rd, imm = i0.Info.rs, i0.Info.rt, i0.Info.rd, i0.Info.imm

    let issue (i:Instruction) =
        let rs, rt, rd, imm = i.Info.rs, i.Info.rt, i.Info.rd, i.Info.imm
        (rd,rs,rt,imm) |> function
        | DstReg.GPR rd, S1Reg.GPR rs, S2Reg.NONE, Imm.A(a,b) ->
            let rd, rs, imm =
                regId i.Int rd,
                regId i.Int rs,
                immVal i.Int a b
            printfn "rd, rs, imm  ===> %d, %d, %d" rd rs imm
            insert (i.Info.opcode) rs rd imm
        | _ -> false

    let stall = issue i0
    printfn "stall : %A" stall
    printfn "%O" RS

    let stall = issue i1
    printfn "stall : %A" stall
    printfn "%O" RS



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





