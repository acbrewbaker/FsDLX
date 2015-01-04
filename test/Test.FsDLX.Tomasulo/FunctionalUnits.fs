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

type RS2() =
    let kvp (r:ReservationStation) = (r.Name, r)
    let RS = 
        [|
            ReservationStation.ArrayInit Config.FunctionalUnit.IntegerUnit
            ReservationStation.ArrayInit Config.FunctionalUnit.TrapUnit
        |] |> Array.concat |> Array.map kvp |> Map.ofArray

    member rs2.Item
        with get i = RS.[i]




type InstructionStage =
    | Issue
    | Execute
    | Write

and IssueStage =
    | Issue of Instruction
    | LoadOrStore
    | LoadOnly
    | StoreOnly

    member is.Go(RS:RS) = is |> function
        | Issue instruction -> 
            let RegisterStat(i) = (RegisterStat.GetInstance instruction.Int).[i]
            let Regs(i) = (Regs.GetInstance instruction.Int).[i]
            let rd, rs, rt = instruction.rd, instruction.rs, instruction.rt
            RS.TryFindEmpty() |> function
            | Some r ->
                if      RegisterStat(rs).Qi.IsSome 
                then    RS.[r].Qj <- RegisterStat(rs).Qi
                else    RS.[r].Vj <- Regs(rs); RS.[r].Qj <- None
                
                if      RegisterStat(rt).Qi.IsSome
                then    RS.[r].Qk <- RegisterStat(rt).Qi
                else    RS.[r].Vk <- Regs(rt); RS.[r].Qk <- None
                
                RS.[r].Busy <- true
                RegisterStat(rd).Qi <- Some(RS.[r].Name)   

            | None -> ()
        | _ -> ()

and ExecuteStage =
    | FPOperation of Instruction
    | LoadStoreStep1
    | LoadStep2

    member es.Go(RS:RS, compute:int -> unit) = es |> function
        | FPOperation i ->
            RS.TryFindReady() |> function
            | Some r -> compute r
            | None -> ()
        | _ -> ()

and WriteStage =
    | FPOperationOrLoad
    | Store

    member ws.Go(RS:RS) = 
        let RegisterStat = RegisterFile.GetInstance
        let Regs = RegisterFile.GetInstance
        ws |> function
        | FPOperationOrLoad ->
            let cdb = CDB.GetInstance
            RS.TryFindResultReady() |> function
            | Some r ->
                for x = 0 to Config.Registers.RegCount - 1 do
                    if RegisterStat.[x].Qi.Value = RS.[r].Name then
                        Regs.[x] <- cdb.Result
                        RegisterStat.[x].Qi <- None
                    
                for x = 0 to RS.Length - 1 do
                    if      RS.[x].Qj.Value = RS.[r].Name 
                    then    RS.[x].Vj <- cdb.Result; RS.[x].Qj <- None

                    if      RS.[x].Qk.Value = RS.[r].Name
                    then    RS.[x].Vk <- cdb.Result; RS.[x].Qk <- None

                RS.[r].Busy <- false
                Some(cdb)
            | None -> None
        | Store ->
            let Mem = Memory.GetInstance
            RS.TryFindResultReady() |> function
            | Some r ->
                if      RS.[r].Qk.IsNone 
                then    
                    Mem.[RS.[r].A.Value] <- RS.[r].Vk
                    RS.[r].Busy <- false
                None
            | None -> None
                

type XUnit2 =
    {
        MaxCycles : int
        mutable RemainingCycles : int
        mutable Busy : bool
        mutable RSId : string
    }

    member xu.Cycle() = xu.RemainingCycles <- xu.RemainingCycles - 1


    override xu.ToString() =
        sprintf "RSId:  %O;    Busy: %O;    RemainingCycles: %d" xu.RSId xu.Busy xu.RemainingCycles

    static member TryFindNotBusy (xunits:XUnit2[]) =
        xunits |> Array.tryFindIndex (fun xunit -> not(xunit.Busy))

    static member Create(maxCycles) =
        { MaxCycles = maxCycles; RemainingCycles = maxCycles; Busy = false; RSId = "" }



type FuncUnit(rsRef:RSGroupRef) =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let xunits = Array.init cfg.unitCount (fun _ -> XUnit2.Create(cfg.maxCycles))

    let RS = RS.IntegerUnit rsRef

[<Test>]
let ``intUnit1`` () =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let xunits = Array.init cfg.unitCount (fun _ -> XUnit2.Create(cfg.maxCycles))
    let RS' = ReservationStation.ArrayInit cfg |> ref   
    let RS = RS' |> RS.IntegerUnit

    let cdb : (CDB option) ref = ref None

    let i0 = Instruction(ints.[0])
    let i1 = Instruction(ints.[1])
    let i2 = Instruction(ints.[2])

    let updateXUnit (xunit:XUnit2) (compute:int -> unit) =
        RS.TryFindReady() |> function
        | Some r ->
            xunit.RSId <- RS.[r].Name
            if  xunit.RemainingCycles = 0 &&
                not(RS.[r].ResultReady)
            then
                compute r
                RS.[r].ResultReady <- true
                xunit.Busy <- false
                xunit.RemainingCycles <- xunit.MaxCycles
        | None -> ()

    let issue (i:Instruction) =
        let opcode = i.Info.opcode
        let rd, rs, rt, imm = i.rd, i.rs, i.rt, i.imm
        
        let reg s = Convert.int2bits2reg i.Int s
        let immval (a,b) = Convert.int2bits2int i.Int a b

       
        let Regs = function
            | OperandReg.NONE -> Register.Init(0).Contents
            | OperandReg.GPR s -> GPR.GetInstance.[reg s].Contents
            | OperandReg.FPR s -> FPR.GetInstance.[reg s].Contents

        let RegisterStat = function
            | OperandReg.NONE -> Register.Init(0)
            | OperandReg.GPR s -> GPR.GetInstance.[reg s]
            | OperandReg.FPR s -> FPR.GetInstance.[reg s]  

        RS.TryFindNotBusy() |> function
        | Some r ->
            let RS(r) = RS.[r]

            if      RegisterStat(rs).Qi.IsSome 
            then    RS(r).Qj <- RegisterStat(rs).Qi
            else    RS(r).Vj <- Regs(rs); RS(r).Qj <- None
            
            if      RegisterStat(rt).Qi.IsSome
            then    RS(r).Qk <- RegisterStat(rt).Qi
            else    RS(r).Vk <- Regs(rt); RS(r).Qk <- None

            RS(r).Op <- Some opcode
            RS(r).Busy <- true
            RegisterStat(rd).Qi <- Some(cfg.rsPrefix + string r)

            RS(r).A <- imm |> function
            | Imm.NONE -> None
            | Imm.A imm -> Some(immval imm)

            false
        | _ -> true


    let execute() = 
        let compute (r:int) =
            let vj, vk, a = RS.[r].Vj, RS.[r].Vk, RS.[r].A
            RS.[r].Result <- RS.[r].Op |> function
                | Some op -> op.Name |> function
                    | "addi" -> if a.IsSome then vj + a.Value else vj
                    | "add" -> vj + vk
                    | _ -> failwith "failed compute instruction"
                | None -> failwith "tried to compute rs with null op"    
        XUnit2.TryFindNotBusy xunits |> function
            | Some u -> 
                RS.TryFindReady() |> function
                | Some r ->
                    xunits.[u].RSId <- RS.[r].Name
                    xunits.[u].Busy <- true
                    xunits.[u].Cycle()
                | None -> ()
            | None -> ()

        xunits |> Array.iter (fun xunit -> if xunit.Busy then updateXUnit xunit compute)

//        XUnit2.TryFindNotBusy xunits |> function
//        | Some u -> 
//            xunits.[u].Update RS compute
//        | None -> ()


    let write() =
        let cdb = CDB.GetInstance
        RS.TryFindResultReady() |> function
        | Some r ->
            RS.[r].ResultWritten <- true
            cdb.Result <- RS.[r].Result
            cdb.Src <- RS.[r].Name
            Some(cdb)
        | None -> None

    let updateReservationStations() = RS.Update()
    let clearReservationStations() = RS.Clear()
    let cc = ref 0
    let stall = ref false

    let display() =
        printfn "Clock Cycle: %d" !cc
        printfn "Stall: %A" !stall
        //xunits |> Array.iter (printfn "%O")
        printfn "%O" RS
        printfn "%O\n" !cdb

    let cycle(instruction:Instruction) =
        
        cdb := write()
        execute()
        let stall = issue instruction
        display()
        updateReservationStations()
        
        clearReservationStations()
//        display()
        cc := !cc + 1
        stall

    stall := cycle i0
    
    stall := cycle i1
    
    stall := cycle i2
    

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





