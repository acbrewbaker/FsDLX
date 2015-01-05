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
 
let strListToStr l = (l |> List.fold (fun s r -> s + r + "\n") ("")).Trim()

let displayThenAssert (expected:string list) (actual:string list) =
    let e, a = strListToStr expected, strListToStr actual
    printfn "==== Expected ===="
    printfn "%s\n" e
    printfn "==== Actual ===="
    printfn "%s\n" a
    Assert.AreEqual(e,a)


let expectedCycle0 =
    [
        "Clock cycle: 0"
        "INTUNIT RESERVATION STATIONS"
        "IntUnit0  True  addi  00000000  00000000  <null>  <null>  00000003"
        "MEMORY"
        "0000:   20010003 20020006 00221820 44600001 44000000 00000000 00000000 00000000"
        "0020:   00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"  
    ]

let expectedCycle1 =
    [
        "Clock cycle: 1"
        "INTUNIT RESERVATION STATIONS"
        "IntUnit0  True  addi  00000000  00000000  <null>  <null>  00000003"
        "IntUnit1  True  addi  00000000  00000000  <null>  <null>  00000006"
        "EXECUTING: instruction in station IntUnit0"
    ]

let expectedCycle2 =
    [
        "Clock cycle: 2"
        "INTUNIT RESERVATION STATIONS"
        "IntUnit1  True  addi  00000000  00000000  <null>  <null>  00000006"
        "IntUnit2  True  add  00000003  00000000  <null>  IntUnit1 00000000"
        "EXECUTING: instruction in station IntUnit1"
        "CDB: result: 00000003 station: IntUnit0"
        "R0-R7:   00000000 00000003 IntUnit1 IntUnit2 00000000 00000000 00000000 00000000"
    ]

let expectedCycle3 =
    [
        "Clock cycle: 3"
        "TRAPUNIT RESERVATION STATIONS"
        "TrapUnit0    true dumpGPR 00000000 00000000 IntUnit2     null 00000000"
        "CDB: result: 00000006 station: IntUnit1"
        "R0-R7:   00000000 00000003 00000006 IntUnit2 00000000 00000000 00000000 00000000"
    ]

let expectedCycle4 =
    [
        "Clock cycle: 4"
        "TRAPUNIT RESERVATION STATIONS"
        "TrapUnit0    true dumpGPR 00000000 00000000 IntUnit2     null 00000000"
        "TrapUnit1    true    halt 00000000 00000000     null     null 00000000"
        "EXECUTING: instruction in station IntUnit2"
    ]

let expectedCycle5 =
    [
        "Clock cycle: 5"
        "CDB: result: 00000009 station: IntUnit2"
        "R0-R7:   00000000 00000003 00000006 00000009 00000000 00000000 00000000 00000000"
    ]

let expectedCycle6 =
    [
        "Clock cycle: 6"
        "EXECUTING: instruction in station TrapUnit0"
    ]

let expectedCycle7 =
    [
        "Clock cycle: 7"
        "EXECUTING: instruction in station TrapUnit1"
    ]

let expectedCycle8 =
    [
        "Clock cycle: 8"
    ]

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
let ``cycle0`` () =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let cc = ref 0
    let cdb : (CDB option) ref = ref None
    let RS' = ReservationStation.ArrayInit cfg |> ref
    let RS = RS.IntegerUnit RS'
    let intUnit = IntegerUnit.GetInstance RS'

    let i0 = Instruction(ints.[0])

    let memory = Memory.GetInstance
    memory.Load(inputdir @@ "add.hex")

    cdb := intUnit.Write()
    intUnit.Execute()
    intUnit.Insert i0 |> ignore
    RS.Update(!cdb)
    RS.Clear()

    let clockStr() = sprintf "Clock cycle: %d" !cc
    let rsInfo() = intUnit.ToString()
    let memDump() = memory.ToString()

    let actualCycle0 =
        [
            clockStr()
            rsInfo()
            memDump()
        ]

    (expectedCycle0, actualCycle0) ||> displayThenAssert

[<Test>]
let ``cycle1`` () =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let cc = ref 0
    let cdb : (CDB option) ref = ref None
    let RS' = ReservationStation.ArrayInit cfg |> ref
    let RS = RS.IntegerUnit RS'
    let intUnit = IntegerUnit.GetInstance RS'

    let i0 = Instruction(ints.[0])
    let i1 = Instruction(ints.[1])

    let memory = Memory.GetInstance
    memory.Load(inputdir @@ "add.hex")

    cdb := intUnit.Write()
    intUnit.Execute()
    intUnit.Insert i0 |> ignore
    RS.Update(!cdb)
    RS.Clear()
    cc := !cc + 1
    
    cdb := intUnit.Write()
    intUnit.Execute()
    intUnit.Insert i1 |> ignore
    RS.Update(!cdb)
    RS.Clear()

    let clockStr() = sprintf "Clock cycle: %d" !cc
    let rsInfo() = intUnit.ToString()

    let actualCycle1 =
        [
            clockStr()
            rsInfo()
        ]

    (expectedCycle1, actualCycle1) ||> displayThenAssert

[<Test>]
let ``cycle2`` () =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let cc = ref 0
    let cdb : (CDB option) ref = ref None
    let RS' = ReservationStation.ArrayInit cfg |> ref
    let RS = RS.IntegerUnit RS'
    let intUnit = IntegerUnit.GetInstance RS'

    let i0 = Instruction(ints.[0])
    let i1 = Instruction(ints.[1])
    let i2 = Instruction(ints.[2])

    let memory = Memory.GetInstance
    memory.Load(inputdir @@ "add.hex")

    cdb := intUnit.Write()
    intUnit.Execute()
    intUnit.Insert i0 |> ignore
    RS.Update(!cdb)
    RS.Clear()
    cc := !cc + 1
    
    cdb := intUnit.Write()
    intUnit.Execute()
    intUnit.Insert i1 |> ignore
    RS.Update(!cdb)
    RS.Clear()
    cc := !cc + 1

    cdb := intUnit.Write()
    intUnit.Execute()
    intUnit.Insert i2 |> ignore
    RS.Update(!cdb)
    RegisterFile.GetInstance.Update(!cdb)
    RS.Clear()

    let clockStr() = sprintf "Clock cycle: %d" !cc
    let rsInfo() = intUnit.ToString()
    let cdbInfo() = CDB.Opt2String !cdb
    let regInfo() = GPR.GetInstance.ToString()

    let actualCycle2 =
        [
            clockStr()
            rsInfo()
            cdbInfo()
            regInfo()
        ]

    (expectedCycle2, actualCycle2) ||> displayThenAssert

[<Test>]
let ``cycle3`` () = ()

[<Test>]
let ``cycle4`` () = ()

[<Test>]
let ``cycle5`` () = ()

[<Test>]
let ``cycle6`` () = ()

[<Test>]
let ``cycle7`` () = ()

[<Test>]
let ``cycle8`` () = ()


[<Test>]
let ``intUnit0``() =
    let i0 = Instruction(ints.[0])
    let i1 = Instruction(ints.[1])
    let i2 = Instruction(ints.[2])

    // for the issuing instruction:
    // rd - the destination
    // rs - source register number
    // rt - source register number
    // imm - sign-extended immediate field
    let rd, rs, rt, imm =
        i0.rd, i0.rs, i0.rt, i0.imm

    // r - reservation station or buffer that the instruction is assigned to
    let r = 0

    // RS - reservation station data structure
    let RS' = ReservationStation.ArrayInit Config.FunctionalUnit.IntegerUnit |> ref
    let RS = RS' |> RS.IntegerUnit

    let X = Array.init Config.FunctionalUnit.IntegerUnit.unitCount (fun _ -> XUnit2.Create Config.FunctionalUnit.IntegerUnit.maxCycles)
    // result - value returned by FP unit or load unit
    
    // RegisterStat - register status data structure
    // Regs - register file
    let issue (instruction:Instruction) =
        let RegisterStat(x) = (RegisterStat.GetInstance instruction.Int).[x]
        let Regs(x) = (Regs.GetInstance instruction.Int).[x]

        RS.TryFindEmpty() |> function
        | Some r ->
        // When an instruction is issued, the destination register has its Qi field set to the
        // number of the buffer or reservation station to which the instruction is issued.
        // If the operands are available in the registers, they are stored in the V fields.
        // Otherwise, the Q fields are set to indicate the reservation station that will produce
        // the values needed as source operands.
            if      RegisterStat(rs).Qi.IsSome
            then    RS.[r].Qj <- RegisterStat(rs).Qi
            else    RS.[r].Vj <- Regs(rs); RS.[r].Qj <- None

            if      RegisterStat(rt).Qi.IsSome
            then    RS.[r].Qk <- RegisterStat(rt).Qi
            else    RS.[r].Vk <- Regs(rt); RS.[r].Qk <- None

            RS.[r].Busy <- true; RegisterStat(rd).Qi <- RS.[r].Name |> Some
        | None -> ()

    // The instruction waits at the reservation station until both its operands are available,
    // indicated by zero in the Q fields.  The Q fields are set to zero either when this instruction
    // is issed or when an instruction on which this instruction depends complete and does its write
    // back.
    let execute() =
        let compute (r:int) =
            let vj, vk, a = RS.[r].Vj, RS.[r].Vk, RS.[r].A
            RS.[r].Result <- RS.[r].Op |> function
                | Some op -> op.Name |> function
                    | "addi" -> vj + a
                    | "add" -> vj + vk
                    | _ -> failwith "failed compute instruction"
                | None -> failwith "tried to compute rs with null op"  
        
        X |> Array.iter (fun u ->
            if not(u.Busy) then
                u.Busy <- true
                u.Cycle()
            else
                if u.RemainingCycles > 0 then u.Cycle()
                if u.RemainingCycles = 0 && not(RS.[u.RSId].ResultReady) then
                    compute 0 //u.RSId
                    RS.[u.RSId].ResultReady <- true
                    u.Busy <- false
                    u.RemainingCycles <- u.MaxCycles)
//        for r = 0 to RS.Length - 1 do
//            (RS.[r].Qj, RS.[r].Qk) |> function
//            | None, None ->
    ()
        //  When an instruction has finished execution and the CDB is available, it can do its write back.
        // All the buffers, registers, and reservation stations whose values of Qj or Qk are the same as
        // the completing reservation station update their values from the CDB and mark the Q fields to
        // indicate that values have been received.  Thus, the CDB can broadcast its result to many
        // destinations in a single clock cycle, and if the waiting instructions have their operands they
        // can all begin execution on the next clock cycle.  Loads go through two steps in execute, and
        // stores perform slightly differently during write result, where they may have to wait for the
        // value to store.  Remember that, to preserve exception behavior, instruction should not be allowed
        // to execute if a branch that is earlier in program order has not yet completed.  Because any
        // concept of program order is not maintained after the issue stage, this restriction is usually
        // implemented by preventing any instruction from leaving the issue step, if there is a pending
        // branch already in the pipeline.

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

        let RegisterStat(x) = (RegisterStat.GetInstance i.Int).[x]
        let Regs(x) = (Regs.GetInstance i.Int).[x]

        RS.TryFindNotBusy() |> function
        | Some r ->
            let RS(r:int) = RS.[r]

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
            | Imm.NONE -> 0
            | Imm.A imm -> immval imm

            false
        | _ -> true


    let execute() = 
        let compute (r:int) =
            let vj, vk, a = RS.[r].Vj, RS.[r].Vk, RS.[r].A
            RS.[r].Result <- RS.[r].Op |> function
                | Some op -> op.Name |> function
                    | "addi" -> vj + a
                    | "add" -> vj + vk
                    | _ -> failwith "failed compute instruction"
                | None -> failwith "tried to compute rs with null op"    
//        for r = 0 to RS.Length - 1
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

    let updateReservationStations = RS.Update
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
        updateReservationStations(!cdb)
        
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

    let updateReservationStations cdb = RS.Update(iuRS, cdb)

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





