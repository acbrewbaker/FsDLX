module Test.FsDLX.Tomasulo.FunctionalUnits

open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

let lines() =
    [
        "00000000: 20010003      #    addi r1, r0, 3"
        "00000004: 20020006      #    addi r2, r0, 6"
        "00000008: 00221820      #    add r3, r1, r2"
        "0000000c: 44600001      #    trap r3, 1              ;dump register"
        "00000010: 44000000      #    trap r0, 0"
    ]

let hex() = lines() |> List.map splitForHex

let ints() = hex() |> List.map (Convert.hex2int)

let regId i s = Convert.int2bits2reg i s
let immVal i a b = Convert.int2bits2int i a b


//let getDisplayStrings (cdb:CDB option) (funits:FunctionalUnits) =
//    let memStr = if Clock.GetInstance.Cycles = 0 then Memory.GetInstance.ToString() else ""
//    [
//        sprintf "%O" (Clock.GetInstance)
//        sprintf "%O" funits
//        CDB.Opt2String cdb
//        
//        sprintf "%O" RegisterFile.GetInstance
//        memStr        
//    ] |> List.choose (fun s -> if s.Length > 1 then Some s else None)



let expectedOutput() =
    [
        expectedCycle0
        expectedCycle1
        expectedCycle2
        expectedCycle3
        expectedCycle4
        expectedCycle5
        expectedCycle6
        expectedCycle7
        expectedCycle8
    ]

//let run (stopCycle:int) (clock:Clock) (pc:PC) (registerFile:RegisterFile) (mem:Memory) (getDisplayStrings: CDB option -> FunctionalUnits -> string list) =
let run (stopCycle:int) = // (getDisplayStrings: CDB option -> FunctionalUnits -> string list) =
    resetSingletons()
    let cdb : CDB option ref = ref None
    let Clock = Clock.GetInstance
    let PC = PC.GetInstance
    let RegisterFile = RegisterFile.GetInstance
    let Mem = Memory.GetInstance
    let FunctionalUnits = FunctionalUnits.GetInstance
 


    let getDisplayStrings () =
        let memStr = if Clock.Cycles = 0 then Mem.ToString() else ""
        [[
            sprintf "%O" Clock
            sprintf "%O" FunctionalUnits
            CDB.Opt2String !cdb        
            sprintf "%O" RegisterFile
            memStr        
        ] |> List.choose (fun s -> if s.Length > 1 then Some s else None) ]
    
    let output = ref List.empty<string list>
    let mutable halt = false

    let finished() = 
        if Clock.Cycles = 0 then false else FunctionalUnits.AllFinished()

    let updateReservationStations(cdb) = 
        FunctionalUnits.UpdateReservationStations()
        RegisterFile.Update(cdb)

    let clearReservationStations() = FunctionalUnits.ClearReservationStations()
    
    let update(cdb) =
        updateReservationStations(cdb)
        //output := !output @ getDisplayStrings()
        clearReservationStations()
        output := !output @ getDisplayStrings()

    let branchInBranchUnit() = false

    let write() = FunctionalUnits.Write() //.All |> Array.tryPick (fun u -> u.Write())
    
    let execute() = FunctionalUnits.Execute(); FunctionalUnits.Halt
        //FunctionalUnits.All |> Array.iter (fun u -> u.Execute()); FunctionalUnits.Halt

    let issue instruction = FunctionalUnits.Issue instruction; FunctionalUnits.Stall

    Mem.Load(inputdir @@ "add.hex")
    let mutable stall = false
    while not(halt) && not(finished()) do
        cdb := write()
        //printfn "HALT: %A" halt
        printfn "\n---------------------------------------------- %O" Clock
        halt <- execute()
        //printfn "HALT: %A" halt
        //output <- output @ [getDisplayStrings cdb FunctionalUnits]
        if not(halt) && not(branchInBranchUnit()) then
//            printfn "Issuing: %A" (lines().[PC.Value])
            let instruction = Mem.[PC.Value] |> Instruction.ofInstructionInt
            stall <- issue(instruction)
            if not(stall) then PC.Increment()
        //FunctionalUnits.DumpLastInsert()        
        //FunctionalUnits.Dump()
        update(!cdb)
        halt <- Clock.Cycles = stopCycle
        Clock.Tic()
        
    !output

[<Test>]
let ``xunit`` () =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let x = XUnit(cfg.maxCycles)
    printfn "%O" x


[<Test>]
let ``integer unit`` () =
    let cfg = Config.FunctionalUnit.IntegerUnit
    let RS' = ReservationStation.ArrayInit cfg |> ref
    let iu = IntegerUnit.GetInstance RS'

    let i0 = Instruction.ofInstructionInt(ints().[0])
    let i1 = Instruction.ofInstructionInt(ints().[1])
    let i2 = Instruction.ofInstructionInt(ints().[2])
    
    iu.Insert i0 |> ignore

    printfn "%O" iu

[<Test>]
let ``cycle0`` () =
    let stopCycle = 0
    let output = 
        run stopCycle
            //getDisplayStrings
        

    //printfn "Output %A" output
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle1`` () =
    let stopCycle = 1
    let output = 
        run stopCycle
            //getDisplayStrings
    //printfn "Output %A" output
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle2`` () =
    let stopCycle = 2
    let output = 
        run stopCycle
            //getDisplayStrings
    //printfn "Output %A" output
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert


[<Test>]
let ``cycle3`` () =
    let expectedOutput = expectedOutput()
    let stopCycle = 3
    let output = 
        run stopCycle
            //getDisplayStrings
    //printfn "Output %A" output
    (expectedOutput.[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle4`` () =
    let stopCycle = 4
    let output = 
        run stopCycle
            //getDisplayStrings
    //printfn "Output %A" output
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle5`` () =
    let stopCycle = 5
    let output = 
        run stopCycle
            //getDisplayStrings
    //printfn "Output %A" output
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle6`` () =
    let stopCycle = 6
    let output = 
        run stopCycle
            //getDisplayStrings
    //printfn "Output %A" output
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle7`` () =
    let stopCycle = 7
    let output = 
        run stopCycle
            //getDisplayStrings
    //printfn "Output %A" output
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert

[<Test>]
let ``cycle8`` () =
    let stopCycle = 8
    let output = 
        run stopCycle
            //getDisplayStrings
    //printfn "Output %A" output
    (expectedOutput().[stopCycle], output.[stopCycle]) ||> displayThenAssert


[<Test>]
let ``intUnit0``() =
    let i0 = Instruction.ofInstructionInt(ints().[0])
    let i1 = Instruction.ofInstructionInt(ints().[1])
    let i2 = Instruction.ofInstructionInt(ints().[2])

    // for the issuing instruction:
    // rd - the destination
    // rs - source register number
    // rt - source register number
    // imm - sign-extended immediate field
    let rd, rs, rt, imm =
        i0.DstReg, 
        i0.S1Reg, 
        i0.S2Reg, 
        i0.Immediate

    // r - reservation station or buffer that the instruction is assigned to
    let r = 0

    // RS - reservation station data structure
    let RS' = ReservationStation.ArrayInit Config.FunctionalUnit.IntegerUnit |> ref
    let RS = RS' |> RS.IntegerUnit

    //let X = Array.init Config.FunctionalUnit.IntegerUnit.unitCount (fun _ -> XUnit2.Create Config.FunctionalUnit.IntegerUnit.maxCycles)
    // result - value returned by FP unit or load unit
    
    // RegisterStat - register status data structure
    // Regs - register file
    let issue (instruction:Instruction) =
        let RegisterStat(x) = (RegisterStat.GetInstance instruction.asInt).[x]
        let Regs(x) = (Regs.GetInstance instruction.asInt).[x]

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
//        let compute (r:int) =
//            let vj, vk, a = RS.[r].Vj, RS.[r].Vk, RS.[r].A
//            RS.[r].Result <- RS.[r].Op |> function
//                | Some op -> op.Name |> function
//                    | "addi" -> vj + a
//                    | "add" -> vj + vk
//                    | _ -> failwith "failed compute instruction"
//                | None -> failwith "tried to compute rs with null op"  
        ()
//        X |> Array.iter (fun u ->
//            if not(u.Busy) then
//                u.Busy <- true
//                u.Cycle()
//            else
//                if u.RemainingCycles > 0 then u.Cycle()
//                if u.RemainingCycles = 0 && not(RS.[u.RSId].ResultReady) then
//                    compute 0 //u.RSId
//                    RS.[u.RSId].ResultReady <- true
//                    u.Busy <- false
//                    u.RemainingCycles <- u.MaxCycles)
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

//[<Test>]
//let ``intUnit1`` () =
//    let cfg = Config.FunctionalUnit.IntegerUnit
//    let xunits = Array.init cfg.unitCount (fun _ -> XUnit2.Create(cfg.maxCycles))
//    let RS' = ReservationStation.ArrayInit cfg |> ref   
//    let RS = RS' |> RS.IntegerUnit
//
//    let cdb : (CDB option) ref = ref None
//
//    let i0 = Instruction.ofInstructionInt(ints.[0])
//    let i1 = Instruction.ofInstructionInt(ints.[1])
//    let i2 = Instruction.ofInstructionInt(ints.[2])
//
//    let updateXUnit (xunit:XUnit2) (compute:int -> unit) =
//        RS.TryFindReady() |> function
//        | Some r ->
//            xunit.RSId <- RS.[r].Name
//            if  xunit.RemainingCycles = 0 &&
//                not(RS.[r].ResultReady)
//            then
//                compute r
//                RS.[r].ResultReady <- true
//                xunit.Busy <- false
//                xunit.RemainingCycles <- xunit.MaxCycles
//        | None -> ()
//
//    let issue (i:Instruction) =
//        let opcode = i.Info.opcode
//        let rd, rs, rt, imm = i.rd, i.rs, i.rt, i.imm
//        
//        let reg s = Convert.int2bits2reg i.Int s
//        let immval (a,b) = Convert.int2bits2int i.Int a b
//
//        let RegisterStat(x) = (RegisterStat.GetInstance i.Int).[x]
//        let Regs(x) = (Regs.GetInstance i.Int).[x]
//
//        RS.TryFindNotBusy() |> function
//        | Some r ->
//            let RS(r:int) = RS.[r]
//
//            if      RegisterStat(rs).Qi.IsSome 
//            then    RS(r).Qj <- RegisterStat(rs).Qi
//            else    RS(r).Vj <- Regs(rs); RS(r).Qj <- None
//            
//            if      RegisterStat(rt).Qi.IsSome
//            then    RS(r).Qk <- RegisterStat(rt).Qi
//            else    RS(r).Vk <- Regs(rt); RS(r).Qk <- None
//
//            RS(r).Op <- Some opcode
//            RS(r).Busy <- true
//            RegisterStat(rd).Qi <- Some(cfg.rsPrefix + string r)
//
//            RS(r).A <- imm |> function
//            | Imm.NONE -> 0
//            | Imm.A imm -> immval imm
//
//            false
//        | _ -> true
//
//
//    let execute() = 
//        let compute (r:int) =
//            let vj, vk, a = RS.[r].Vj, RS.[r].Vk, RS.[r].A
//            RS.[r].Result <- RS.[r].Op |> function
//                | Some op -> op.Name |> function
//                    | "addi" -> vj + a
//                    | "add" -> vj + vk
//                    | _ -> failwith "failed compute instruction"
//                | None -> failwith "tried to compute rs with null op"    
////        for r = 0 to RS.Length - 1
//        XUnit2.TryFindNotBusy xunits |> function
//            | Some u -> 
//                RS.TryFindReady() |> function
//                | Some r ->
//                    xunits.[u].RSId <- RS.[r].Name
//                    xunits.[u].Busy <- true
//                    xunits.[u].Cycle()
//                | None -> ()
//            | None -> ()
//
//        xunits |> Array.iter (fun xunit -> if xunit.Busy then updateXUnit xunit compute)
//
////        XUnit2.TryFindNotBusy xunits |> function
////        | Some u -> 
////            xunits.[u].Update RS compute
////        | None -> ()
//
//
//    let write() =
//        let cdb = CDB.GetInstance
//        RS.TryFindResultReady() |> function
//        | Some r ->
//            RS.[r].ResultWritten <- true
//            cdb.Result <- RS.[r].Result
//            cdb.Src <- RS.[r].Name
//            Some(cdb)
//        | None -> None
//
//    let updateReservationStations = RS.Update
//    let clearReservationStations() = RS.Clear()
//    let cc = ref 0
//    let stall = ref false
//
//    let display() =
//        printfn "Clock Cycle: %d" !cc
//        printfn "Stall: %A" !stall
//        //xunits |> Array.iter (printfn "%O")
//        printfn "%O" RS
//        printfn "%O\n" !cdb
//
//    let cycle(instruction:Instruction) =
//        
//        cdb := write()
//        execute()
//        let stall = issue instruction
//        display()
//        updateReservationStations(!cdb)
//        
//        clearReservationStations()
////        display()
//        cc := !cc + 1
//        stall
//
//    stall := cycle i0
//    
//    stall := cycle i1
//    
//    stall := cycle i2
    

//[<Test>]
//let ``intUnit2`` () =
//    let RS' = ReservationStation.ArrayInit Config.FunctionalUnit.IntegerUnit
//    let rsRef = ref RS'
//    let intUnit = IntegerUnit.GetInstance rsRef
//    let iuRS = [| rsRef |> RS.IntegerUnit |]
//
//    let i0 = Instruction.ofInstructionInt(ints().[0])
//    let i1 = Instruction.ofInstructionInt(ints().[1])
//
//    let uid = ref 0
//
//    let mutable cdb : CDB option = None
//
//    let updateReservationStations cdb = RS.Update(iuRS)
//
//    let write() = intUnit.Write() // |> Array.tryPick (fun u -> u.Write())
//
//    let execute() = intUnit.Execute() //|> Array.iter (fun u -> u.Execute())
//
//    let issue (instruction:Instruction) =
//        intUnit.IsBusy() |> function
//        | false ->
//            intUnit.Insert i0
//        | _ -> ()
//        
//
//    cdb <- write()
//    printfn "cdb: %O" cdb
//    let halt = execute()
//    //printfn "%s" (intUnit.Dump())
//    let stall = issue(i0)
//    printfn "stall?? : %A" stall
//
//
//
//    cdb <- write()
//    printfn "cdb: %O" cdb
//    let halt = execute()
//    //printfn "%s" (intUnit.Dump())
//    let stall = issue(i1)
//    printfn "stall?? : %A" stall





