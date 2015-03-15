//module FsDLX.Tomasulo.FunctionalUnits
namespace FsDLX.Tomasulo

open System
open System.Collections
open FsDLX.Common

type XUnit(maxCycles:int) =
    member val MaxCycles = maxCycles with get
    member val RemainingCycles = maxCycles with get, set
    member val Busy = false with get, set
    member val CurrentRS : ReservationStation option = None with get, set
    
    member xu.Set(r:ReservationStation) = xu.Busy <- true; xu.CurrentRS <- Some(r)
    
    member xu.Cycle() = xu.RemainingCycles <- xu.RemainingCycles - 1
    
    member xu.Reset() =
        if xu.RemainingCycles <= 0 then
            xu.RemainingCycles <- xu.MaxCycles
            xu.Busy <- false
            xu.CurrentRS <- None

    static member Cycle (xunit:XUnit) = xunit.Cycle()
    
    override xu.ToString() =
        sprintf "-MaxCycles:         %d\n" xu.MaxCycles +
        sprintf "-RemainingCycles:   %d\n" xu.RemainingCycles +
        sprintf "-Busy:              %A\n" xu.Busy +
        sprintf "-CurrentRS:         %O\n" xu.CurrentRS

    static member TryFindAvailable (xunits:XUnit[]) =
        xunits |> Array.tryFindIndex (fun xu -> not(xu.Busy))

    static member AllBusy (xunits:XUnit[]) =
        xunits |> Array.forall (fun xunit -> xunit.Busy)

    static member AllNotBusy (xunits:XUnit[]) =
        xunits |> Array.forall (fun xunit -> not(xunit.Busy))

[<AbstractClass>]
type FunctionalUnit (cfg:Config.FunctionalUnit, rsRef:RSGroupRef) as fu =
    let xunits = Array.init cfg.unitCount (fun _ -> XUnit(cfg.maxCycles))

    let reservationStations = fu |> function
        | :? IntegerUnit -> RS.IntegerUnit rsRef
        | :? TrapUnit -> RS.TrapUnit rsRef
        | :? BranchUnit -> RS.BranchUnit rsRef
        | :? MemoryUnit -> RS.MemoryUnit rsRef
        | :? FloatingPointUnit -> RS.FloatingPointUnit rsRef
        | _ -> failwith "invalid reservation station group"

    let RS(r) = reservationStations.[r]
    let XUnits(i) = xunits.[i]
    
    let tryFindReadyStation() = reservationStations.TryFind (fun r -> r.OperandsAvailable())
    let tryFindAvailableXUnit() = XUnit.TryFindAvailable xunits
    let tryFindResultReady() = reservationStations.TryFindResultReady()
    let tryFindBusyStation() = reservationStations.TryFind (fun r -> r.Busy)

    member val ExecUnits = xunits with get
    member val ExecRS : string option = None with get,set
    member val LastInsert : string option = None with get, set
    member val Stall = false with get, set
    member val Halt = false with get, set
   
    member fu.IsBusy() = XUnit.AllBusy xunits

    member fu.Finished() = reservationStations.AllNotBusy()

    member fu.Clear() = fu |> function
//        | :? MemoryUnit as mu -> 
//            mu.LoadBuffer |> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
//            mu.StoreBuffer|> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
        | _ -> reservationStations.Clear()


    //abstract member Execute : unit -> unit
    // begin execution of an instruction if its operands are available and a corresponding functional unit is available
    // decrease the x count of an instruction that is already being executed by the corresponding functional unit
    // compute the result of an executing instruction if the execution count is 0 and set result & result ready fields of Rstation
    member fu.Execute() =
        printfn "+++++ %s EXECUTE +++++" (fu.Name())
//        let derp() =
//            match tryFindReadyStation(), tryFindAvailableXUnit() with
//            | Some r, Some x -> XUnits(x).Set(r)
//            | _ ->
//                match

        let cycle (xunit:XUnit) = xunit.CurrentRS |> function
            | Some station -> 
                if xunit.Busy then xunit.Cycle()
                if xunit.RemainingCycles = 0 && not(station.ResultReady) then
                    fu.Halt <- fu.Compute station; xunit.Reset();  printfn "executing :%O" fu.ExecRS
            | None -> ()

        let tryCompute = function
            | Some station -> if station.ResultReady then fu.Compute station else false
            | None -> false
            
            //XUnit.TryCompute fu.Compute xunit
//                (sprintf "%s-TryCompute\n" (fu.Name()))
//                >=> xunit 
//                |> XUnit.TryCompute fu.Compute
       // printfn "------ Stations -------"
        //printfn "%A" (reservationStations.Dump())

        match tryFindReadyStation(), tryFindAvailableXUnit() with
        | Some r, Some x -> 
            //printfn "ReadyStation, ReadyXunit : %A, %A" (RS(r).Name) (x)
            XUnits(x).Set(r) 
        | _ -> ()
        
        xunits |> Array.iter cycle

        

    member fu.Write() =
        printfn "+++++ %s WRITE +++++" (fu.Name())
        let cdb = CDB.GetInstance
        match fu with
        | :? TrapUnit ->
            printfn "------ Stations (before) -------"
            printfn "%A" (reservationStations.Dump())
            match tryFindResultReady() with
            | Some r ->
                printfn "Result Ready ===> %O" r
                RS(r).ResultWritten <- true
                cdb.Result <- RS(r).Result
                cdb.Src <- RS(r).Name
                printfn "------ Stations (after) -------"
                printfn "%A" (reservationStations.Dump())
            | None ->   match tryFindBusyStation() with
                        | Some r -> printfn "Still busy: %O" r
                        | None -> ()
            
            None
        | _ ->
            match tryFindResultReady() with
            | Some r ->
                //printfn "%s write" (fu.Name())
                RS(r).ResultWritten <- true
                cdb.Result <- RS(r).Result
                cdb.Src <- RS(r).Name
                Some(cdb)
            | None -> None

    member fu.Name() =
        sprintf "%sUNIT"
            (match fu with
            | :? IntegerUnit -> "INT"
            | :? TrapUnit -> "TRAP"
            | :? BranchUnit -> "BRANCH"
            | :? MemoryUnit -> "MEM"
            | :? FloatingPointUnit -> "FP"
            | _ -> "Functional ")

    member fu.GetRSInfo() = sprintf "%s RESERVATION STATIONS\n%O" (fu.Name()) reservationStations

//    member private fu.Dump() =
//        fu.State <- fu.State +
//            let name = fu.Name()
//            sprintf "====== %s ======\n" name +
//            (fu.XUnits |> Array.mapi (sprintf "XUnit(%d)\n%O\n") |> Array.reduce (+))
    

    abstract member Insert   : Instruction -> unit
    abstract member Compute  : ReservationStation -> bool

and IntegerUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let mutable instance = fun rsRef -> IntegerUnit(cfg, rsRef)
    
    let RS' = RS.IntegerUnit rsRef
    let tryFindEmptyStation() = RS'.TryFindEmpty()

    let RS(r) = RS'.[r]

    override iu.Insert instruction =
        printfn "+++++ IntegerUnit - Insert ++++++"
        let Regs(i) = (Regs.GetInstance instruction.asInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.asInt).[i]
        
        let opcode, rd, rs, rt, imm = 
            instruction.Opcode,
            instruction.DstReg, 
            instruction.S1Reg, 
            instruction.S2Reg,
            instruction.Immediate
           
        match tryFindEmptyStation() with
        | Some r ->
            //"IntUnit-tryfindempty-" >=> r |> ignore
            //"IntUnit-RS\n" >=> RS' |> ignore

            if rs <> S1Reg.NONE then
                if      RegisterStat(rs).Qi.IsSome 
                then    RS(r).Qj <- RegisterStat(rs).Qi
                else    RS(r).Vj <- Regs(rs); RS(r).Qj <- None
            
            if rt <> S2Reg.NONE then
                if      RegisterStat(rt).Qi.IsSome
                then    RS(r).Qk <- RegisterStat(rt).Qi
                else    RS(r).Vk <- Regs(rt); RS(r).Qk <- None

            RS(r).Op <- Some opcode
            RS(r).Busy <- true

            let rsId = Some(RS(r).Name)
            RegisterStat(rd).Qi <- rsId
            iu.LastInsert <- rsId; //printfn "did insert"

            RS(r).A <- imm
        
        | _ -> iu.Stall <- true

    override iu.Compute r =
        //printfn "+++++ IntegerUnit - Compute ++++++"
        let halt = false
        iu.ExecRS <- Some(RS(r).Name)
        RS(r).Result <- 
            match RS(r).Op with
            | Some op -> 
                if      RS(r).A.IsSome
                then    RS(r).Vj, RS(r).A.Value
                else    RS(r).Vj, RS(r).Vk
                ||>
                match op.Name with
                | "addi" -> (+)
                | "add" -> (+)
                | "sub" -> (-)
                | "and" -> (&&&)
                | "or" -> (|||)
                | "xor" -> (^^^)
                | "movf" -> fun x y -> x
                | "movfp2i" -> fun x y -> x
                | "movi2fp" -> fun x y -> x
                | "nop" -> fun _ _ -> 0
                | op -> printfn "%A" op; failwith "invalid integer unit instruction"
            | None -> failwith "tried to compute with no opcode"
        RS(r).ResultReady <- true
        halt

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> IntegerUnit(cfg, rsRef)

and TrapUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.TrapUnit
    static let mutable instance = fun rsRef -> TrapUnit(cfg, rsRef) 

    let queue = Queue()
    let mutable traps = Seq.empty<ReservationStation>

    let RS' = RS.TrapUnit rsRef
    let tryFindReadyStation() = RS'.TryFind (fun r -> r.OperandsAvailable())
    let tryFindEmptyStation() = RS'.TryFindEmpty()
    let RS(r) = RS'.[r]
    
    member tu.Traps 
        with get() = traps
        and set v = traps <- Seq.append traps v
        

    override tu.Insert instruction = 
        let Regs(i) = (Regs.GetInstance instruction.asInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.asInt).[i]

        let opcode, funcCode, rd, rs, rt, imm =
            instruction.Opcode,
            instruction.FuncCode,
            instruction.DstReg,
            instruction.S1Reg,
            instruction.S2Reg,
            instruction.Immediate
        printfn "+++++ TrapUnit - Insert ++++++"
        match tryFindEmptyStation() with
        | Some r -> 
            //"TrapUnit-tryfindempty-" >=> r |> ignore
            //"TrapUnit-RS\n" >=> RS' |> ignore

            if      RegisterStat(rs).Qi.IsSome 
            then    RS(r).Qj <- RegisterStat(rs).Qi
            else    RS(r).Vj <- Regs(rs); RS(r).Qj <- None
//            
//            if      RegisterStat(rt).Qi.IsSome
//            then    RS(r).Qk <- RegisterStat(rt).Qi
//            else    RS(r).Vk <- Regs(rt); RS(r).Qk <- None

            opcode.Name <-
                match funcCode with
                | FuncCode.HALT -> "halt"
                | FuncCode.DUMPGPR -> "dumpGPR"
                | FuncCode.DUMPFPR -> "dumpFPR"
                | FuncCode.DUMPSTR -> "dumpSTR"
                | _ -> failwith "invalid trap instruction"
            
            RS(r).Op <- Some opcode
            RS(r).Busy <- true
            let rsId = Some(RS(r).Name)
            RegisterStat(rd).Qi <- rsId
            tu.LastInsert <- rsId; //printfn "did insert"

            RS(r).A <- imm
            //printfn "r is : %O" r
            queue.Enqueue(r)
            tu.Traps <- [r]
//            let qenum = queue.GetEnumerator()
//            while qenum.MoveNext() do printfn "Queue entry ==> %O" qenum.Current
            //printfn "Queue after insert-%O" (queue.GetEnumerator() :?> ReservationStation)
            //printfn "traps after append-" 
            //for r in tu.Traps do printfn "%O" r

        | None -> tu.LastInsert <- None; tu.Stall <- true


    override tu.Compute r =
        //printfn "+++++ TrapUnit - Compute ++++++"
        printfn "trap r ==> %O" r
        //printfn "queue.Peek() ==> %O" (queue.Peek() :?> ReservationStation)
        let display() =
            let qenum = queue.GetEnumerator()
            while qenum.MoveNext() do printfn "Queue entry ==> %O" qenum.Current
        
        //display(); queue.Dequeue()|>ignore; display()
        if r.Name = (queue.Peek() :?> ReservationStation).Name then
            //printfn "BEFORE"; display()
            let r : ReservationStation = queue.Dequeue() :?> ReservationStation
            printfn "POPPED ====> %O" r
//            let qenum = queue.GetEnumerator()
//            while qenum.MoveNext() do 
//                let r' = qenum.Current :?> ReservationStation
//                RS(r).ResultReady <- false
//                RS(r).ResultWritten <- false
            //"TrapDequeue&Compute-" >=> queue.Dequeue() :?> ReservationStation
            //printfn "AFTER"; display()
            tu.ExecRS <- Some(RS(r).Name)
            //printfn "Compute Trap, RS(r).Vj, RS(r).Vk ==>  %A, %A" (RS(r).Vj) (RS(r).Vk)
            let halt, result = RS(r).Op |> function
                | Some op -> 
                    match op.Name, RS(r).A with
                    | "halt", _ -> printfn "--------------------------------------------------------------------saw halt"; true, 0
                    | "dumpGPR", _ -> false, RS(r).Vj
                    | "dumpFPR", _ -> false, RS(r).Vj
                    | "dumpSTR", Some A' -> false, Memory.GetInstance.[A']
                    | _ -> failwith "invalid trap unit instruction"
                | None -> false, 0
            //printfn "trap Result:  %A" result
            RS(r).Result <- result
            RS(r).ResultReady <- true
            halt
        else
            false

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> TrapUnit(cfg, rsRef)

and BranchUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.BranchUnit
    static let mutable instance = fun rsRef -> BranchUnit(cfg, rsRef)

    override bu.Insert instruction = ()

    override bu.Compute r = false

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> BranchUnit(cfg, rsRef)

and MemoryUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.MemoryUnit
    static let mutable instance = fun rsRef -> MemoryUnit(cfg, rsRef)
    let mutable xQueue = List.empty<int>
    let mutable wQueue = List.empty<int>

//    member val LoadBuffer   = ReservationStation.ArrayInit Config.FunctionalUnit.MemoryUnit with get, set
//    member val StoreBuffer  = ReservationStation.ArrayInit Config.FunctionalUnit.MemoryUnit with get, set
//

    override mu.Insert instruction = ()

    override mu.Compute r = false

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> MemoryUnit(cfg, rsRef)
    
and FloatingPointUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.FloatingPointUnit
    static let mutable instance = fun rsRef -> FloatingPointUnit(cfg, rsRef)

    override fpu.Insert i = ()
    override fpu.Compute r = false

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> FloatingPointUnit(cfg, rsRef)
    
and FunctionalUnits private () =
    static let mutable instance = FunctionalUnits()

    let iuCfg, tuCfg, buCfg, muCfg, fpuCfg = 
        Config.FunctionalUnit.IntegerUnit,
        Config.FunctionalUnit.TrapUnit,
        Config.FunctionalUnit.BranchUnit,
        Config.FunctionalUnit.MemoryUnit,
        Config.FunctionalUnit.FloatingPointUnit
    
    let iuRS, tuRS, buRS, muRS, fpuRS =
        ReservationStation.ArrayInit iuCfg |> ref,
        ReservationStation.ArrayInit tuCfg |> ref,
        ReservationStation.ArrayInit buCfg |> ref,
        ReservationStation.ArrayInit muCfg |> ref,
        ReservationStation.ArrayInit fpuCfg |> ref

    let iu, tu, bu, mu, fpu =
        IntegerUnit.GetInstance iuRS,
        TrapUnit.GetInstance tuRS,
        BranchUnit.GetInstance buRS,
        MemoryUnit.GetInstance muRS,
        FloatingPointUnit.GetInstance fpuRS

    let allfu =
        let cast u = u :> FunctionalUnit
        [| cast iu; cast tu; cast bu; cast mu; cast fpu |]
            //.[0..1]

    let allrs = 
        [| 
            RS.IntegerUnit iuRS 
            RS.TrapUnit tuRS
            RS.BranchUnit buRS
            RS.MemoryUnit muRS 
            RS.FloatingPointUnit fpuRS |]

    let allInfos = Array.init<string option> allfu.Length (fun _ -> None)

    let getRSInfo() =
        match allfu |> Array.tryFind (fun funit -> funit.LastInsert.IsSome) with
        | Some funit -> funit.GetRSInfo()
        | None -> ""
    
    let mutable execRS : string option = None
    let getExecInfo (funit:FunctionalUnit) =
        let execinfo = 
            sprintf "\nEXECUTING: instruction in station %O" 
                    (Convert.strOption2str funit.ExecRS)
        //execRS <- None; execinfo
        funit.ExecRS <- None; execinfo

    member val All = allfu with get

    member val ReservationStations = allrs with get

    member val InfoString = "" with get,set

    member fu.Write() = allfu |> Array.tryPick (fun u -> u.Write())

    member fu.Execute() = allfu |> Array.iter (fun u -> u.Execute())

    member fu.Execute2() = 
        (allfu, allrs) ||> Array.iter2 (fun funit rstations ->
            let x = match rstations.TryFindOperandsAvailable(), XUnit.TryFindAvailable funit.ExecUnits with
                    | Some r, Some x -> funit.ExecUnits.[x].Set(rstations.[r]); x
                    | _ -> 0 

            funit.ExecUnits |> Array.iter (fun xunit -> if xunit.Busy then xunit.Cycle())
            
            match rstations.TryFindResultReady() with
            | Some r -> 
                printfn "ResultReady at %O" r
                execRS <- Some(r.Name)
                printfn "execRS: %O" execRS
                funit.Halt <- funit.Compute r
            | None -> () //execRS <- None


                //if x = 0 then if xunit.Busy then xunit.Cycle()
                //elif x <> i then if xunit.Busy then xunit.Cycle())
            

            )
                

    member fu.Issue (i:Instruction) =
        fu.All |> Array.iter (fun u -> u.LastInsert <- None) 
        //printfn "Issue ==> %A" (i.Opcode)
        if i.Opcode.Name <> "nop" then
            match i with
            | Integer(_) -> iu.Insert i
            | Trap(_) -> tu.Insert i
            | Branch(_) -> bu.Insert i
            | Memory(_) -> mu.Insert i
            | FloatingPoint(_) -> fpu.Insert i

    member fu.Halt() = allfu |> Array.forall (fun u -> u.Halt)
    member fu.Stall() = allfu |> Array.forall (fun u -> u.Stall)

    member fu.AllFinished() = allfu |> Array.forall (fun u -> u.Finished())

    member fu.UpdateReservationStations() = RS.Update(fu.ReservationStations)

    member fu.ClearReservationStations() =
        allfu |> Array.iter (fun u -> u.Clear())

    member fu.Dump() = ()
//        sprintf "---------------------------------- %O" Clock.GetInstance +
//        (allfu.[0..1] |> Array.map (fun u -> u.State) |> Array.map ((+) "\n") |> Array.reduce (+)) 
//        |> printfn "%s"

    member fu.DumpLastInsert() = allfu |> Array.iter (fun u -> printfn "%A" u.LastInsert)

    override fu.ToString() = 
       match allfu |> Array.tryFindIndex (fun u -> u.ExecRS.IsSome) with
       | Some idx -> sprintf "%s%s" (getRSInfo()) (getExecInfo allfu.[idx])
       | None -> getRSInfo()
//        //printfn "rs only"; 
//        match execRS with
//        | Some _ -> sprintf "%s%s" (getRSInfo()) (getExecInfo()) // allfu.[idx])
//        | _ -> getRSInfo()
    
    static member GetInstance = instance
    static member Reset() = 
        IntegerUnit.Reset()
        TrapUnit.Reset()
        BranchUnit.Reset()
        MemoryUnit.Reset()
        FloatingPointUnit.Reset()
        instance <- FunctionalUnits()