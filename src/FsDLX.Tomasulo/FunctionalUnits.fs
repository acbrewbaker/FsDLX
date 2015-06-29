﻿namespace FsDLX.Tomasulo

open System
open System.Collections
open System.Linq
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
    
//    override xu.ToString() =
//        sprintf "MaxCycles:         %d\n" xu.MaxCycles +
//        sprintf "RemainingCycles:   %d\n" xu.RemainingCycles +
//        sprintf "Busy:              %A\n" xu.Busy +
//        sprintf "CurrentRS:         %O\n" xu.CurrentRS

    static member TryFindAvailable (xunits:XUnit[]) =
        xunits |> Array.tryFindIndex (fun xu -> not(xu.Busy))


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

    member val ReservationStations = reservationStations
    member val ExecutionUnits = xunits with get, set
    member val Queue = ReservationStationQueue()

    member val ExecRS : string option = None with get,set
    member val LastInsert : string option = None with get, set
    member val Stall = false with get, set
    member val Halt = false with get, set
    
    member fu.TryFindEmptyStation() = fu.ReservationStations.TryFindEmpty()
    member fu.TryFindAvailableXUnit() = XUnit.TryFindAvailable fu.ExecutionUnits
    member fu.TryFindReadyStation() = fu.ReservationStations.TryFind (fun r -> r.OperandsAvailable())
    member fu.TryFindResultReady() = fu.ReservationStations.TryFindResultReady()
    member fu.Finished() = fu.ReservationStations.AllNotBusy()
    member fu.Clear() = fu.ReservationStations.Clear()
    
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
    
    abstract member Cycle : XUnit -> unit
    abstract member Compute  : ReservationStation -> unit
    abstract member Issue : Instruction -> unit
    abstract member Execute : unit -> unit
    abstract member Write : unit -> CDB option
    
    default fu.Issue instruction =
        let Regs(i) = (Regs.GetInstance instruction.AsInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.AsInt).[i]

        let opcode, rd, rs, rt, imm =
            instruction.Opcode,
            instruction.DstReg,
            instruction.S1Reg,
            instruction.S2Reg,
            instruction.Immediate

        match fu.TryFindEmptyStation() with
        | Some r -> 
            
            match RegisterStat(rs).Qi with  | Some _->  RS(r).Qj <- RegisterStat(rs).Qi
                                            | None  ->  RS(r).Vj <- Regs(rs); RS(r).Qj <- None
  
            match RegisterStat(rt).Qi with  | Some _->  RS(r).Qk <- RegisterStat(rt).Qi
                                            | None  ->  RS(r).Vk <- Regs(rt); RS(r).Qk <- None

            RS(r).Op <- Some opcode; RS(r).Busy <- true
            let rsId = Some(RS(r).Name)
            RegisterStat(rd).Qi <- rsId
            fu.LastInsert <- rsId

            RS(r).A <- imm

            fu.Queue.Enqueue(r)

        | None -> fu.LastInsert <- None; fu.Stall <- true            
    
    default fu.Cycle xunit =
        match xunit.CurrentRS with
            | Some station -> 
                if xunit.Busy then xunit.Cycle()
                if xunit.RemainingCycles = 0 && not(station.ResultReady) then
                    fu.Compute station; xunit.Reset();
            | None -> ()

    default fu.Execute() =
        match fu.TryFindReadyStation(), fu.TryFindAvailableXUnit() with
        | Some r, Some x -> XUnits(x).Set(r) 
        | _ -> ()

//        let cycle i =
//            match XUnits(i).CurrentRS with
//                | Some station -> 
//                    if XUnits(i).Busy then XUnits(i).Cycle()
//                    if XUnits(i).RemainingCycles = 0 && not(station.ResultReady) then
//                        fu.Compute station; XUnits(i).Reset();
//                | None -> ()
//        for i = 0 to fu.ExecutionUnits.Length - 1 do cycle i
        xunits |> Array.iter fu.Cycle

    default fu.Write() =
        let cdb = CDB.GetInstance
        match fu with
        | :? TrapUnit -> None
        | _ ->
            match fu.TryFindResultReady() with
            | Some r ->
                RS(r).ResultWritten <- true
                cdb.Result <- RS(r).Result
                cdb.Src <- RS(r).Name
                Some(cdb)
            | None -> None

and IntegerUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let mutable instance = fun rsRef -> IntegerUnit(cfg, rsRef)
          
    override iu.Compute r =
        let RS(r) = iu.ReservationStations.[r]
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

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> IntegerUnit(cfg, rsRef)

and TrapUnit private (cfg, rsRef) as tu =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.TrapUnit
    static let mutable instance = fun rsRef -> TrapUnit(cfg, rsRef) 

    let RS(r) = tu.ReservationStations.[r]
    let XUnits(x) = tu.ExecutionUnits.[x]
    
    override tu.Compute r =
        tu.ExecRS <- Some(RS(r).Name)
        printf "%s" 
            (match RS(r).Op with
            | Some op -> 
                match op.Name with
                | "halt" -> tu.Halt <- true; ""
                | "dumpGPR" -> 
                    RS(r).Vj.ToString() // |> Convert.int2hex
                | "dumpFPR" -> 
                    BitConverter.ToSingle(BitConverter.GetBytes(RS(r).Vj),0).ToString(".0######")
                | "dumpSTR" ->
                    Memory.GetInstance.AsBytes.Skip(RS(r).Vj).TakeWhile((<>) 0uy).ToArray() 
                    |> Convert.bytes2string
                | s -> failwith (sprintf "(%s) is an invalid trap unit instruction" s)
            | None -> "")
        RS(r).ResultReady <- true; RS(r).ResultWritten <- true

    override tu.Execute() =
        if not(XUnits(0).Busy) && tu.Queue.Count <> 0 then 
            
            XUnits(0).Set(tu.Queue.Dequeue()); XUnits(0).Cycle()
        elif XUnits(0).Busy then
            XUnits(0).Cycle()
            if XUnits(0).RemainingCycles = 0 then
                match XUnits(0).CurrentRS with
                | Some r -> tu.Compute r; XUnits(0).Reset()
                | _ -> ()

//    override tu.Execute() =
//        let XUnits(x) = tu.ExecutionUnits.[x]
//        if tu.Queue.Count <> 0 then
//            tu.ExecutionUnits |> Array.iteri (fun i xunit ->
//                if not(XUnits(i).Busy) then 
//                    XUnits(i).Set(tu.Queue.Dequeue()); XUnits(i).Cycle()
//                if XUnits(i).Busy then
//                    XUnits(i).Cycle()
//                    if XUnits(i).RemainingCycles = 0 then
//                        match XUnits(i).CurrentRS with
//                        | Some r -> tu.Compute r; XUnits(i).Reset()
//                        | _ -> ())
    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> TrapUnit(cfg, rsRef)

and BranchUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.BranchUnit
    static let mutable instance = fun rsRef -> BranchUnit(cfg, rsRef)
        
    override bu.Compute r = ()

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> BranchUnit(cfg, rsRef)

and MemoryUnit private (cfg, rsRef) as mu =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.MemoryUnit
    static let mutable instance = fun rsRef -> MemoryUnit(cfg, rsRef)

    let RS(r) = mu.ReservationStations.[r]
    let XUnits(x) = mu.ExecutionUnits.[x]
    
    let loadQueue = ReservationStationQueue()
    let storeQueue = ReservationStationQueue()
    
    override mu.Compute r =
        match RS(r).Op.Value.Name with
        | "lw" | "lf" -> RS(r).Result <- Memory.GetInstance.[RS(r).Vj + RS(r).A.Value]
        | _ -> () //Memory.GetInstance.[RS(r).Vj + RS(r).A.Value] <- RS(r).Vk
        RS(r).ResultReady <- true

    override mu.Execute() =
        if not(XUnits(0).Busy) && mu.Queue.Count <> 0 then 
            
            XUnits(0).Set(mu.Queue.Dequeue()); XUnits(0).Cycle()
        elif XUnits(0).Busy then
            XUnits(0).Cycle()
            if XUnits(0).RemainingCycles = 0 then
                match XUnits(0).CurrentRS with
                | Some r -> mu.Compute r; XUnits(0).Reset()
                | _ -> ()

    override mu.Write() =
        let cdb = CDB.GetInstance
        mu.ReservationStations.Filter (fun r -> RS(r).ResultReady)
        |> Array.tryPick (fun r ->
            match RS(r).Op.Value.Name with
            | "lw" | "lf" ->               
                RS(r).ResultWritten <- true
                cdb.Result <- RS(r).Result
                cdb.Src <- RS(r).Name
                Some(cdb)
            | _ ->
                RS(r).ResultWritten <- true
                Memory.GetInstance.[RS(r).Vj + RS(r).A.Value] <- RS(r).Vk
                None)

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> MemoryUnit(cfg, rsRef)
    
and FloatingPointUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.FloatingPointUnit
    static let mutable instance = fun rsRef -> FloatingPointUnit(cfg, rsRef)

    override fpu.Compute r =
        let RS(r) = fpu.ReservationStations.[r]
        fpu.ExecRS <- Some(RS(r).Name)
        RS(r).Result <-
            match RS(r).Op with
            | Some op ->
                if      RS(r).A.IsSome
                then    RS(r).Vj, RS(r).A.Value
                else    RS(r).Vj, RS(r).Vk
                ||> (fun x y -> float x, float y) ||>
                (match op.Name with
                | "addf" -> (+)
                | "subf" -> (-)
                | "multf" -> (*)
                | "divf" -> (/)
                | "mult" -> (*)
                | "div" -> (/)
                | "cvtf2i" -> fun x y -> x
                | "cvti2f" -> fun x y -> x
                | _ -> failwith "invalid floating point unit instruction")
                |> int
            | _ -> failwith "tried to compute with no opcode"
        RS(r).ResultReady <- true

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> FloatingPointUnit(cfg, rsRef)
    
and FunctionalUnits private () =
    static let mutable instance = FunctionalUnits()

    let iuRSG, tuRSG, buRSG, muRSG, fpuRSG =
        RSGroup.IntUnitInit() |> ref,
        RSGroup.TrapUnitInit() |> ref,
        RSGroup.BranchUnitInit() |> ref,
        RSGroup.MemoryUnitInit() |> ref,
        RSGroup.FloatingPointUnitInit() |> ref

    let iu, tu, bu, mu, fpu =
        IntegerUnit.GetInstance iuRSG,
        TrapUnit.GetInstance tuRSG,
        BranchUnit.GetInstance buRSG,
        MemoryUnit.GetInstance muRSG,
        FloatingPointUnit.GetInstance fpuRSG

    let allfu =
        let cast u = u :> FunctionalUnit
        //[| cast iu; cast tu; cast bu; cast mu; cast fpu |]
        [| cast iu; cast tu; cast mu; |]

    let allrs = 
        [|   
            RS.IntegerUnit iuRSG
            RS.TrapUnit tuRSG
            //RS.BranchUnit buRSG
            RS.MemoryUnit muRSG 
            |]
            //RS.FloatingPointUnit fpuRSG |]
    

    let allInfos = Array.init<string option> allfu.Length (fun _ -> None)

    let getRSInfo() =
        match allfu |> Array.tryFind (fun funit -> funit.LastInsert.IsSome) with
        | Some funit -> funit.GetRSInfo()
        | None -> ""
    
    let getExecInfo (funit:FunctionalUnit) =
        let execinfo = 
            sprintf "\nEXECUTING: instruction in station %O" 
                    (Convert.strOption2str funit.ExecRS)
        funit.ExecRS <- None; execinfo

    member val All = allfu with get

    member val ReservationStations = allrs with get

    member val InfoString = "" with get,set

    member fu.Halt() = allfu |> Array.forall (fun u -> u.Halt = false) |> not
    member fu.Stall() = allfu |> Array.forall (fun u -> u.Stall)

    member fu.Write() = allfu |> Array.tryPick (fun u -> u.Write())

    member fu.Execute() = allfu |> Array.iter (fun u -> u.Execute())

    member fu.Issue (i:Instruction) =
        fu.All |> Array.iter (fun u -> u.LastInsert <- None) 
        if i.Opcode.Name <> "nop" then
            match i with
            | Integer(_) -> iu.Issue i
            | Trap(_) -> tu.Issue i
            | Branch(_) -> bu.Issue i
            | Memory(_) -> mu.Issue i
            | FloatingPoint(_) -> fpu.Issue i
        fu.Stall()
        
    member fu.Finished() = allrs |> Array.forall (fun rs -> rs.AllNotBusy())

    member fu.UpdateReservationStations() = RS.Update(fu.ReservationStations)

    member fu.ClearReservationStations() = allfu |> Array.iter (fun u -> u.Clear())

    member fu.DumpLastInsert() = allfu |> Array.iter (fun u -> printfn "%A" u.LastInsert)

    override fu.ToString() = 
       match allfu |> Array.tryFindIndex (fun u -> u.ExecRS.IsSome) with
       | Some idx -> sprintf "%s%s" (getRSInfo()) (getExecInfo allfu.[idx])
       | None -> getRSInfo()
    
    static member GetInstance = instance
    static member Reset() = 
        IntegerUnit.Reset()
        TrapUnit.Reset()
        BranchUnit.Reset()
        MemoryUnit.Reset()
        FloatingPointUnit.Reset()
        instance <- FunctionalUnits()