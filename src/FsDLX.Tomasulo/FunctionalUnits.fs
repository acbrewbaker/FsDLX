﻿namespace FsDLX.Tomasulo

open System
open System.Collections
open System.Linq
open FsDLX.Common


type XUnit(maxCycles:int) =
    member val MaxCycles = maxCycles with get
    member val RemainingCycles = maxCycles with get, set
    member val Busy = false with get, set
    member val Station : ReservationStation option = None with get, set
    member xu.Set r = xu.Busy <- true; xu.Station <- Some(r)
    member xu.Cycle() = xu.RemainingCycles <- xu.RemainingCycles - 1
    member xu.Reset() = xu.RemainingCycles <- xu.MaxCycles; xu.Busy <- false; xu.Station <- None

    override xu.ToString() = 
        sprintf "Cycles(%d/%d) Busy(%A) Station(%s)" (xu.RemainingCycles) (xu.MaxCycles) xu.Busy 
            (match xu.Station with Some station -> station.Name | _ -> "<null>")

[<AbstractClass>]
type FunctionalUnit (cfg:Config.FunctionalUnit, rsg:RSGroup) =
    
    let xunits = Array.init cfg.unitCount (fun _ -> XUnit cfg.maxCycles)

    let reservationStations = rsg.FilterByPrefix cfg.rsPrefix

    let RS(r) = reservationStations.[r]
    let XUnits(i) = xunits.[i]

    member val ReservationStations = reservationStations
    member val ExecutionUnits = xunits with get, set
    member val Queue = ReservationStationQueue()
    member val BranchValue = 0 with get,set

    member val Stall = false with get, set
    member val Halt = false with get, set
    
    member fu.TryFindEmptyStation() = fu.ReservationStations.TryFindEmpty()
    member fu.TryFindReadyStation() = 
        match fu with
        | :? TrapUnit | :? MemoryUnit ->
            if      fu.Queue.Count > 0 
            then    fu.ReservationStations.TryFind (fun r -> r.OperandsAvailable() && (r.Name = fu.Queue.Peek().Name))
            else    None
        | _ ->
            fu.ReservationStations.TryFind (fun r -> r.OperandsAvailable())
    
    member fu.TryFindAvailableXUnit() = fu.ExecutionUnits |> Array.tryFindIndex (fun xunit -> xunit.Busy = false)
    member fu.TrySetXUnit() = 
        match fu.TryFindReadyStation(), fu.TryFindAvailableXUnit() with 
        | Some r, Some x -> 
            if fu.ExecutionUnits |> Array.forall (fun xunit -> 
                match xunit.Station with 
                | Some station -> r.Name <> station.Name
                | _ -> true) then XUnits(x).Set r 
              
        | _ -> ()

    member fu.TryFindResultReady() = fu.ReservationStations.TryFindResultReady()
    member fu.Finished() = 
        (fu.ExecutionUnits |> Array.forall (fun xunit -> not(xunit.Busy))) &&
        fu.ReservationStations.Finished()

    member fu.Clear() = fu.ReservationStations.Clear()
    
    member fu.Name() = cfg.rsPrefix

    member fu.Dump() = 
        let active = fu.ExecutionUnits |> Array.filter (fun xunit -> xunit.Busy)
        if active.Length > 0 
        then active |> Array.mapi (fun i xunit -> sprintf "%s(%d): %O" cfg.rsPrefix i xunit)
        else [|""|]

    //abstract member Cycle : XUnit -> unit
    abstract member Compute  : ReservationStation -> unit
    abstract member Issue : Instruction -> unit
    //abstract member Execute : unit -> unit
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
            RS(r).Op <- Some(opcode)
            RS(r).Busy <- true
            RS(r).A <- imm
            
            match RegisterStat(rs).Qi with  | Some _->  RS(r).Qj <- RegisterStat(rs).Qi
                                            | None  ->  RS(r).Vj <- Regs(rs); RS(r).Qj <- None
  
            match opcode.Name with
            | "lw" | "lf" ->
                RegisterStat(rd).Qi <- Some(RS(r).Name)
            | "sw" | "sf" ->
                match RegisterStat(rd).Qi with  | Some _->  RS(r).Qk <- RegisterStat(rd).Qi
                                                | None  ->  RS(r).Vk <- Regs(rd); RS(r).Qk <- None
            | _ ->
                match RegisterStat(rt).Qi with  | Some _->  RS(r).Qk <- RegisterStat(rt).Qi
                                                | None  ->  RS(r).Vk <- Regs(rt); RS(r).Qk <- None

            match opcode.Name with
            | "jal" | "jalr" ->
                GPR.GetInstance.[31].Qi <- Some(RS(r).Name)
            | _ -> RegisterStat(rd).Qi <- Some(RS(r).Name)
            
            fu.Queue.Enqueue(r)
        | _ -> fu.Stall <- true

    member fu.Cycle (xunit:XUnit) =
        match xunit.Station with
            | Some station -> 
                if xunit.Busy && xunit.RemainingCycles > 0 then xunit.Cycle()
                if xunit.RemainingCycles = 0 && not(station.ResultReady) then
                    fu.Compute station; xunit.Reset();
            | None -> ()

    member fu.Execute() = 
        match fu with
        | :? TrapUnit | :? MemoryUnit ->
            match fu.TryFindAvailableXUnit(), fu.TryFindReadyStation() with
            | Some x, Some r -> XUnits(x).Set(fu.Queue.Dequeue()) | _ -> ()
        | _ ->
            fu.TrySetXUnit()
        xunits |> Array.iter fu.Cycle

    default fu.Write() =
        let cdb = CDB.GetInstance
        match fu.TryFindResultReady() with
        | Some r ->
            RS(r).ResultWritten <- true
            cdb.Result <- RS(r).Result
            cdb.Src <- RS(r).Name
            match fu with :? BranchUnit -> PC.GetInstance.Value <- fu.BranchValue | _ -> ()
            Some(cdb)
        | None -> None

and IntegerUnit private (cfg, rsg) as iu =
    inherit FunctionalUnit(cfg, rsg)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let mutable instance = fun rsg -> IntegerUnit(cfg, rsg)
    
    let RS(r) = iu.ReservationStations.[r]
    
    override iu.Compute r =
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
                | "movfp2i" -> fun x y -> x |> Convert.i2f |> Convert.f2i
                | "movi2fp" -> fun x y -> x |> Convert.i2f |> Convert.f2i
                | "nop" -> fun _ _ -> 0
                | op -> printfn "%A" op; failwith "invalid integer unit instruction"
            | None -> failwith "tried to compute with no opcode"
        RS(r).ResultReady <- true

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> IntegerUnit(cfg, rsg)

and TrapUnit private (cfg, rsg) as tu =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.TrapUnit
    static let mutable instance = fun rsg -> TrapUnit(cfg, rsg) 
    
    let RS(r) = tu.ReservationStations.[r]
    
    override tu.Compute r =
        printf "%s"
            (match RS(r).Op with
            | Some op -> 
                match op.Name with
                | "halt" -> tu.Halt <- true; ""
                | "dumpGPR" -> RS(r).Vj.ToString()
                | "dumpFPR" -> Convert.i2f(RS(r).Vj).ToString(".0######")
                | "dumpSTR" ->
                    Memory.GetInstance.AsBytes.Skip(RS(r).Vj).TakeWhile((<>) 0uy).ToArray() 
                    |> Convert.bytes2string
                | s -> failwith (sprintf "(%s) is an invalid trap unit instruction" s)
            | None -> "")
        RS(r).ResultReady <- true; RS(r).ResultWritten <- true
        
    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> TrapUnit(cfg, rsg)

and BranchUnit private (cfg, rsg) as bu =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.BranchUnit
    static let mutable instance = fun rsg -> BranchUnit(cfg, rsg)
    
    let RS(r) = bu.ReservationStations.[r]    

    override bu.Compute r =
        let pc = PC.GetInstance.Value
        RS(r).Result <- 
            match RS(r).Op with
            | Some op -> 
                match op.Name with
                | "beqz" -> 
                    let bv = pc + RS(r).A.Value
                    if      RS(r).Vj = 0 
                    then    bu.BranchValue <- bv; bv
                    else    bu.BranchValue <- pc; pc
                | "j" -> bu.BranchValue <- pc + RS(r).A.Value; pc
                | "jr" -> bu.BranchValue <- RS(r).Vj; pc
                | "jal" -> bu.BranchValue <- pc + RS(r).A.Value; pc
                | "jalr" -> 
                    bu.BranchValue <-
                        match RS(r).A with
                        | Some a -> RS(r).Vj + a
                        | _ -> RS(r).Vj
                    pc
                | op -> printfn "%A" op; failwith "invalid branch unit instruction"
            | None -> failwith "tried to compute with no opcode"
        RS(r).ResultReady <- true

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> BranchUnit(cfg, rsg)

and MemoryUnit private (cfg, rsg) as mu =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.MemoryUnit
    static let mutable instance = fun rsg -> MemoryUnit(cfg, rsg)

    let RS(r) = mu.ReservationStations.[r]
    
    override mu.Compute r =
        match RS(r).Op.Value.Name with
        | "lw" | "lf" -> RS(r).Result <- Memory.GetInstance.[RS(r).Vj + RS(r).A.Value]
        | _ -> Memory.GetInstance.[RS(r).Vj + RS(r).A.Value] <- RS(r).Vk
        RS(r).ResultReady <- true

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> MemoryUnit(cfg, rsg)
    
and FloatingPointUnit private (cfg, rsg) as fpu =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.FloatingPointUnit
    static let mutable instance = fun rsg -> FloatingPointUnit(cfg, rsg)

    let RS(r) = fpu.ReservationStations.[r]
    
    override fpu.Compute r =
        let f g x y = (Convert.i2f x, Convert.i2f y) ||> g |> Convert.f2i
        RS(r).Result <-
            match RS(r).Op with
            | Some op ->
                if      RS(r).A.IsSome
                then    RS(r).Vj, RS(r).A.Value
                else    RS(r).Vj, RS(r).Vk
                ||>
                (match op.Name with
                | "addf" -> f (+)
                | "subf" -> f (-)
                | "multf" -> f (*)
                | "divf" -> f (/)
                | "mult" -> (*)
                | "div" -> (/)
                | "cvtf2i" -> fun x y -> x |> Convert.i2f |> int
                | "cvti2f" -> fun x y -> x |> float32 |> Convert.f2i
                | _ -> failwith "invalid floating point unit instruction")
            | _ -> failwith "tried to compute with no opcode"
        RS(r).ResultReady <- true

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> FloatingPointUnit(cfg, rsg)
    
and FunctionalUnits private () =
    static let mutable instance = FunctionalUnits()

    let allrs = RSGroup.InitAll

    let iu, tu, bu, mu, fpu =
        IntegerUnit.GetInstance allrs,
        TrapUnit.GetInstance allrs,
        BranchUnit.GetInstance allrs,
        MemoryUnit.GetInstance allrs,
        FloatingPointUnit.GetInstance allrs

    let allfu =
        let cast u = u :> FunctionalUnit
        [| cast iu; cast tu; cast bu; cast mu; cast fpu |]
           
    member val All = allfu with get

    member val ReservationStations = allrs with get

    member fu.Halt() = allfu |> Array.forall (fun u -> u.Halt = false) |> not
    member fu.Stall() = allfu |> Array.forall (fun u -> u.Stall = false) |> not
    
    member fu.Write() = allfu |> Array.tryPick (fun u -> u.Write())

    member fu.Execute() = allfu |> Array.iter (fun u -> u.Execute())

    member fu.Issue (i:Instruction) =
        if i.Opcode.Name <> "nop" then
            match i with
            | Integer(_) -> iu.Issue i
            | Trap(_) -> tu.Issue i
            | Branch(_) -> bu.Issue i
            | Memory(_) -> mu.Issue i
            | FloatingPoint(_) -> fpu.Issue i
        fu.Stall()
    
    member fu.BranchInBranchUnit() = bu.ReservationStations.ForAll (fun r -> r.Busy = false) |> not
       
    member fu.Finished() = allfu |> Array.forall (fun fu -> fu.Finished())

    member fu.UpdateReservationStations = allrs.Update

    member fu.ClearReservationStations() = allfu |> Array.iter (fun u -> u.Clear())
   
    member fu.DumpRS() = allrs.Dump()
        
    member fu.DumpFU() = 
        (allfu |> Array.map (fun funit -> funit.Dump()) 
        |> Array.concat |> Array.map ((+) "\n") |> Array.reduce (+))
            .Trim()

    static member GetInstance = instance
    static member Reset() = 
        IntegerUnit.Reset()
        TrapUnit.Reset()
        BranchUnit.Reset()
        MemoryUnit.Reset()
        FloatingPointUnit.Reset()
        instance <- FunctionalUnits()