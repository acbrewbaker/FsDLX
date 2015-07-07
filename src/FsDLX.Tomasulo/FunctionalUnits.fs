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

    member val Stall = false with get, set
    member val Halt = false with get, set
    
    member fu.TryFindEmptyStation() = fu.ReservationStations.TryFindEmpty()
    member fu.TryFindReadyStation() = fu.ReservationStations.TryFind (fun r -> r.OperandsAvailable())
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

            RS(r).Op <- Some(opcode)
            RS(r).Busy <- true
            RegisterStat(rd).Qi <- Some(RS(r).Name)
            RS(r).A <- imm

            fu.Queue.Enqueue(r)
        | _ -> fu.Stall <- true

    default fu.Cycle xunit =
        match xunit.Station with
            | Some station -> 
                if xunit.Busy && xunit.RemainingCycles > 0 then xunit.Cycle()
                if xunit.RemainingCycles = 0 && not(station.ResultReady) then
                    fu.Compute station; xunit.Reset();
            | None -> ()

    default fu.Execute() = 
        fu.TrySetXUnit(); xunits |> Array.iter fu.Cycle

    default fu.Write() =
        let cdb = CDB.GetInstance
        match fu.TryFindResultReady() with
        | Some r ->
            RS(r).ResultWritten <- true
            cdb.Result <- RS(r).Result
            cdb.Src <- RS(r).Name
            if RS(r).Name.StartsWith("Branch") then
                PC.GetInstance.Value <- RS(r).Result
            Some(cdb)
        | None -> None

and IntegerUnit private (cfg, rsg) as iu =
    inherit FunctionalUnit(cfg, rsg)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let mutable instance = fun rsg -> IntegerUnit(cfg, rsg)
    
    let RS(r) = iu.ReservationStations.[r]
    
    let cvtf2i (x:float32) = BitConverter.ToInt32(BitConverter.GetBytes(x),0)
    let cvti2f (x:int) = BitConverter.ToSingle(BitConverter.GetBytes(x),0)

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
                | "movfp2i" -> fun x y -> x |> cvti2f |> cvtf2i
                | "movi2fp" -> fun x y -> x |> cvti2f |> cvtf2i
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
                | "dumpFPR" -> BitConverter.ToSingle(BitConverter.GetBytes(RS(r).Vj),0).ToString(".0######")
                | "dumpSTR" ->
                    Memory.GetInstance.AsBytes.Skip(RS(r).Vj).TakeWhile((<>) 0uy).ToArray() 
                    |> Convert.bytes2string
                | s -> failwith (sprintf "(%s) is an invalid trap unit instruction" s)
            | None -> "")
        RS(r).ResultReady <- true; RS(r).ResultWritten <- true
        
    override tu.Execute() =
        let XUnits(x) = tu.ExecutionUnits.[x]
        let tryFindReadyStation() =
            if      tu.Queue.Count > 0 
            then    tu.ReservationStations.TryFind (fun r -> r.OperandsAvailable() && (r.Name = tu.Queue.Peek().Name))
            else    None
        match tu.TryFindAvailableXUnit(), tryFindReadyStation() with
        | Some x, Some r -> XUnits(x).Set(tu.Queue.Dequeue()) | _ -> ()

        tu.ExecutionUnits |> Array.iter tu.Cycle

    override tu.Write() =
        //let cdb = CDB.GetInstance
        //tu.ReservationStations.Iter (fun r -> if RS(r).ResultReady then RS(r).ResultWritten <- true)
        None

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> TrapUnit(cfg, rsg)

and BranchUnit private (cfg, rsg) as bu =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.BranchUnit
    static let mutable instance = fun rsg -> BranchUnit(cfg, rsg)
    
    let RS(r) = bu.ReservationStations.[r]    

    override bu.Compute r =
        RS(r).Result <- 
            match RS(r).Op with
            | Some op -> 
                match op.Name with
                | "beqz" -> PC.GetInstance.Value + RS(r).A.Value
                | "j" -> PC.GetInstance.Value + RS(r).A.Value
                | "jr" -> RS(r).Vj
                | "jal" -> PC.GetInstance.Value + RS(r).A.Value
                | "jalr" -> RS(r).Vj + RS(r).A.Value
                | op -> printfn "%A" op; failwith "invalid branch unit instruction"
            | None -> failwith "tried to compute with no opcode"
        printfn "Branch Compute Result ====> %A" (RS(r).Result)
        RS(r).ResultReady <- true

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> BranchUnit(cfg, rsg)

and MemoryUnit private (cfg, rsg) as mu =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.MemoryUnit
    static let mutable instance = fun rsg -> MemoryUnit(cfg, rsg)

    let RS(r) = mu.ReservationStations.[r]
    let XUnits(x) = mu.ExecutionUnits.[x]

    override mu.Issue instruction =
        let Regs(i) = (Regs.GetInstance instruction.AsInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.AsInt).[i]

        let opcode, rd, rs, rt, imm =
            instruction.Opcode,
            instruction.DstReg,
            instruction.S1Reg,
            instruction.S2Reg,
            instruction.Immediate

        match mu.TryFindEmptyStation() with
        | Some r -> 
            RS(r).Op <- Some(opcode)

            match RegisterStat(rs).Qi with  | Some _->  RS(r).Qj <- RegisterStat(rs).Qi
                                            | None  ->  RS(r).Vj <- Regs(rs); RS(r).Qj <- None
  
            RS(r).A <- imm
            RS(r).Busy <- true
            
            match opcode.Name with
            | "lw" | "lf" ->
                RegisterStat(rd).Qi <- Some(RS(r).Name)
            | _ ->
                match RegisterStat(rd).Qi with  | Some _->  RS(r).Qk <- RegisterStat(rd).Qi
                                                | None  ->  RS(r).Vk <- Regs(rd); RS(r).Qk <- None
                                                
            mu.Queue.Enqueue(r)
        | None -> mu.Stall <- true  

    override mu.Compute r =
        match RS(r).Op.Value.Name with
        | "lw" | "lf" -> RS(r).Result <- Memory.GetInstance.[RS(r).Vj + RS(r).A.Value]
        | _ -> Memory.GetInstance.[RS(r).Vj + RS(r).A.Value] <- RS(r).Vk
        RS(r).ResultReady <- true

    override mu.Execute() =
        let XUnits(x) = mu.ExecutionUnits.[x]
        let tryFindReadyStation() =
            if      mu.Queue.Count > 0 
            then    mu.ReservationStations.TryFind (fun r -> r.OperandsAvailable() && (r.Name = mu.Queue.Peek().Name))
            else    None
        match mu.TryFindAvailableXUnit(), tryFindReadyStation() with
        | Some x, Some r -> XUnits(x).Set(mu.Queue.Dequeue()) | _ -> ()

        mu.ExecutionUnits |> Array.iter mu.Cycle

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> MemoryUnit(cfg, rsg)
    
and FloatingPointUnit private (cfg, rsg) as fpu =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.FloatingPointUnit
    static let mutable instance = fun rsg -> FloatingPointUnit(cfg, rsg)

    let RS(r) = fpu.ReservationStations.[r]
    
    let cvtf2i (x:float32) = BitConverter.ToInt32(BitConverter.GetBytes(x),0)
    let cvti2f (x:int) = BitConverter.ToSingle(BitConverter.GetBytes(x),0)

    override fpu.Compute r =
        let f g x y = (cvti2f x, cvti2f y) ||> g |> cvtf2i
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
                | "cvtf2i" -> fun x y -> x |> cvti2f |> int
                | "cvti2f" -> fun x y -> x |> float32 |> cvtf2i
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