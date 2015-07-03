namespace FsDLX.Tomasulo

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
        sprintf "Cycles(%d/%d) Busy(%A) Station(%A)" (xu.RemainingCycles) (xu.MaxCycles) xu.Busy xu.Station

[<AbstractClass>]
type FunctionalUnit (cfg:Config.FunctionalUnit, rsg:RSGroup) as fu =
    
    let xunits = Array.init cfg.unitCount (fun _ -> XUnit cfg.maxCycles)

    let reservationStations = rsg.FilterByPrefix cfg.rsPrefix
//        fu |> function
//        | :? IntegerUnit -> RS.IntegerUnit rsRef
//        | :? TrapUnit -> RS.TrapUnit rsRef
//        | :? BranchUnit -> RS.BranchUnit rsRef
//        | :? MemoryUnit -> RS.MemoryUnit rsRef
//        | :? FloatingPointUnit -> RS.FloatingPointUnit rsRef
//        | _ -> failwith "invalid reservation station group"

    let RS(r) = reservationStations.[r]
    let XUnits(i) = xunits.[i]

    member val ReservationStations = reservationStations
    member val ExecutionUnits = xunits with get, set
    member val Queue = ReservationStationQueue()

    member val Stall = false with get, set
    member val Halt = false with get, set
    
    member fu.TryFindEmptyStation() = fu.ReservationStations.TryFindEmpty()
    member fu.TryFindReadyStation() = fu.ReservationStations.TryFind (fun r -> r.OperandsAvailable())
    member fu.TryFindAvailableXUnit() = fu.ExecutionUnits |> Array.tryFindIndex (fun xunit -> not(xunit.Busy))
    member fu.TrySetXUnit() = 
        match fu.TryFindReadyStation(), fu.TryFindAvailableXUnit() with 
        | Some r, Some x -> XUnits(x).Set r | _ -> ()

    member fu.TryFindResultReady() = fu.ReservationStations.TryFindResultReady()
    member fu.Finished() = 
        (fu.ExecutionUnits |> Array.forall (fun xunit -> not(xunit.Busy))) &&
        fu.ReservationStations.Finished()

    member fu.Clear() = fu.ReservationStations.Clear()
       
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

        | None -> fu.Stall <- true            
    
    default fu.Cycle xunit =
        match xunit.Station with
            | Some station -> 
                if xunit.Busy && xunit.RemainingCycles > 0 then xunit.Cycle()
                if xunit.RemainingCycles = 0 && not(station.ResultReady) then
                    fu.Compute station; xunit.Reset();
            | None -> ()

    default fu.Execute() = fu.TrySetXUnit(); xunits |> Array.iter fu.Cycle

    default fu.Write() =
        let cdb = CDB.GetInstance
        match fu.TryFindResultReady() with
        | Some r ->
            RS(r).ResultWritten <- true
            cdb.Result <- RS(r).Result
            cdb.Src <- RS(r).Name
            Some(cdb)
        | None -> None

and IntegerUnit private (cfg, rsg) =
    inherit FunctionalUnit(cfg, rsg)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let mutable instance = fun rsg -> IntegerUnit(cfg, rsg)
    
    override iu.Compute r =
        let RS(r) = iu.ReservationStations.[r]
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
    static member Reset() = instance <- fun rsg -> IntegerUnit(cfg, rsg)

and TrapUnit private (cfg, rsg) =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.TrapUnit
    static let mutable instance = fun rsg -> TrapUnit(cfg, rsg) 

    override tu.Compute r =
        let RS(r) = tu.ReservationStations.[r]
        match r.Qj with
        | Some _ -> ()
        | None ->
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
            RS(r).ResultReady <- true

    override tu.Execute() =
        let XUnits(x) = tu.ExecutionUnits.[x]
        let tryFindReadyStation() =
            if      tu.Queue.Count > 0 
            then    tu.ReservationStations.TryFind (fun r -> r.OperandsAvailable() && (r.Name = tu.Queue.Peek().Name))
            else    None
        match tu.TryFindAvailableXUnit(), tryFindReadyStation() with
        | Some x, Some r -> XUnits(x).Set(tu.Queue.Dequeue()) | _ -> ()

        tu.ExecutionUnits |> Array.iter tu.Cycle

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> TrapUnit(cfg, rsg)

and BranchUnit private (cfg, rsg) =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.BranchUnit
    static let mutable instance = fun rsg -> BranchUnit(cfg, rsg)
        
    override bu.Compute r = ()

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> BranchUnit(cfg, rsg)

and MemoryUnit private (cfg, rsg) as mu =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.MemoryUnit
    static let mutable instance = fun rsg -> MemoryUnit(cfg, rsg)

    let RS(r) = mu.ReservationStations.[r]
    let XUnits(x) = mu.ExecutionUnits.[x]

    override mu.Compute r =
        match RS(r).Op.Value.Name with
        | "lw" | "lf" -> RS(r).Result <- Memory.GetInstance.[RS(r).Vj + RS(r).A.Value]
        | _ -> Memory.GetInstance.[RS(r).Vj + RS(r).A.Value] <- RS(r).Vk
        RS(r).ResultReady <- true

    static member GetInstance = instance
    static member Reset() = instance <- fun rsg -> MemoryUnit(cfg, rsg)
    
and FloatingPointUnit private (cfg, rsg) =
    inherit FunctionalUnit(cfg, rsg)

    static let cfg = Config.FunctionalUnit.FloatingPointUnit
    static let mutable instance = fun rsg -> FloatingPointUnit(cfg, rsg)

    override fpu.Compute r =
        let RS(r) = fpu.ReservationStations.[r]
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
        //[| cast iu; cast tu; cast bu; cast mu; cast fpu |]
        [| cast iu; cast tu; cast mu; |]

    
    member val All = allfu with get

    member val ReservationStations = allrs with get

    member fu.Halt() = allfu |> Array.forall (fun u -> u.Halt = false) |> not
    member fu.Stall() = allfu |> Array.forall (fun u -> u.Stall)

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

    member fu.UpdateReservationStations() = allrs.Update()

    member fu.ClearReservationStations() = allfu |> Array.iter (fun u -> u.Clear())
   
    static member GetInstance = instance
    static member Reset() = 
        IntegerUnit.Reset()
        TrapUnit.Reset()
        BranchUnit.Reset()
        MemoryUnit.Reset()
        FloatingPointUnit.Reset()
        instance <- FunctionalUnits()