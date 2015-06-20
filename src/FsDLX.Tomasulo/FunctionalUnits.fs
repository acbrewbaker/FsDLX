//module FsDLX.Tomasulo.FunctionalUnits
namespace FsDLX.Tomasulo

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

    static member Cycle (xunit:XUnit) = xunit.Cycle()
    
    override xu.ToString() =
        sprintf "-MaxCycles:         %d\n" xu.MaxCycles +
        sprintf "-RemainingCycles:   %d\n" xu.RemainingCycles +
        sprintf "-Busy:              %A\n" xu.Busy +
        sprintf "-CurrentRS:         %O\n" xu.CurrentRS

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
    
    let tryFindReadyStation() = reservationStations.TryFind (fun r -> r.OperandsAvailable())
    let tryFindAvailableXUnit() = XUnit.TryFindAvailable xunits
    let tryFindResultReady() = reservationStations.TryFindResultReady()
    let tryFindBusyStation() = reservationStations.TryFind (fun r -> r.Busy)

    member val ExecUnits = xunits with get
    member val ExecRS : string option = None with get,set
    member val LastInsert : string option = None with get, set
    member val Stall = false with get, set
    member val Halt = false with get, set

    member fu.Finished() = reservationStations.AllNotBusy()

    member fu.Clear() = fu |> function
        | _ -> reservationStations.Clear()
        
    member fu.Execute() =
        let cycle (xunit:XUnit) = xunit.CurrentRS |> function
            | Some station -> 
                if xunit.Busy then xunit.Cycle()
                if xunit.RemainingCycles = 0 && not(station.ResultReady) then
                    fu.Compute station; xunit.Reset();
            | None -> ()

        match tryFindReadyStation(), tryFindAvailableXUnit() with
        | Some r, Some x -> XUnits(x).Set(r) 
        | _ -> ()
        
        xunits |> Array.iter cycle

    member fu.Write() =
        let cdb = CDB.GetInstance
        match tryFindResultReady() with
        | Some r ->
            //printfn "\nWrite\n%O\n%O\n%A\n" (Clock.GetInstance) r (RS(r).Result)
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

    abstract member Insert   : Instruction -> unit
    abstract member Compute  : ReservationStation -> unit

and IntegerUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let mutable instance = fun rsRef -> IntegerUnit(cfg, rsRef)
    
    let RS' = RS.IntegerUnit rsRef
    let tryFindEmptyStation() = RS'.TryFindEmpty()

    let RS(r) = RS'.[r]

    override iu.Insert instruction =
        //printfn "Insert Int."
        let Regs(i) = (Regs.GetInstance instruction.AsInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.AsInt).[i]
        
        let opcode, rd, rs, rt, imm = 
            instruction.Opcode,
            instruction.DstReg, 
            instruction.S1Reg, 
            instruction.S2Reg,
            instruction.Immediate
           
        match tryFindEmptyStation() with
        | Some r ->
            
            match RegisterStat(rs).Qi with  | Some _->  RS(r).Qj <- RegisterStat(rs).Qi
                                            | None  ->  RS(r).Vj <- Regs(rs); RS(r).Qj <- None
                        
            match RegisterStat(rt).Qi with  | Some _->  RS(r).Qk <- RegisterStat(rt).Qi
                                            | None  ->  RS(r).Vk <- Regs(rt); RS(r).Qk <- None

            RS(r).Op <- Some opcode; RS(r).Busy <- true

            let rsId = Some(RS(r).Name)
            RegisterStat(rd).Qi <- rsId
            iu.LastInsert <- rsId

            RS(r).A <- imm
        
        | _ -> iu.Stall <- true

    override iu.Compute r =
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
        //printfn "\nIntUnit-Compute\n%O\n%O\n%A" (Clock.GetInstance) r (RS(r).Result)
        RS(r).ResultReady <- true
        

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

    override tu.Insert instruction = 
        let Regs(i) = (Regs.GetInstance instruction.AsInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.AsInt).[i]

        let opcode, funcCode, rd, rs, rt, imm =
            instruction.Opcode,
            instruction.FuncCode,
            instruction.DstReg,
            instruction.S1Reg,
            instruction.S2Reg,
            instruction.Immediate
        
        opcode.Name <-
            match funcCode with
            | FuncCode.HALT     -> tu.Halt <- true; "halt"
            | FuncCode.DUMPGPR  -> "dumpGPR"
            | FuncCode.DUMPFPR  -> "dumpFPR"
            | FuncCode.DUMPSTR  -> "dumpSTR"
            | _ -> failwith "invalid trap instruction"

        match tryFindEmptyStation() with
        | Some r -> 
            
            match RegisterStat(rs).Qi with  | Some _->  RS(r).Qj <- RegisterStat(rs).Qi
                                            | None  ->  RS(r).Vj <- Regs(rs); RS(r).Qj <- None
  
            RS(r).Op <- Some opcode; RS(r).Busy <- true
            let rsId = Some(RS(r).Name)
            RegisterStat(rd).Qi <- rsId
            tu.LastInsert <- rsId

            RS(r).A <- imm

            queue.Enqueue(r)

        | None -> tu.LastInsert <- None; tu.Stall <- true

    override tu.Compute r =
        let r : ReservationStation = queue.Dequeue() :?> ReservationStation
        tu.ExecRS <- Some(RS(r).Name)
        //printf "\n%O\n%O\n%O\n==>%s\n" (Clock.GetInstance) r (GPR.GetInstance)
        printf "%s" 
            (match RS(r).Op with
            | Some op -> 
                match op.Name with
                | "halt" -> tu.Halt <- true; ""
                | "dumpGPR" -> RS(r).Vj.ToString()
                | "dumpFPR" -> RS(r).Vj.ToString()
                | "dumpSTR" ->
                    Memory.GetInstance.AsBytes.Skip(RS(r).Vj).TakeWhile((<>) 0uy).ToArray() 
                    |> Convert.bytes2string
                | s -> failwith (sprintf "(%s) is an invalid trap unit instruction" s)
            | None -> "")
        //RS(r).Result <- RS(r).Vj
        RS(r).ResultReady <- true

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> TrapUnit(cfg, rsRef)

and BranchUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.BranchUnit
    static let mutable instance = fun rsRef -> BranchUnit(cfg, rsRef)

    override bu.Insert instruction = ()

    override bu.Compute r = ()

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

    override mu.Compute r = ()

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> MemoryUnit(cfg, rsRef)
    
and FloatingPointUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.FloatingPointUnit
    static let mutable instance = fun rsRef -> FloatingPointUnit(cfg, rsRef)

    override fpu.Insert i = ()
    override fpu.Compute r = ()

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
            | Integer(_) -> iu.Insert i
            | Trap(_) -> tu.Insert i
            | Branch(_) -> bu.Insert i
            | Memory(_) -> mu.Insert i
            | FloatingPoint(_) -> fpu.Insert i
        fu.Stall()
        
    member fu.Finished() = allrs |> Array.forall (fun rs -> rs.AllNotBusy())

    member fu.UpdateReservationStations() = RS.Update(fu.ReservationStations)

    member fu.ClearReservationStations() =
        allfu |> Array.iter (fun u -> u.Clear())

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