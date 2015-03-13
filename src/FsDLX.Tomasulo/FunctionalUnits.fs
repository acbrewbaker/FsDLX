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

    override xu.ToString() =
        sprintf "-MaxCycles:         %d\n" xu.MaxCycles +
        sprintf "-RemainingCycles:   %d\n" xu.RemainingCycles +
        sprintf "-Busy:              %A\n" xu.Busy +
        sprintf "-CurrentRS:         %O\n" xu.CurrentRS

    static member TryCompute (compute:ReservationStation -> bool) (xunit:XUnit) =
        let mutable halt = false
        match xunit.CurrentRS with
        | Some station ->
            xunit.Cycle()
            if xunit.RemainingCycles = 0 && not(station.ResultReady)
            then station.ResultReady <- true; halt <- compute station; xunit.Reset()
        | None -> ()
        halt

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
    
    let tryFindReadyStation() = reservationStations.TryFindReady()
    let tryFindAvailableXUnit() = XUnit.TryFindAvailable xunits
    let tryFindResultReady() = reservationStations.TryFindResultReady()

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

    member fu.Execute() =
        let tryCompute xunit = fu.Halt <- XUnit.TryCompute fu.Compute xunit

        match tryFindReadyStation(), tryFindAvailableXUnit() with
        | Some r, Some x -> XUnits(x).Set(RS(r)) | _ -> ()
        
        xunits |> Array.iter tryCompute
        

    member fu.Write() =
        let cdb = CDB.GetInstance
        match tryFindResultReady() with
        | Some r ->
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
        halt

    static member GetInstance = instance
    static member Reset() = instance <- fun rsRef -> IntegerUnit(cfg, rsRef)

and TrapUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.TrapUnit
    static let mutable instance = fun rsRef -> TrapUnit(cfg, rsRef) 

    let queue = Queue()

    let RS' = RS.TrapUnit rsRef
    let tryFindEmptyStation() = RS'.TryFindEmpty()
    let RS(r) = RS'.[r]

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
        
        match tryFindEmptyStation() with
        | Some r -> 
            if      RegisterStat(rs).Qi.IsSome 
            then    RS(r).Qj <- RegisterStat(rs).Qi
            else    RS(r).Vj <- Regs(rs); RS(r).Qj <- None
            
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
            queue.Enqueue(r)

        | None -> tu.LastInsert <- None; tu.Stall <- true
        

    override tu.Compute _ =
        let r = queue.Dequeue() :?> ReservationStation
        printfn "r ===>\n%O" r
        //printfn "Compute Trap, RS(r).Vj, RS(r).Vk ==>  %A, %A" (RS(r).Vj) (RS(r).Vk)
        let halt, result = RS(r).Op |> function
            | Some op -> 
                match op.Name, RS(r).A with
                | "halt", _ -> true, 0
                | "dumpGPR", _ -> false, RS(r).Vj
                | "dumpFPR", _ -> false, RS(r).Vj
                | "dumpSTR", Some A' -> false, Memory.GetInstance.[A']
                | _ -> failwith "invalid trap unit instruction"
            | None -> false, 0
        printfn "trap Result:  %A" result
        RS(r).Result <- result
        halt

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
//        [| cast iu; cast tu; cast bu; cast mu; cast fpu |]
        [| cast tu; cast iu; cast bu; cast mu; cast fpu |]

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

    member fu.Write() = allfu |> Array.tryPick (fun u -> u.Write())

    member fu.Execute() = allfu |> Array.iter (fun u -> u.Execute())

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

    member fu.Halt = allfu |> Array.forall (fun u -> u.Halt)
    member fu.Stall = allfu |> Array.forall (fun u -> u.Stall)

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
       | Some idx -> 
        //printfn "rs & exec"
        sprintf "%s%s" (getRSInfo()) (getExecInfo allfu.[idx])
       | None -> 
        //printfn "rs only"; 
        getRSInfo()
    
    static member GetInstance = instance
    static member Reset() = 
        IntegerUnit.Reset()
        TrapUnit.Reset()
        BranchUnit.Reset()
        MemoryUnit.Reset()
        FloatingPointUnit.Reset()
        instance <- FunctionalUnits()