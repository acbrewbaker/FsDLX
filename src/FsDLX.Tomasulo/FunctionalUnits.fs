//module FsDLX.Tomasulo.FunctionalUnits
namespace FsDLX.Tomasulo


open System.Collections
open FsDLX.Common

//// This design includes container classes that contain a group of the same time of 
//// functional units (for example, integer units) and the reservation stations that 
//// feed them.  Thus, there is a int unit container, floating point unit container, 
//// memory unit container, trap unit container, and a branch unit container.  Each 
//// of these classes inherit from the FunctionalUnitnitContainer class that contains functionality 
//// common to all subclasses.  For example, each container class contains a FunctionalUnitManager 
//// and a RStationManager and methods that for executing, issuing, writing, updating 
//// the reservation stations, clearing the reservation stations, etc. This can be 
//// inherited from the FunctionalUnitnitContainer class. Code that is unique to a specific 
//// functional unit type can be placed in the descendent class.  For example, the 
//// IntUnitContainer class contains the method that computes the result of an 
//// instruction handled by the integer functional unit.
//[<AbstractClass>]
//type FunctionalUnitContainer(cfg:Config.FunctionalUnit) =
//    member val FunctionalUnitManager  = FunctionalUnitManager(cfg) with get
//    member val RSManager  = RSManager(cfg) with get
//
//// The FunctionalUnitManager contains an array of functional units and is in charge of managing 
//// that array (initializing, clearing, etc.).  Similarly, the RStationManager contains 
//// an array of reservation stations and is in charge of managing that array (initializing, 
//// clearing, updating them with a CDB value, etc.)
//and FunctionalUnitManager(cfg:Config.FunctionalUnit) =
//    member val FunctionalUnitnits = FunctionalUnitnit.ArrayInit cfg
//
//
//
//and RSManager(cfg:Config.FunctionalUnit) =
//    member val RStations = ReservationStation.ArrayInit(cfg)
//
//// The FunctionalUnitnit class contains the fields that are necessary for keeping track of what is 
//// going on in a particular functional unit: busy, max cycles, remaining cycles, and a 
//// reference to the station containing the instruction that is currently being executed 
//// by the functional unit. It also contains methods that access or modify an individual 
//// functional unit.
//and FunctionalUnitnit(cfg:Config.FunctionalUnit) =
//    member val Busy = false with get, set
//    member val MaxCycles = cfg.XCycles with get
//    member val RemainingCycles = cfg.XCycles with get, set
//    member val RSRef = 0
//
//    static member ArrayInit (cfg:Config.FunctionalUnit) = 
//        Array.init cfg.XUnitCount (fun _ -> FunctionalUnitnit(cfg))


type XUnit(maxCycles:int) =
    member val MaxCycles = maxCycles with get
    member val RemainingCycles = maxCycles with get, set
    member val Busy = false with get, set
    member val CurrentRS : string option = None with get, set

    member xu.Reset() =
        if xu.RemainingCycles = 0 then
            xu.RemainingCycles <- xu.MaxCycles
            xu.Busy <- false
            xu.CurrentRS <- None

    member xu.Cycle (RS:RS) (compute:string -> unit) =  
        xu.RemainingCycles <- xu.RemainingCycles - 1
        let r = xu.CurrentRS.Value
        if  xu.RemainingCycles = 0 &&
            not(RS.[r].ResultReady)
        then
            compute r
            RS.[r].ResultReady <- true
            //xu.Reset()

    member xu.Update(RS:RS, compute:string -> unit) =
        RS.TryFindReady() |> function
        | Some r ->
//            printfn "XU busy? %A;    Some r: %O" xu.Busy (RS.[r])
            if not(xu.Busy) then
                xu.Busy <- true
                xu.CurrentRS <- RS.[r].Name |> Some
                xu.Cycle RS compute
            xu.CurrentRS
        | None -> 
            if xu.Busy && xu.RemainingCycles > 0 then xu.Cycle RS compute
            xu.CurrentRS

    override xu.ToString() =
        sprintf "Busy? %O, RemainingCycles? %O, CurrentInstruction? %O"
            xu.Busy
            xu.RemainingCycles
            xu.CurrentRS

    static member Update(xunits:XUnit[], RS:RS, compute:string -> unit) =
        xunits |> Array.tryPick (fun xunit -> xunit.Update(RS, compute))

    static member Reset(xunits:XUnit[]) =
        xunits |> Array.iter (fun xunit -> xunit.Reset())

    static member TryFindNotBusy (xunits:XUnit[]) =
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

    member val private UnitInfoString = "" with get, set
    member val private CurrentRS : string option = None with get, set
    member val RS = reservationStations with get
    member val XUnits = xunits with get
    
    member fu.IsBusy() = fu.XUnits |> Array.forall (fun xunit -> xunit.Busy)

    member fu.Finished() = fu.RS.Contents |> Array.forall (fun r -> not r.Busy)

    member fu.Clear() = fu |> function
//        | :? MemoryUnit as mu -> 
//            mu.LoadBuffer |> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
//            mu.StoreBuffer|> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
        | _ ->
            fu.RS.Contents |> Array.iter (fun r -> r.Clear()) //ReservationStation.Clear

    member fu.Execute() = 
        fu.CurrentRS <- XUnit.Update(fu.XUnits, fu.RS, fu.Compute)
        fu.XUnits |> Array.iter (fun xunit -> xunit.Reset())
        fu.UpdateInfoString()

    member fu.Write() =
        let cdb = CDB.GetInstance
        let RS(r:int) = fu.RS.[r]
        let RegisterStat(x) = RegisterFile.GetInstance.[x]
        let Regs(x) = RegisterFile.GetInstance.[x]
        let cdb' = fu.RS.TryFindResultReady() |> function
            | Some r ->
                RS(r).ResultWritten <- true
                cdb.Result <- RS(r).Result
                cdb.Src <- RS(r).Name
                Some(cdb)
            | None -> None
//        fu.UpdateInfoString()
        cdb'

    member fu.Dump() = fu.XUnits |> Array.map (sprintf "%O\n") |> Array.reduce (+) 

    member fu.UpdateInfoString() =
        let s1 = 
            if fu.CurrentRS.IsSome then
                sprintf "%sUNIT RESERVATION STATIONS"
                    (   fu |> function
                        | :? IntegerUnit -> "INT"
                        | :? TrapUnit -> "TRAP"
                        | :? BranchUnit -> "BRANCH"
                        | :? MemoryUnit -> "MEM"
                        | :? FloatingPointUnit -> "FP"
                        | _ -> "FunctionalUnit")
            else ""

        let s2 = if fu.CurrentRS.IsSome then sprintf "\nEXECUTING: instruction in station %s" (fu.CurrentRS.Value) else ""

        fu.UnitInfoString <- sprintf "%s\n%O%s" s1 fu.RS s2

    override fu.ToString() = fu.UnitInfoString

    member fu.TryInsert (instruction:Instruction) =
        let opcode, rd, rs, rt = 
            instruction.Opcode,
            instruction.DstReg, 
            instruction.S1Reg, 
            instruction.S2Reg
            
        let RS(r:int) = fu.RS.[r]
        let Regs(i) = (Regs.GetInstance instruction.asInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.asInt).[i]
        
        match fu.RS.TryFindNotBusy() with
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
            RegisterStat(rd).Qi <- Some(RS(r).Name)

            RS(r).A <- instruction.imm

            false, Some(fun _ -> fu.UpdateInfoString())
        | _ -> true, None

    abstract member Insert   : Instruction -> bool
    abstract member Compute  : string -> unit
  

and IntegerUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let instance rsRef = IntegerUnit(cfg, rsRef)
    
    override iu.Insert instruction =
        match iu.TryInsert instruction with
        | false, Some f -> f(); false
        | _ -> true

    override iu.Compute r =
        let RS(r:string) = iu.RS.[r]
       
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
                | _ -> failwith "invalid integer unit instruction"
            | None -> failwith "tried to compute with no opcode"
            

    static member GetInstance = instance


and TrapUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.TrapUnit
    static let instance rsRef = TrapUnit(cfg, rsRef) 

    override tu.Insert instruction = 
        let RS(r:int) = tu.RS.[r]
        let Regs(i) = (Regs.GetInstance instruction.asInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.asInt).[i]

        let opcode, funcCode, rs =
            instruction.Opcode,
            instruction.FuncCode,
            instruction.S1Reg
        
        tu.RS.TryFindNotBusy() |> function
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
            
            false

        | None -> true

    override tu.Compute r =
        let RS(r:string) = tu.RS.[r]
        
        let halt, result = RS(r).Op |> function
            | Some op -> 
                match op.Name, RS(r).A with
                | "halt", _ -> true, 0
                | "dumpgpr", _ -> false, RS(r).Vj
                | "dumpfpr", _ -> false, RS(r).Vj
                | "dumpstr", Some A' -> false, Memory.GetInstance.[A']
                | _ -> failwith "invalid trap unit instruction"
            | None -> false, 0
        printfn "trap Result:  %A" result
        RS(r).Result <- result
//        halt

    static member GetInstance = instance
    
and BranchUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.BranchUnit
    static let instance rsRef = BranchUnit(cfg, rsRef)

    override bu.Insert instruction = false

    override bu.Compute r = ()

    static member GetInstance = instance

and MemoryUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.MemoryUnit
    static let instance rsRef = MemoryUnit(cfg, rsRef)
    let mutable xQueue = List.empty<int>
    let mutable wQueue = List.empty<int>

//    member val LoadBuffer   = ReservationStation.ArrayInit Config.FunctionalUnit.MemoryUnit with get, set
//    member val StoreBuffer  = ReservationStation.ArrayInit Config.FunctionalUnit.MemoryUnit with get, set
//

    override mu.Insert instruction = false

    override mu.Compute r = ()

    static member GetInstance = instance
    
and FloatingPointUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.FloatingPointUnit
    static let instance rsRef = FloatingPointUnit(cfg, rsRef)

    override fpu.Insert i = false
    override fpu.Compute r = ()

    static member GetInstance = instance
    
and FunctionalUnits() =
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

    member val IntegerUnit = iu with get
    member val IntegerUnitReservationStations = !iuRS with get
    member val TrapUnit = tu with get
    member val TrapUnitReservationStations = !tuRS with get
    member val BranchUnit = bu with get
    member val BranchUnitReservationStations = !buRS with get
    member val MemoryUnit = mu with get
    member val MemoryUnitReservationStations = !muRS with get
    member val FloatingPointUnit = fpu with get
    member val FloatingPointUnitReservationStations = !fpuRS with get

    member val All = allfu with get

    member val ReservationStations = allrs with get

    member fu.Issue = function
        | Integer(_) -> fu.IntegerUnit.Insert
        | Trap(_) -> fu.TrapUnit.Insert
        | Branch(_) -> fu.BranchUnit.Insert
        | Memory(_) -> fu.MemoryUnit.Insert
        | FloatingPoint(_) -> fu.FloatingPointUnit.Insert

    member fu.AllFinished() = allfu |> Array.forall (fun u -> u.Finished())

    // The update reservation stations step will use the name and result on the CDB to 
    // update each reservation station and register file.
    member fu.UpdateReservationStations cdb = RS.Update(fu.ReservationStations, cdb)

    // The clear reservation stations step will clear each reservation station who has 
    // written in the current clock cycle. (Note there may be more than one since not 
    // all writes go to the CDB -- for example, stores and branches.)
    member fu.ClearReservationStations() =
        allfu |> Array.iter (fun u -> u.Clear())

    member fu.UpdateInfoStrings() =
        allfu |> Array.iter (fun u -> u.UpdateInfoString())

    override fu.ToString() = 
        fu.All |> Array.map (sprintf "%O\n") |> Array.reduce (+)
        