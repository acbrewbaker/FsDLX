//module FsDLX.Tomasulo.FunctionalUnits
namespace FsDLX.Tomasulo


open System.Collections
open FsDLX.Common

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
        match xu.CurrentRS with
        | Some r ->
            xu.RemainingCycles <- xu.RemainingCycles - 1
            if  xu.RemainingCycles = 0 &&
                not(RS.[r].ResultReady)
            then
                compute r
                RS.[r].ResultReady <- true
        | None -> ()

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

    //member val private UnitInfoString = "" with get, set
    member val CurrentRS : string option = None with get, set
    member val ExecRS : string option = None with get,set
    member val RS = reservationStations with get
    member val XUnits = xunits with get
    
    member fu.IsBusy() = fu.XUnits |> Array.forall (fun xunit -> xunit.Busy)
    member fu.IsExecuting() = not(fu.XUnits |> Array.forall (fun u -> not u.Busy))

    member fu.Finished() = fu.RS.Contents |> Array.forall (fun r -> not r.Busy)

    member fu.Clear() = fu |> function
//        | :? MemoryUnit as mu -> 
//            mu.LoadBuffer |> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
//            mu.StoreBuffer|> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
        | _ ->
            fu.RS.Contents |> Array.iter (fun r -> r.Clear()) //ReservationStation.Clear


    member fu.Execute() =
        let XUnits(i) = fu.XUnits.[i]
        (fu.RS.TryFindReady(), XUnit.TryFindNotBusy fu.XUnits) |> function
        | Some r, Some x ->
            let rsId = fu.RS.[r].Name 
            XUnits(x).Busy <- true
            XUnits(x).CurrentRS <- Some(rsId); fu.CurrentRS <- XUnits(x).CurrentRS
            XUnits(x).Cycle fu.RS fu.Compute
        | _ -> ()
        fu.XUnits |> Array.iter (fun xunit -> 
            match xunit.Busy, xunit.RemainingCycles with
            | true, rc when rc > 0 -> 
                xunit.Cycle fu.RS fu.Compute
            | true, rc when rc <= 0 -> 
                xunit.Reset(); fu.ExecRS <- fu.CurrentRS; fu.CurrentRS <- None
            | _ -> ())   

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
        cdb'

    member fu.Dump() = fu.XUnits |> Array.map (sprintf "%O\n") |> Array.reduce (+) 
    
    abstract member Insert   : Instruction -> bool
    abstract member Compute  : string -> unit

and IntegerUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let instance rsRef = IntegerUnit(cfg, rsRef)
    
    override iu.Insert instruction =
        let opcode, rd, rs, rt = 
            instruction.Opcode,
            instruction.DstReg, 
            instruction.S1Reg, 
            instruction.S2Reg
            
        let RS(r:int) = iu.RS.[r]
        let Regs(i) = (Regs.GetInstance instruction.asInt).[i]
        let RegisterStat(i) = (RegisterStat.GetInstance instruction.asInt).[i]
        
        match iu.RS.TryFindNotBusy() with
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
            false
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

        let opcode, funcCode, rd, rs, rt =
            instruction.Opcode,
            instruction.FuncCode,
            instruction.DstReg,
            instruction.S1Reg,
            instruction.S2Reg
        
        tu.RS.TryFindNotBusy() |> function
        | Some r -> 
            if rs <> S1Reg.NONE then
                if      RegisterStat(rs).Qi.IsSome 
                then    RS(r).Qj <- RegisterStat(rs).Qi
                else    RS(r).Vj <- Regs(rs); RS(r).Qj <- None
            
            if rt <> S2Reg.NONE then
                if      RegisterStat(rt).Qi.IsSome
                then    RS(r).Qk <- RegisterStat(rt).Qi
                else    RS(r).Vk <- Regs(rt); RS(r).Qk <- None
            
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
                | "dumpGPR", _ -> false, RS(r).Vj
                | "dumpFPR", _ -> false, RS(r).Vj
                | "dumpSTR", Some A' -> false, Memory.GetInstance.[A']
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
    
and FunctionalUnits private () =
    static let instance = FunctionalUnits()

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

    let getRSInfo (funit:FunctionalUnit) =
        sprintf "%sUNIT RESERVATION STATIONS\n%O"
            (match funit with
            | :? IntegerUnit -> "INT"
            | :? TrapUnit -> "TRAP"
            | :? BranchUnit -> "BRANCH"
            | :? MemoryUnit -> "MEM"
            | :? FloatingPointUnit -> "FP"
            | _ -> "Functional ")
            (funit.RS)

    let getExecInfo (funit:FunctionalUnit) =
        let execinfo = 
            sprintf "\nEXECUTING: instruction in station %O" 
                    (Convert.strOption2str funit.ExecRS)
        funit.ExecRS <- None; execinfo
        
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

    member val InfoString = "" with get,set

    member fu.Write() = allfu |> Array.tryPick (fun u -> u.Write())

    member fu.Execute() = allfu |> Array.iteri (fun i u -> u.Execute())

    member fu.Issue i = 
        match i with
        | Integer(_) -> fu.IntegerUnit.Insert i
        | Trap(_) -> fu.TrapUnit.Insert i
        | Branch(_) -> fu.BranchUnit.Insert i
        | Memory(_) -> fu.MemoryUnit.Insert i
        | FloatingPoint(_) -> fu.FloatingPointUnit.Insert i

    member fu.AllFinished() = allfu |> Array.forall (fun u -> u.Finished())

    // The update reservation stations step will use the name and result on the CDB to 
    // update each reservation station and register file.
    member fu.UpdateReservationStations cdb = RS.Update(fu.ReservationStations, cdb)

    // The clear reservation stations step will clear each reservation station who has 
    // written in the current clock cycle. (Note there may be more than one since not 
    // all writes go to the CDB -- for example, stores and branches.)
    member fu.ClearReservationStations() =
        allfu |> Array.iter (fun u -> u.Clear())

    

    member fu.Dump() =
        allfu |> Array.map (fun u -> u.Dump()) |> Array.map ((+) "\n") |> Array.reduce (+)

//    override fu.ToString() = 
//        fu.All |> Array.map (sprintf "%O\n") |> Array.reduce (+)

    override fu.ToString() = 
       match allfu |> Array.tryFindIndex (fun u -> u.ExecRS.IsSome) with
       | Some idx -> sprintf "%s%s" (getRSInfo allfu.[idx]) (getExecInfo allfu.[idx])
       | None -> 
        match allfu |> Array.tryFindIndex (fun u -> u.CurrentRS.IsSome) with
        | Some idx -> getRSInfo allfu.[idx]
        | None -> getRSInfo allfu.[0]
//    override fu.ToString() =
//        fu.All |> Array.tryPick (fun funit ->
//            match funit.CurrentRS with
//            | Some _ ->
//                let s1 = 
//                    sprintf "%sUNIT RESERVATION STATIONS"
//                            (   funit |> function
//                                | :? IntegerUnit -> "INT"
//                                | :? TrapUnit -> "TRAP"
//                                | :? BranchUnit -> "BRANCH"
//                                | :? MemoryUnit -> "MEM"
//                                | :? FloatingPointUnit -> "FP"
//                                | _ -> "FunctionalUnit")
//
//                let s2 = 
//                    if      funit.CurrentRS.IsSome
//                    then    sprintf "\nEXECUTING: instruction in station %O" (Convert.strOption2str funit.CurrentRS) 
//                    else    ""
//
//                Some(sprintf "%s\n%O%s" s1 funit.RS s2 )
//            | None -> None)
//        |> function
//        | Some s -> s
//        | None -> ""
    
    static member GetInstance = instance