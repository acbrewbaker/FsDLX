//module FsDLX.Tomasulo.FunctionalUnits
namespace FsDLX.Tomasulo

open System
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
         
    member xu.Cycle (RS:RS) (compute:string -> bool) =  
        let mutable halt = false
        match xu.CurrentRS with
        | Some r ->
            xu.RemainingCycles <- xu.RemainingCycles - 1
            if  xu.RemainingCycles = 0 &&
                not(RS.[r].ResultReady)
            then
                halt <- compute r
                RS.[r].ResultReady <- true
        | None -> ()
        halt

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

    member val ExecRS : string option = None with get,set
    member val LastInsert : string option = None with get, set
    member val RS = reservationStations with get
    member val XUnits = xunits with get
    member val Stall = false with get, set
    member val Halt = false with get, set

    member fu.IsBusy() = fu.XUnits |> Array.forall (fun xunit -> xunit.Busy)
    member fu.IsExecuting() = not(fu.XUnits |> Array.forall (fun u -> not u.Busy))

    member fu.Finished() = fu.RS.Contents |> Array.forall (fun r -> not r.Busy)

    member fu.Clear() = fu |> function
//        | :? MemoryUnit as mu -> 
//            mu.LoadBuffer |> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
//            mu.StoreBuffer|> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
        | _ ->
            //if not(fu.IsBusy()) then fu.ExecRS <- None
            //fu.LastInsert <- None
            fu.RS.Contents |> Array.iter (fun r -> r.Clear()) //ReservationStation.Clear


    member fu.Execute() =
        let XUnits(i) = fu.XUnits.[i]
        (fu.RS.TryFindReady(), XUnit.TryFindNotBusy fu.XUnits) |> function
        | Some r, Some x ->
            let rsId = Some(fu.RS.[r].Name)
            XUnits(x).Busy <- true
            XUnits(x).CurrentRS <- rsId
            fu.ExecRS <- rsId
            fu.Halt <- XUnits(x).Cycle fu.RS fu.Compute
        | _ -> ()
        fu.XUnits |> Array.iter (fun xunit -> 
            match xunit.Busy, xunit.RemainingCycles with
            | true, rc when rc > 0 -> 
                fu.Halt <- xunit.Cycle fu.RS fu.Compute
                fu.ExecRS <- xunit.CurrentRS
                
            | true, rc when rc <= 0 -> 
                //fu.ExecRS <- None
                xunit.Reset()       
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
    
    abstract member Insert   : Instruction -> unit
    abstract member Compute  : string -> bool

and IntegerUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let instance rsRef = IntegerUnit(cfg, rsRef)
    
    override iu.Insert instruction =
        let opcode, rd, rs, rt, imm = 
            instruction.Opcode,
            instruction.DstReg, 
            instruction.S1Reg, 
            instruction.S2Reg,
            instruction.imm
            
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

            let rsId = Some(RS(r).Name)
            RegisterStat(rd).Qi <- rsId
            iu.LastInsert <- rsId; //printfn "did insert"

            RS(r).A <- imm
        
        | _ -> iu.Stall <- true

    override iu.Compute r =
        let halt = false
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
                | "nop" -> fun _ _ -> 0
                | op -> printfn "%A" op; failwith "invalid integer unit instruction"
            | None -> failwith "tried to compute with no opcode"
        halt

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
            tu.LastInsert <- Some(RS(r).Name)

        | None -> tu.LastInsert <- None; tu.Stall <- true

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
        //printfn "trap Result:  %A" result
        RS(r).Result <- result
        halt

    static member GetInstance = instance
    
and BranchUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.BranchUnit
    static let instance rsRef = BranchUnit(cfg, rsRef)

    override bu.Insert instruction = ()

    override bu.Compute r = false

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

    override mu.Insert instruction = ()

    override mu.Compute r = false

    static member GetInstance = instance
    
and FloatingPointUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.FloatingPointUnit
    static let instance rsRef = FloatingPointUnit(cfg, rsRef)

    override fpu.Insert i = ()
    override fpu.Compute r = false

    static member GetInstance = instance
    
and FunctionalUnits private () =
    static let instance = new FunctionalUnits()

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

    let getRSInfo() = // (funit:FunctionalUnit) =
        match allfu |> Array.tryFindIndex (fun u -> u.LastInsert.IsSome) with
        | Some idx ->
            let funit = allfu.[idx] 
            sprintf "%sUNIT RESERVATION STATIONS\n%O"
                (match funit with
                | :? IntegerUnit -> "INT"
                | :? TrapUnit -> "TRAP"
                | :? BranchUnit -> "BRANCH"
                | :? MemoryUnit -> "MEM"
                | :? FloatingPointUnit -> "FP"
                | _ -> "Functional ")
                (funit.RS)
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

    member fu.Issue i =
        fu.All |> Array.iter (fun u -> u.LastInsert <- None) 
        //printfn "Issue ==> %O" i
        match i with
        | Integer(_) -> iu.Insert i
        | Trap(_) -> tu.Insert i
        | Branch(_) -> bu.Insert i
        | Memory(_) -> mu.Insert i
        | FloatingPoint(_) -> fpu.Insert i

    member fu.Halt = allfu |> Array.forall (fun u -> u.Halt)
    member fu.Stall = allfu |> Array.forall (fun u -> u.Stall)

    member fu.AllFinished() = allfu |> Array.forall (fun u -> u.Finished())

    member fu.UpdateReservationStations cdb = RS.Update(fu.ReservationStations, cdb)

    member fu.ClearReservationStations() =
        allfu |> Array.iter (fun u -> u.Clear())

    member fu.Dump() =
        allfu |> Array.map (fun u -> u.Dump()) |> Array.map ((+) "\n") |> Array.reduce (+)

    override fu.ToString() = 
       match allfu |> Array.tryFindIndex (fun u -> u.ExecRS.IsSome) with
       | Some idx -> sprintf "%s%s" (getRSInfo()) (getExecInfo allfu.[idx])
       | None -> getRSInfo()
    
    static member GetInstance = instance
    interface IDisposable with member this.Dispose() = ()