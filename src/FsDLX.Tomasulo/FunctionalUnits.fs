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

type CurrentInstruction =
    {
        mutable Op : Opcode option
        mutable RSId : int option
    }

    member ci.Update(rs:RS) =
        ci.RSId <- rs.TryFindReady()
        ci.Op <- if ci.RSId.IsSome then rs.[ci.RSId.Value].Op else None       

    override ci.ToString() =
        sprintf "%O in RS %O" ci.Op ci.RSId 

    static member Init() = { Op = None; RSId = None }


type XUnit(maxCycles:int) =
    member val MaxCycles = maxCycles with get
    member val RemainingCycles = maxCycles with get, set
    member val Busy = false with get, set
    member val CurrentInstruction : int option = None with get, set

    member xu.Cycle() = xu.RemainingCycles <- xu.RemainingCycles - 1

    member xu.Update (RS:RS) (compute:int -> unit) =
        RS.TryFindReady() |> function
        | Some r ->
            if not(xu.Busy) then
                xu.Busy <- true
                xu.Cycle()
            else        
                if xu.RemainingCycles > 0 then xu.Cycle()
                if  xu.RemainingCycles = 0 &&
                    not(RS.[r].ResultReady)
                then
                    compute r
                    RS.[r].ResultReady <- true
                    xu.Busy <- false
         | _ -> ()

    override xu.ToString() =
        sprintf "Busy? %O, RemainingCycles? %O, CurrentInstruction? %O"
            xu.Busy
            xu.RemainingCycles
            xu.CurrentInstruction 

    static member TryFindNotBusy (xunits:XUnit[]) =
        xunits |> Array.tryFindIndex (fun xu -> not(xu.Busy))

[<AbstractClass>]
type FunctionalUnit (cfg:Config.FunctionalUnit, rsRef:RSGroupRef) as fu =
    let xunits = Array.init cfg.unitCount (fun _ -> XUnit(cfg.maxCycles))

    let reservationStations = fu |> function
        | :? IntegerUnit -> RS.IntegerUnit rsRef
        | :? TrapUnit -> RS.TrapUnit rsRef
//        | :? BranchUnit -> RS.BranchUnit rsRef
//        | :? MemoryUnit -> RS.MemoryUnit rsRef
//        | :? FloatingPointUnit -> RS.FloatingPointUnit rsRef
        | _ -> failwith "invalid reservation station group"


    member val RS = reservationStations with get
    member val XUnits = xunits with get
    

//    member fu.FindEmptyStation() =
//        !fu.RS |> Array.tryFindIndex (fun r -> 
//            //printfn "tryfind:  %O" r
//            r.IsEmpty())
    
    member fu.IsBusy() = fu.XUnits |> Array.forall (fun xunit -> xunit.Busy)

    member fu.Finished() = fu.RS.Contents |> Array.forall (fun r -> not r.Busy)
//        let finished (rs:RSGroupRef) = !rs |> Array.forall (fun r -> not r.Busy)
//        RS.ApplyFunction fu.RS finished

    member fu.Clear() = fu |> function
//        | :? MemoryUnit as mu -> 
//            mu.LoadBuffer |> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
//            mu.StoreBuffer|> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
        | _ ->
            fu.RS.Contents |> Array.iter (fun r -> r.Clear()) //ReservationStation.Clear
        
    

    member fu.Execute() = fu.XUnits |> Array.iter (fun xunit -> 
        xunit.Update fu.RS fu.Compute)
            
        


    member fu.Write() =
        let cdb = CDB.GetInstance
        fu.RS.TryFindReady() |> function
        | Some r ->
            fu.RS.[r].ResultWritten <- true
            cdb.Result <- fu.RS.[r].Result
            cdb.Src <- fu.RS.[r].Name
            Some(cdb)
        | None -> None

    member fu.Dump() = fu.XUnits |> Array.map (sprintf "%O\n") |> Array.reduce (+) 

    override fu.ToString() =
        //sprintf "MaxCycles: %d; Remaining Cycles: %d; Busy: %A" fu.MaxCycles fu.RemainingCycles fu.Busy
        let s1 = 
            sprintf "%sUNIT RESERVATION STATIONS"
                (   fu |> function
                    | :? IntegerUnit -> "INT"
                    | :? TrapUnit -> "TRAP"
//                    | :? BranchUnit -> "BRANCH"
//                    | :? MemoryUnit -> "MEM"
//                    | :? FloatingPointUnit -> "FP"
                    | _ -> "FunctionalUnit")
//        let s2 = 
//            [for r in fu.RS.Contents do
//                if not(r.IsEmpty()) then
//                    yield sprintf "%s\n" (r.ToString())]
        sprintf "%s\n%O" s1 fu.RS

//    static member ArrayInit (cfg:Config.FunctionalUnit) (init:_ -> FunctionalUnit) =
//        Array.init cfg.XUnitCount init
//
//    static member InitAll() =
//        let init = FunctionalUnit.ArrayInit 
//        [|  init Config.FunctionalUnit.IntegerUnit          (fun _ -> IntegerUnit()         :> FunctionalUnit)
//            init Config.FunctionalUnit.TrapUnit             (fun _ -> TrapUnit()            :> FunctionalUnit)
//            init Config.FunctionalUnit.BranchUnit           (fun _ -> BranchUnit()          :> FunctionalUnit)
//            init Config.FunctionalUnit.MemoryUnit           (fun _ -> MemoryUnit()          :> FunctionalUnit)
//            init Config.FunctionalUnit.FloatingPointUnit    (fun _ -> FloatingPointUnit()   :> FunctionalUnit) |]
//        |> Array.concat
    
//    abstract member Instructions        : Map<string, Instruction>
    abstract member Insert   : Instruction -> bool
    abstract member Compute  : int -> unit
    //abstract member Write    : CDB -> unit

//    override fu.ToString() = 
//        printfn "RS contents size: %A" fu.RS.Length
//        let onlyBusyRS = fu.RS.Contents |> Array.filter (fun r -> r.Busy)
//        printfn "OnlyBusyRS size: %A" onlyBusyRS.Length
////        printfn "onlyBusyRS: %A" onlyBusyRS
//        let strs = onlyBusyRS |> Array.map (sprintf "%O")
//        printfn "Strs ===> %A" strs
//        if onlyBusyRS.Length <> 0 then
//            
//            "not 0"
////            onlyBusyRS |> Array.fold (fun s r -> Array.append s [| r.ToString() + "\n" |] ) Array.empty<string>
////            |> Array.reduce (+)
//        else
//            ""
////            |> Array.fold 
////                (fun s r -> if r.Busy then Array.append s [| r.ToString() + "\n" |] else s) 
////                Array.empty<string>
        

and IntegerUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)
    
    static let cfg = Config.FunctionalUnit.IntegerUnit
    static let instance rsRef = IntegerUnit(cfg, rsRef)

//    override iu.Instructions = 
//        [   "addi",     Instruction.ADDI
//            "nop",      Instruction.NOP
//            "add",      Instruction.ADD
//            "sub",      Instruction.SUB
//            "and",      Instruction.AND
//            "or",       Instruction.OR
//            "xor",      Instruction.XOR
//            "movf",     Instruction.MOVF
//            "movfp2i",  Instruction.MOVFP2I
//            "movi2fp",  Instruction.MOVI2FP ] |> Map.ofList

   
    
    override iu.Insert i =
        let RS = iu.RS
        let opcode = i.Info.opcode
        let rd, rs, rt, imm = i.rd, i.rs, i.rt, i.imm
        
        let reg s = Convert.int2bits2reg i.Int s
        let immval (a,b) = Convert.int2bits2int i.Int a b

        let RS(r) = iu.RS.[r]

        let Regs = function
            | OperandReg.NONE -> Register.Init(0).Contents
            | OperandReg.GPR s -> GPR.GetInstance.[reg s].Contents
            | OperandReg.FPR s -> FPR.GetInstance.[reg s].Contents

        let RegisterStat = function
            | OperandReg.NONE -> Register.Init(0)
            | OperandReg.GPR s -> GPR.GetInstance.[reg s]
            | OperandReg.FPR s -> FPR.GetInstance.[reg s]  

        iu.RS.TryFindNotBusy() |> function
        | Some r ->
            if      RegisterStat(rs).Qi.IsSome 
            then    RS(r).Qj <- RegisterStat(rs).Qi
            else    RS(r).Vj <- Regs(rs); RS(r).Qj <- None
            
            if      RegisterStat(rt).Qi.IsSome
            then    RS(r).Qk <- RegisterStat(rt).Qi
            else    RS(r).Vk <- Regs(rt); RS(r).Qk <- None

            RS(r).Op <- Some opcode
            RS(r).Busy <- true
            RegisterStat(rd).Qi <- Some(cfg.rsPrefix + string r)

            RS(r).A <- imm |> function
            | Imm.NONE -> None
            | Imm.A imm -> Some(immval imm)

            false
        | _ -> true


    override iu.Compute r =
        //printfn "int unit compute"
        let RS = iu.RS
        let vj, vk, a =
            RS.[r].Vj,
            RS.[r].Vk,
            RS.[r].A
        
        RS.[r].Result <-  RS.[r].Op |> function
            | Some op -> op.Name |> function
                | "addi" -> if a.IsSome then vj + a.Value else vj
                | "nop" -> 0
                | "add" -> vj + vk
                | "sub" -> vj - vk
                | "and" -> vj &&& vk
                | "or" -> vj ||| vk
                | "xor" -> vj ^^^ vk
                | "movf" -> 0
                | "movfp2i" -> 0
                | "movi2fp" -> 0
                | _ -> failwith "invalid integer unit instruction"
            | None -> failwith "tried to compute with no opcode"
        //printfn "RStation in int.compute:\n%s" (RS.[r].Dump())
        //printfn "RStation in int.compute:\n%O" (RS.[r])
//        false

    static member GetInstance = instance



and TrapUnit private (cfg, rsRef) =
    inherit FunctionalUnit(cfg, rsRef)

    static let cfg = Config.FunctionalUnit.TrapUnit
    static let instance rsRef = TrapUnit(cfg, rsRef) 

//    override tu.Instructions =
//        [   "trap0", Instruction.TRAP0
//            "trap1", Instruction.TRAP1
//            "trap2", Instruction.TRAP2
//            "trap3", Instruction.TRAP3  ] |> Map.ofList

    override tu.Insert i = 
        let gpr = GPR.GetInstance
        let fpr = FPR.GetInstance
        let RS = tu.RS.Contents
        let opcode = i.Info.opcode
        let funCode = Convert.int2bits2int i.Int 27 31
        let rs = Convert.int2bits2reg i.Int 6
        RS |> Array.tryFindIndex (fun r -> not(r.Busy)) |> function
        | Some r -> 
            RS.[r].Busy <- true
            
            funCode |> function
                | 0 ->
                    opcode.Name <- "halt"; RS.[r].Op <- Some opcode 
                    if      gpr.[0].Qi.IsSome
                    then    RS.[r].Vj <- gpr.[rs].Contents
                    else    RS.[r].Qj <- gpr.[rs].Qi
                    
                    //printfn "case1.2"
                    
//                    printfn "Trap0: Halt!"
                    
                | 1 ->
                    opcode.Name <- "dumpgpr"; RS.[r].Op <- Some opcode 
                    if      gpr.[rs].Qi.IsSome
                    then    RS.[r].Vj <- gpr.[rs].Contents
                    else    RS.[r].Qj <- gpr.[rs].Qi
                    
                    printfn "Trap1: %O" (gpr.[rs])
                    

                | 2 ->
                    opcode.Name <- "dumpfpr"; RS.[r].Op <- Some opcode 
                    if      fpr.[rs].Qi.IsSome
                    then    RS.[r].Vj <- fpr.[rs].Contents
                    else    RS.[r].Qj <- fpr.[rs].Qi                    
//                    printfn "Trap2: %O" (fpr.[rs])
                    
                | 3 ->
                    opcode.Name <- "dumpstr"; RS.[r].Op <- Some opcode 
                    if      gpr.[rs].Qi.IsSome
                    then    RS.[r].Vj <- gpr.[rs].Contents
                    else    RS.[r].Qj <- gpr.[rs].Qi
                    
//                    printfn "Trap3: %O" (fpr.[rs])
                    
                | _ -> failwith "didnt match any trap instruction case"
            false
        | None -> true

    override tu.Compute r =
        let RS = tu.RS.Contents
        let vj, vk, a =
            RS.[r].Vj,
            RS.[r].Vk,
            RS.[r].A

        let halt, result = RS.[r].Op |> function
            | Some op -> op.Name |> function
                | "halt" -> true, 0
                | "dumpgpr" -> false, vj
                | "dumpfpr" -> false, vj
                | "dumpstr" -> false, if a.IsSome then Memory.GetInstance.[a.Value] else 0
                | _ -> failwith "invalid trap unit instruction"
            | None -> false, 0
        printfn "trap Result:  %A" result
        RS.[r].Result <- result
//        halt

    static member GetInstance = instance
    
//and BranchUnit private (maxCycles, rsRef) =
//    inherit FunctionalUnit(maxCycles, rsRef)
//
//    static let cfg = Config.FunctionalUnit.BranchUnit
//    static let instance rsRef = 
//        Array.init cfg.unitCount (fun _ -> BranchUnit(cfg.maxCycles, rsRef))
//
////    override bu.Instructions =
////        [   "beqz", Instruction.BEQZ
////            "j",    Instruction.J
////            "jr",   Instruction.JR
////            "jal",  Instruction.JAL
////            "jalr", Instruction.JALR    ] |> Map.ofList
//
//    override bu.Insert i = false
//
//    override bu.Compute r = ()
//
//    static member GetInstance rsRef = instance rsRef
//
//and MemoryUnit private (maxCycles, rsRef) =
//    inherit FunctionalUnit(maxCycles, rsRef)
//
//    static let cfg = Config.FunctionalUnit.MemoryUnit
//    static let instance rsRef = 
//        Array.init cfg.unitCount (fun _ -> MemoryUnit(cfg.maxCycles, rsRef))
//
//    let mutable xQueue = List.empty<int>
//    let mutable wQueue = List.empty<int>
//
////    member val LoadBuffer   = ReservationStation.ArrayInit Config.FunctionalUnit.MemoryUnit with get, set
////    member val StoreBuffer  = ReservationStation.ArrayInit Config.FunctionalUnit.MemoryUnit with get, set
////
////    override mu.Instructions =
////        [   "lw",   Instruction.LW
////            "lf",   Instruction.LF
////            "sw",   Instruction.SW
////            "sf",   Instruction.SF  ] |> Map.ofList
//
//    override mu.Insert i = false
//
//    override mu.Compute r = ()
//
//    static member GetInstance = instance
//    
//and FloatingPointUnit private (maxCycles, rsRef) =
//    inherit FunctionalUnit(maxCycles, rsRef)
//
//    static let cfg = Config.FunctionalUnit.FloatingPointUnit
//    static let instance rsRef = 
//        Array.init cfg.unitCount (fun _ -> FloatingPointUnit(cfg.maxCycles, rsRef))
//
////    override fpu.Instructions =
////        [   "addf",     Instruction.ADDF
////            "subf",     Instruction.SUBF
////            "multf",    Instruction.MULTF
////            "divf",     Instruction.DIVF
////            "mult",     Instruction.MULT
////            "div",      Instruction.DIV
////            "cvtf2i",   Instruction.CVTF2I
////            "cvti2f",   Instruction.CVTI2F  ] |> Map.ofList
//
//    override fpu.Insert i = false
//    override fpu.Compute r = ()
//
//    static member GetInstance = instance

    
and FunctionalUnits() =
    let iuCfg, tuCfg = //, buCfg, muCfg, fpuCfg = 
        Config.FunctionalUnit.IntegerUnit,
        Config.FunctionalUnit.TrapUnit
//        Config.FunctionalUnit.BranchUnit,
//        Config.FunctionalUnit.MemoryUnit,
//        Config.FunctionalUnit.FloatingPointUnit
    
//    let iuRS, tuRS, buRS, muRS, fpuRS =
//        RS.IntegerUnit(ReservationStation.ArrayInit iuCfg |> ref),
//        RS.TrapUnit(ReservationStation.ArrayInit tuCfg |> ref),
//        RS.BranchUnit(ReservationStation.ArrayInit buCfg |> ref),
//        RS.MemoryUnit(ReservationStation.ArrayInit muCfg |> ref),
//        RS.FloatingPointUnit(ReservationStation.ArrayInit fpuCfg |> ref)

    let p x = printfn "%O" x; x

    let iuRS, tuRS = //, buRS, muRS, fpuRS =
        ReservationStation.ArrayInit iuCfg |> ref,
        ReservationStation.ArrayInit tuCfg |> ref
//        ReservationStation.ArrayInit buCfg |> ref,
//        ReservationStation.ArrayInit muCfg |> ref,
//        ReservationStation.ArrayInit fpuCfg |> ref

    let iu, tu = //, bu, mu, fpu =
        //!iuRS |> Array.iter (printfn "%O")
        IntegerUnit.GetInstance iuRS,
        TrapUnit.GetInstance tuRS
//        BranchUnit.GetInstance buRS,
//        MemoryUnit.GetInstance muRS,
//        FloatingPointUnit.GetInstance fpuRS



    let allfu =
        let cast u = u :> FunctionalUnit
        let iu, tu = cast iu, cast tu
        [| iu; tu |] 

    let allrs = 
        [| 
            RS.IntegerUnit iuRS 
            RS.TrapUnit tuRS    |]
            //RS.BranchUnit buRS
            //RS.MemoryUnit muRS 
            //RS.FloatingPointUnit fpuRS |] //|> Array.concat

   
    member val IntegerUnit = iu with get
    member val IntegerUnitReservationStations = !iuRS with get
    member val TrapUnit = tu with get
    member val TrapUnitReservationStations = !tuRS with get
//    member val BranchUnit = bu with get
//    member val MemoryUnit = mu with get
//    member val FloatingPointUnit = fpu with get

    member val All = allfu with get

    member val ReservationStations = allrs with get

//    member fu.Execute() = 

    member fu.AllFinished() = allfu |> Array.forall (fun u -> u.Finished())

    member fu.GetExecuting() = "Executing: "

    member fu.GetStationIssued() = 
        sprintf "%sUNIT RESERVATION STATIONS" "INT"
//
//    member fu.GetIntegerUnit(i) = fu.IntegerUnits.[i]   :?> IntegerUnit
//    member fu.GetTrapUnit()     = fu.TrapUnits.[0]      :?> TrapUnit
////    member fu.GetBranchUnit()   = fu.BranchUnits.[0]    :?> BranchUnit
////    member fu.GetMemoryUnit(i)  = fu.MemoryUnits.[i]    :?> MemoryUnit
////    member fu.GetFPUnit(i)      = fu.FPUnits.[i]        :?> FloatingPointUnit
//
////    member fu.Issue (i:int) = 
////        InstructionKind.ofInt i |> function
////        | Integer -> ()
////        | Trap -> ()
////        | Branch -> ()
////        | Memory -> ()
////        | FloatingPoint -> ()
////        
////        false
//////        let i, kind = 
//////            Instruction.ofInt i,
//////            InstructionKind.ofInt i
////        
//////        kind |> function
//////        | Integer -> ()
////////            let r = fu.IntegerUnits |> Array.tryPick (fun u -> u.FindEmptyStation())
////////            if r.IsSome then
////////                fu.IntegerUnits.
//////        | Trap -> ()
//////        | Branch -> ()
//////        | Memory -> ()
//////        | FloatingPoint -> ()
//
//
//    member fu.Finished() = fu.All |> Array.forall (fun fu -> fu.Finished())
//
//    // The update reservation stations step will use the name and result on the CDB to 
//    // update each reservation station and register file.
    member fu.UpdateReservationStations() = fu.ReservationStations |> RS.Update
//
    // The clear reservation stations step will clear each reservation station who has 
    // written in the current clock cycle. (Note there may be more than one since not 
    // all writes go to the CDB -- for example, stores and branches.)
    member fu.ClearReservationStations() =
        allfu |> Array.iter (fun u -> u.Clear())
//
    override fu.ToString() = 
        RS.Filter(fu.ReservationStations, fun r -> r.Busy)
        |> Array.map (sprintf "%O\n") 
        //|> Array.map (fun s -> s.Trim())
        |> Array.reduce (+)
        
//        |> Array.map (sprintf "%O")
//        |> Array.reduce (+)
//
//            ""
        
//        fu.All |> Array.map (fun u -> 
//            u.RS.Contents |> Array.fold (fun s r -> if r.Busy then Array.append s [| r.ToString() + "\n" |] else s) Array.empty<string> )
//            |> Array.concat
//            |> Array.reduce (+)

        