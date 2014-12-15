//module FsDLX.Tomasulo.FunctionalUnits
namespace FsDLX.Tomasulo


open System.Collections
open FsDLX.Common

//// This design includes container classes that contain a group of the same time of 
//// functional units (for example, integer units) and the reservation stations that 
//// feed them.  Thus, there is a int unit container, floating point unit container, 
//// memory unit container, trap unit container, and a branch unit container.  Each 
//// of these classes inherit from the FUnitContainer class that contains functionality 
//// common to all subclasses.  For example, each container class contains a FUManager 
//// and a RStationManager and methods that for executing, issuing, writing, updating 
//// the reservation stations, clearing the reservation stations, etc. This can be 
//// inherited from the FUnitContainer class. Code that is unique to a specific 
//// functional unit type can be placed in the descendent class.  For example, the 
//// IntUnitContainer class contains the method that computes the result of an 
//// instruction handled by the integer functional unit.
//[<AbstractClass>]
//type FUContainer(cfg:Config.FU) =
//    member val FUManager  = FUManager(cfg) with get
//    member val RSManager  = RSManager(cfg) with get
//
//// The FUManager contains an array of functional units and is in charge of managing 
//// that array (initializing, clearing, etc.).  Similarly, the RStationManager contains 
//// an array of reservation stations and is in charge of managing that array (initializing, 
//// clearing, updating them with a CDB value, etc.)
//and FUManager(cfg:Config.FU) =
//    member val FUnits = FUnit.ArrayInit cfg
//
//
//
//and RSManager(cfg:Config.FU) =
//    member val RStations = ReservationStation.ArrayInit(cfg)
//
//// The FUnit class contains the fields that are necessary for keeping track of what is 
//// going on in a particular functional unit: busy, max cycles, remaining cycles, and a 
//// reference to the station containing the instruction that is currently being executed 
//// by the functional unit. It also contains methods that access or modify an individual 
//// functional unit.
//and FUnit(cfg:Config.FU) =
//    member val Busy = false with get, set
//    member val MaxCycles = cfg.XCycles with get
//    member val RemainingCycles = cfg.XCycles with get, set
//    member val RSRef = 0
//
//    static member ArrayInit (cfg:Config.FU) = 
//        Array.init cfg.XUnitCount (fun _ -> FUnit(cfg))



[<AbstractClass>]
type FU(cfg:Config.FU) =
    member val RS = ReservationStation.ArrayInit cfg with get, set
    
    member val MaxCycles        = cfg.XCycles
    member val RemainingCycles  = cfg.XCycles with get, set
    member val Busy             = false with get, set
    

    member fu.FindEmptyStation() =
        fu.RS |> Array.tryFindIndex (fun r -> r.IsEmpty())
    
    member fu.Finished() = fu.RS |> Array.forall (fun r -> not r.Busy)

    member fu.Clear() = fu |> function
        | :? MemoryUnit as mu -> 
            mu.LoadBuffer |> Array.iter ReservationStation.ClearIfResultWritten
            mu.StoreBuffer|> Array.iter ReservationStation.ClearIfResultWritten
        | _ ->
            fu.RS |> Array.iter ReservationStation.Clear
        
    member fu.UpdateRS (cdb:CDB) =
        fu.RS |> Array.iter (fun r -> 
            if r.Busy && cdb.Src = r.Qj.Value then 
                r.Qj <- None; r.Vj <- cdb.Result.Value
            if r.Busy && cdb.Src = r.Qk.Value then
                r.Qk <- None; r.Vk <- cdb.Result.Value)

        
//    member fu.Write(cdb:CDB) =
//        //let cdb = CDB()
//        let r = fu.RS |> Array.findIndex (fun r -> r.ResultReady)
//        fu.RS.[r].ResultWritten <- true
//        cdb.Src     <- fu.RS.[r].Name
//        cdb.Result  <- fu.RS.[r].Result |> Some
//        //cdb

    override fu.ToString() =
        //sprintf "MaxCycles: %d; Remaining Cycles: %d; Busy: %A" fu.MaxCycles fu.RemainingCycles fu.Busy
        [for r in fu.RS do
            //if not(r.IsEmpty()) then
                yield sprintf "%s\n" (r.ToString())]
        |> List.reduce (+)

    static member ArrayInit (cfg:Config.FU) (init:_ -> FU) =
        Array.init cfg.XUnitCount init

//    static member InitAll() =
//        let init = FU.ArrayInit 
//        [|  init Config.FU.IntegerUnit          (fun _ -> IntegerUnit()         :> FU)
//            init Config.FU.TrapUnit             (fun _ -> TrapUnit()            :> FU)
//            init Config.FU.BranchUnit           (fun _ -> BranchUnit()          :> FU)
//            init Config.FU.MemoryUnit           (fun _ -> MemoryUnit()          :> FU)
//            init Config.FU.FloatingPointUnit    (fun _ -> FloatingPointUnit()   :> FU) |]
//        |> Array.concat
    
    

    abstract member Issue   : int -> RegisterFile -> bool
    abstract member Execute : ReservationStation -> unit
    abstract member Write   : CDB -> unit
    
   
        

and IntegerUnit(gpr:GeneralPurposeRegister) =
    inherit FU(Config.FU.IntegerUnit)

    static let instructions = 
        [| "addi",  Instruction("addi", DstReg.GPR 11, S1Reg.GPR 6, Imm.A(16,31))
           "nop",   Instruction("nop")
           "add",   Instruction("add", DstReg.GPR 16, S1Reg.GPR 6, S2Reg.GPR 11)
           "sub",   Instruction("sub", DstReg.GPR 16, S1Reg.GPR 6, S2Reg.GPR 11)
           "and",   Instruction("and", DstReg.GPR 16, S1Reg.GPR 6, S2Reg.GPR 11)
        |] |> Map.ofArray

//    member iu.ADDI    (r:ReservationStation) = r.Vj + r.A.Value
//    member iu.NOP     _ = ()
//    member iu.ADD     (r:ReservationStation) = r.Vj + r.Vk
//    member iu.SUB     (r:ReservationStation) = r.Vj - r.
//    member iu.AND     (r:ReservationStation) = ()
//    member iu.OR      (r:ReservationStation) = ()
//    member iu.XOR     (r:ReservationStation) = ()
//    member iu.MOVF    (r:ReservationStation) = ()
//    member iu.MOVFP2I (r:ReservationStation) = ()
//    member iu.MOVI2FP (r:ReservationStation) = ()

    override iu.Issue i regs =         
        let instruction = instructions.[(Opcode.ofInstructionInt i).Name]
        let rs = instruction.GetRS i regs
        iu.FindEmptyStation() |> function
        | Some r -> false
        | None -> true
        
//        let stall = ref false
//        iu.RS |> Array.iter (fun r ->
//            if r.IsEmpty() then
//                regs.[i.rs].Qi |> function
//                | Some _ -> r.Qj <- regs.[i.rs].Qi
//                | None -> r.Vj <- regs.[i.rs].Contents; r.Qj <- None
//        
//                regs.[i.rt].Qi |> function
//                | Some _ -> r.Qk <- regs.[i.rt].Qi
//                | None -> r.Vk <- regs.[i.rs].Contents; r.Qk <- None
//        
//                r.Busy <- true; regs.[i.rd].Qi <- Some(string r)
//            else
//                stall := true)
////                if not(r.Busy) then
////                    r.Busy <- true
////                    r.Op <- Some i.Opcode
////                    r.A <- i.imm
////                    
////                    if regs.[i.rs].IsAvailable() 
////                    then    r.Vj <- regs.[i.rs].Contents
////                    else    r.Qj <- regs.[i.rs].Qi
////                else
////                    stall <- true )            
//        !stall
        

    override iu.Execute r =
        r.Result <-  r.Op.Value.Name |> function
            | "addi" -> r.Vj + r.A.Value
            | "nop" -> 0
            | "add" -> r.Vj + r.Vk
            | "sub" -> r.Vj - r.Vk
            | "and" -> r.Vj &&& r.Vk
            | "or" -> r.Vj ||| r.Vk
            | "xor" -> r.Vj ^^^ r.Vk
            | "movf" -> 0
            | "movfp2i" -> 0
            | "movi2fp" -> 0
            | _ -> failwith "invalid integer unit instruction"

    override iu.Write(cdb:CDB) = ()



and TrapUnit() =
    inherit FU(Config.FU.TrapUnit)

    static let regBits = (6,10)
    static let trap0 = S1Reg.R(regBits) |> Instruction.InitTrap
    static let trap1 = S1Reg.R(regBits) |> Instruction.InitTrap
    static let trap2 = S1Reg.F(regBits) |> Instruction.InitTrap
    static let trap3 = S1Reg.R(regBits) |> Instruction.InitTrap
    
    override tu.Issue i regs = false

    override tu.Execute r = ()

    override tu.Write(cdb:CDB) = ()

and BranchUnit() =
    inherit FU(Config.FU.BranchUnit)

    override bu.Issue i regs = false

    override bu.Execute i = ()

    override bu.Write(cdb:CDB) = ()

and MemoryUnit() =
    inherit FU(Config.FU.MemoryUnit)

    let mutable xQueue = List.empty<int>
    let mutable wQueue = List.empty<int>

    member val LoadBuffer   = ReservationStation.ArrayInit Config.FU.MemoryUnit with get, set
    member val StoreBuffer  = ReservationStation.ArrayInit Config.FU.MemoryUnit with get, set

    override mu.Issue i regs = false

    override mu.Execute i = ()

    override mu.Write(cdb:CDB) = ()

and FloatingPointUnit() =
    inherit FU(Config.FU.FloatingPointUnit)

    override mu.Issue i regs = false
    override mu.Execute i = ()

    override mu.Write(cdb:CDB) = ()

and FunctionalUnits() =
    let iu, tu =
    //let iu, tu, bu, mu, fpu = 
        FU.ArrayInit Config.FU.IntegerUnit          (fun _ -> IntegerUnit()         :> FU),
        FU.ArrayInit Config.FU.TrapUnit             (fun _ -> TrapUnit()            :> FU)
//        FU.ArrayInit Config.FU.BranchUnit           (fun _ -> BranchUnit()          :> FU),
//        FU.ArrayInit Config.FU.MemoryUnit           (fun _ -> MemoryUnit()          :> FU),
//        FU.ArrayInit Config.FU.FloatingPointUnit    (fun _ -> FloatingPointUnit()   :> FU)

//    let all = [| iu; tu; bu; mu; fpu |] |> Array.concat
    let all = [| iu; tu |] |> Array.concat

    let rsMap = all |> Array.map (fun fu -> 
        fu.RS |> Array.map (fun r -> r.Name ))

    
    member val IntegerUnits = iu with get
    member val TrapUnits    = tu with get
//    member val BranchUnits  = bu with get
//    member val MemoryUnits  = mu with get
//    member val FPUnits      = fpu with get
    
    member val All          = all with get

    member fu.GetIntegerUnit(i) = fu.IntegerUnits.[i]   :?> IntegerUnit
    member fu.GetTrapUnit()     = fu.TrapUnits.[0]      :?> TrapUnit
//    member fu.GetBranchUnit()   = fu.BranchUnits.[0]    :?> BranchUnit
//    member fu.GetMemoryUnit(i)  = fu.MemoryUnits.[i]    :?> MemoryUnit
//    member fu.GetFPUnit(i)      = fu.FPUnits.[i]        :?> FloatingPointUnit

    member fu.Issue (i:int) = 
        InstructionKind.ofInt i |> function
        | Integer -> ()
        | Trap -> ()
        | Branch -> ()
        | Memory -> ()
        | FloatingPoint -> ()
        
        false
//        let i, kind = 
//            Instruction.ofInt i,
//            InstructionKind.ofInt i
        
//        kind |> function
//        | Integer -> ()
////            let r = fu.IntegerUnits |> Array.tryPick (fun u -> u.FindEmptyStation())
////            if r.IsSome then
////                fu.IntegerUnits.
//        | Trap -> ()
//        | Branch -> ()
//        | Memory -> ()
//        | FloatingPoint -> ()


    member fu.Finished() = fu.All |> Array.forall (fun fu -> fu.Finished())

    // The update reservation stations step will use the name and result on the CDB to 
    // update each reservation station and register file.
    member fu.UpdateReservationStations cdb =
        all |> Array.iter (fun u -> u.UpdateRS cdb)

    // The clear reservation stations step will clear each reservation station who has 
    // written in the current clock cycle. (Note there may be more than one since not 
    // all writes go to the CDB -- for example, stores and branches.)
    member fu.ClearReservationStations() =
        all |> Array.iter (fun u -> u.Clear())
