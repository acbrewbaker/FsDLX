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
       
    member val CurrentInstruction : int option = None with get, set

    member fu.FindEmptyStation() =
        fu.RS |> Array.tryFindIndex (fun r -> 
            //printfn "tryfind:  %O" r
            r.IsEmpty())
    
    member fu.Finished() = fu.RS |> Array.forall (fun r -> not r.Busy)

    member fu.Clear() = fu |> function
        | :? MemoryUnit as mu -> 
            mu.LoadBuffer |> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
            mu.StoreBuffer|> Array.iter (fun (r:ReservationStation) -> r.ClearIfResultWritten()) //ReservationStation.ClearIfResultWritten
        | _ ->
            fu.RS |> Array.iter (fun r -> r.Clear()) //ReservationStation.Clear
        
    member fu.UpdateRS () =
        let cdb = CDB.GetInstance
        fu.RS |> Array.iter (fun r ->
            if r.Qj.IsSome then 
                if r.Busy && cdb.Src = r.Qj.Value then 
                    r.Qj <- None; r.Vj <- cdb.Result.Value
            if r.Qk.IsSome then
                if r.Busy && cdb.Src = r.Qk.Value then
                    r.Qk <- None; r.Vk <- cdb.Result.Value )


    member fu.Execute() =
        fu.CurrentInstruction <- fu.RS |> Array.tryFindIndex (fun r -> r.IsReady())
        //let mutable halt = false
        if not(fu.Busy) then
            if fu.CurrentInstruction.IsSome then fu.Busy <- true; fu.RemainingCycles <- fu.RemainingCycles - 1
        
        else
            if fu.RemainingCycles > 0 then fu.RemainingCycles <- fu.RemainingCycles - 1
            if fu.CurrentInstruction.IsSome then
                let r = fu.CurrentInstruction.Value
                if fu.RemainingCycles = 0 && not(fu.RS.[r].ResultReady) then
                    fu.Compute r
                    fu.RS.[r].ResultReady <- true
                    fu.Busy <- false
        
        false


    member fu.Write() =
        let cdb = CDB.GetInstance
        fu.RS |> Array.tryFindIndex (fun r -> r.ResultReady) |> function
        | Some r ->
            fu.RS.[r].ResultWritten <- true
            cdb.Result <- fu.RS.[r].Result |> Some
            cdb.Src <- fu.RS.[r].Name
            true
        | None -> false

    override fu.ToString() =
        //sprintf "MaxCycles: %d; Remaining Cycles: %d; Busy: %A" fu.MaxCycles fu.RemainingCycles fu.Busy
        let s1 = 
            sprintf "%sUNIT RESERVATION STATIONS"
                (   fu |> function
                    | :? IntegerUnit -> "INT"
                    | :? TrapUnit -> "TRAP"
                    | :? BranchUnit -> "BRANCH"
                    | :? MemoryUnit -> "MEM"
                    | :? FloatingPointUnit -> "FP"
                    | _ -> "FU")
        let s2 = 
            [for r in fu.RS do
                if not(r.IsEmpty()) then
                    yield sprintf "%s\n" (r.ToString())]
        sprintf "%s\n%s" s1 (if s2.IsEmpty then "" else s2 |> List.reduce (+))

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
    
    
    abstract member Instructions        : Map<string, Instruction>
    abstract member Insert   : int -> bool
    abstract member Compute  : int -> unit
    //abstract member Write    : CDB -> unit
    
   
        

and IntegerUnit() =
    inherit FU(Config.FU.IntegerUnit)
    

    override iu.Instructions = 
        [   "addi",     Instruction.ADDI
            "nop",      Instruction.NOP
            "add",      Instruction.ADD
            "sub",      Instruction.SUB
            "and",      Instruction.AND
            "or",       Instruction.OR
            "xor",      Instruction.XOR
            "movf",     Instruction.MOVF
            "movfp2i",  Instruction.MOVFP2I
            "movi2fp",  Instruction.MOVI2FP ] |> Map.ofList

   
    
    override iu.Insert i =
        let gpr = GPR.GetInstance
        let fpr = FPR.GetInstance
        //let RS = base.RS
        let opcode = Opcode.ofInstructionInt i
        let instruction = iu.Instructions.[opcode.Name]

        let rd, rs, rt, imm =
                instruction.rd,
                instruction.rs,
                instruction.rt,
                instruction.imm

        let regNum startBit = Convert.int2bits2reg i startBit
      
        let p x = printfn "%A" x; x

        iu.FindEmptyStation() |> function
        | Some r -> 
//            RS.[r].Busy <- true
//            RS.[r].Op <- Some opcode
            
            //printfn "Opcode: %O" opcode
            (rd, rs, rt, imm) |> function
                | DstReg.GPR rd, S1Reg.GPR rs, S2Reg.NONE, Imm.A(a,b) -> 
                    //printfn "case1.0"
                    iu.RS.[r].Busy <- true
                    iu.RS.[r].Op <- Some opcode
            
                    
                    let rd, rs, rt = 
                        regNum rd, 
                        regNum rs, 
                        (Convert.int2bits2int i a b)
                    iu.RS.[r].A <- Some rt

                    //printfn "case1.1"
                    if      gpr.[rs].IsAvailable()
                    then    iu.RS.[r].Vj <- gpr.[rs].Contents
                    else    iu.RS.[r].Qj <- gpr.[rs].Qi
                    
                    //printfn "case1.2"
                    printfn "RS.[r].Name %A" (iu.RS.[r].Name)
                    gpr.[rd].Qi <- iu.RS.[r].Name |> Some
                    //printfn "%A" (gpr.[rt].Qi.ToString())

                | DstReg.GPR rd, S1Reg.GPR rs, S2Reg.GPR rt, Imm.NONE ->
                    printfn "case2"
                    
                    let rd, rs, rt = regNum rd, regNum rs, regNum rt
                    
                    if      gpr.[rs].IsAvailable()
                    then    iu.RS.[r].Vj <- gpr.[rs].Contents
                    else    iu.RS.[r].Qj <- gpr.[rs].Qi

                    if      gpr.[rt].IsAvailable()
                    then    iu.RS.[r].Vk <- gpr.[rt].Contents
                    else    iu.RS.[r].Qk <- gpr.[rt].Qi

                    gpr.[rd].Qi <- iu.RS.[r].Name |> Some

                | DstReg.FPR rd, S1Reg.FPR rs, S2Reg.NONE, Imm.NONE -> ()

                | DstReg.GPR rd, S1Reg.FPR rs, S2Reg.NONE, Imm.NONE -> ()

                | DstReg.FPR rd, S1Reg.GPR rs, S2Reg.NONE, Imm.NONE -> ()
                
                | _ -> failwith "didnt match any int instruction case"
                
            false
        | None -> true

    override iu.Compute r =
        let vj, vk, a =
            iu.RS.[r].Vj,
            iu.RS.[r].Vk,
            iu.RS.[r].A.Value

        iu.RS.[r].Result <-  iu.RS.[r].Op.Value.Name |> function
            | "addi" -> vj + a
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
//        false
        
    


and TrapUnit() =
    inherit FU(Config.FU.TrapUnit)

    
    override tu.Instructions =
        [   "trap0", Instruction.TRAP0
            "trap1", Instruction.TRAP1
            "trap2", Instruction.TRAP2
            "trap3", Instruction.TRAP3  ] |> Map.ofList

    override tu.Insert i = 
        let gpr = GPR.GetInstance
        let fpr = FPR.GetInstance
        let RS = tu.RS
        let opcode = Opcode.ofInstructionInt i
        let funCode = Convert.int2bits2int i 27 31
        let rs = Convert.int2bits2reg i 6
        tu.FindEmptyStation() |> function
        | Some r -> 
            RS.[r].Busy <- true
            
            funCode |> function
                | 0 ->
                    opcode.Name <- "halt"; RS.[r].Op <- Some opcode 
                    if      gpr.[0].IsAvailable()
                    then    RS.[r].Vj <- gpr.[rs].Contents
                    else    RS.[r].Qj <- gpr.[rs].Qi
                    
                    //printfn "case1.2"
                    
//                    printfn "Trap0: Halt!"
                    
                | 1 ->
                    opcode.Name <- "dumpgpr"; RS.[r].Op <- Some opcode 
                    if      gpr.[rs].IsAvailable()
                    then    RS.[r].Vj <- gpr.[rs].Contents
                    else    RS.[r].Qj <- gpr.[rs].Qi
                    
                    printfn "Trap1: %O" (gpr.[rs])
                    

                | 2 ->
                    opcode.Name <- "dumpfpr"; RS.[r].Op <- Some opcode 
                    if      fpr.[rs].IsAvailable()
                    then    RS.[r].Vj <- fpr.[rs].Contents
                    else    RS.[r].Qj <- fpr.[rs].Qi                    
//                    printfn "Trap2: %O" (fpr.[rs])
                    
                | 3 ->
                    opcode.Name <- "dumpstr"; RS.[r].Op <- Some opcode 
                    if      gpr.[rs].IsAvailable()
                    then    RS.[r].Vj <- gpr.[rs].Contents
                    else    RS.[r].Qj <- gpr.[rs].Qi
                    
//                    printfn "Trap3: %O" (fpr.[rs])
                    
                | _ -> failwith "didnt match any trap instruction case"
            false
        | None -> true

    override tu.Compute r =
        let vj, vk, a =
            tu.RS.[r].Vj,
            tu.RS.[r].Vk,
            tu.RS.[r].A.Value

        let halt, result = tu.RS.[r].Op.Value.Name |> function
            | "halt" -> true, 0
            | "dumpgpr" -> false, vj
            | "dumpfpr" -> false, vj
            | "dumpstr" -> false, Memory.GetInstance.[a]
            | _ -> failwith "invalid trap unit instruction"
        printfn "trap Result:  %A" result
        tu.RS.[r].Result <- result
//        halt

    
and BranchUnit() =
    inherit FU(Config.FU.BranchUnit)

    override bu.Instructions =
        [   "beqz", Instruction.BEQZ
            "j",    Instruction.J
            "jr",   Instruction.JR
            "jal",  Instruction.JAL
            "jalr", Instruction.JALR    ] |> Map.ofList

    override bu.Insert i = false

    override bu.Compute r = ()

   
and MemoryUnit() =
    inherit FU(Config.FU.MemoryUnit)

    

    let mutable xQueue = List.empty<int>
    let mutable wQueue = List.empty<int>

    member val LoadBuffer   = ReservationStation.ArrayInit Config.FU.MemoryUnit with get, set
    member val StoreBuffer  = ReservationStation.ArrayInit Config.FU.MemoryUnit with get, set

    override mu.Instructions =
        [   "lw",   Instruction.LW
            "lf",   Instruction.LF
            "sw",   Instruction.SW
            "sf",   Instruction.SF  ] |> Map.ofList

    override mu.Insert i = false

    override mu.Compute r = ()

    
and FloatingPointUnit() =
    inherit FU(Config.FU.FloatingPointUnit)

    override fpu.Instructions =
        [   "addf",     Instruction.ADDF
            "subf",     Instruction.SUBF
            "multf",    Instruction.MULTF
            "divf",     Instruction.DIVF
            "mult",     Instruction.MULT
            "div",      Instruction.DIV
            "cvtf2i",   Instruction.CVTF2I
            "cvti2f",   Instruction.CVTI2F  ] |> Map.ofList

    override fpu.Insert i = false
    override fpu.Compute r = ()

    
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

//    member fu.Issue (i:int) = 
//        InstructionKind.ofInt i |> function
//        | Integer -> ()
//        | Trap -> ()
//        | Branch -> ()
//        | Memory -> ()
//        | FloatingPoint -> ()
//        
//        false
////        let i, kind = 
////            Instruction.ofInt i,
////            InstructionKind.ofInt i
//        
////        kind |> function
////        | Integer -> ()
//////            let r = fu.IntegerUnits |> Array.tryPick (fun u -> u.FindEmptyStation())
//////            if r.IsSome then
//////                fu.IntegerUnits.
////        | Trap -> ()
////        | Branch -> ()
////        | Memory -> ()
////        | FloatingPoint -> ()


    member fu.Finished() = fu.All |> Array.forall (fun fu -> fu.Finished())

    // The update reservation stations step will use the name and result on the CDB to 
    // update each reservation station and register file.
    member fu.UpdateReservationStations() =
        all |> Array.iter (fun u -> u.UpdateRS()) //CDB.GetInstance)

    // The clear reservation stations step will clear each reservation station who has 
    // written in the current clock cycle. (Note there may be more than one since not 
    // all writes go to the CDB -- for example, stores and branches.)
    member fu.ClearReservationStations() =
        all |> Array.iter (fun u -> u.Clear())

    override fu.ToString() =
        fu.All |> Array.map (fun u -> 
            let r = u.RS |> Array.filter (fun r -> r.Busy)
            if r.Length > 0
            then r |> Array.map (fun r -> r.ToString() + "\n")
            else [|""|])
        |> Array.concat
        |> Array.reduce (+)