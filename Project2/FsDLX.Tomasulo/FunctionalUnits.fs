//module FsDLX.Tomasulo.FunctionalUnits
namespace FsDLX.Tomasulo

open System.Collections


    

[<AbstractClass>]
type FU(cfg:Config.FU, cdb:CDB) =
    member val RS = ReservationStation.ArrayInit cfg with get, set
    member fu.RSCount = cfg.RSCount
    member fu.XCycles = cfg.XCycles

    member fu.Busy() = fu.RS |> Array.forall (fun r -> not r.Busy)

    member fu.Clear() = fu |> function
        | :? MemoryUnit as mu -> 
            mu.LoadBuffer |> Array.iter ReservationStation.ClearIfResultWritten
            mu.StoreBuffer|> Array.iter ReservationStation.ClearIfResultWritten
        | _ ->
            fu.RS |> Array.iter ReservationStation.Clear
        
    member fu.UpdateRS() =
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

    static member InitAll(cdb:CDB) =
        let init (cfg:Config.FU) (init:_ -> FU) = Array.init cfg.XUnitCount init
        [|  init Config.FU.IntegerUnit          (fun _ -> IntegerUnit(cdb)         :> FU)
            init Config.FU.TrapUnit             (fun _ -> TrapUnit(cdb)            :> FU)
            init Config.FU.BranchUnit           (fun _ -> BranchUnit(cdb)          :> FU)
            init Config.FU.MemoryUnit           (fun _ -> MemoryUnit(cdb)          :> FU)
            init Config.FU.FloatingPointUnit    (fun _ -> FloatingPointUnit(cdb)   :> FU) |]
        |> Array.concat
    
    abstract member Issue   : (Instruction -> bool)
    abstract member Execute : unit -> bool
    abstract member Write   : CDB -> unit
    
   
        

and IntegerUnit(cdb:CDB) =
    inherit FU(Config.FU.IntegerUnit, cdb)

    member iu.ADDI    rs rd imm = false
    member iu.NOP     () = false
    member iu.ADD     rs rt rd = false
    member iu.SUB     rs rt rd = false
    member iu.AND     rs rt rd = false
    member iu.OR      rs rt rd = false
    member iu.XOR     rs rt rd = false
    member iu.MOVF    rs rt rd = false
    member iu.MOVFP2I rs rt rd = false
    member iu.MOVI2FP rs rt rd = false

    override iu.Issue = function
        | Instruction.IntegerInstruction -> false
        | _ -> failwith "Can only issue integer instructions to integer unit"

    override iu.Execute() = false

    override iu.Write(cdb:CDB) = ()



and TrapUnit(cdb:CDB) =
    inherit FU(Config.FU.TrapUnit, cdb)

    override iu.Issue = function
        | Instruction.TrapInstruction -> false
        | _ -> failwith "Can only issue trap instructions to trap unit"

    override iu.Execute() = false

    override iu.Write(cdb:CDB) = ()

and BranchUnit(cdb:CDB) =
    inherit FU(Config.FU.BranchUnit, cdb)

    override iu.Issue = function
        | Instruction.BranchInstruction -> false
        | _ -> failwith "Can only issue branch instructions to branch unit"

    override iu.Execute() = false

    override iu.Write(cdb:CDB) = ()

and MemoryUnit(cdb:CDB) =
    inherit FU(Config.FU.MemoryUnit, cdb)

    let mutable xQueue = List.empty<int>
    let mutable wQueue = List.empty<int>

    member val LoadBuffer   = ReservationStation.ArrayInit Config.FU.MemoryUnit with get, set
    member val StoreBuffer  = ReservationStation.ArrayInit Config.FU.MemoryUnit with get, set

    override iu.Issue = function
        | Instruction.MemoryInstruction -> false
        | _ -> failwith "Can only issue memory instructions to memory unit"

    override iu.Execute() = false

    override iu.Write(cdb:CDB) = ()

and FloatingPointUnit(cdb:CDB) =
    inherit FU(Config.FU.FloatingPointUnit, cdb)

    override iu.Issue = function
        | Instruction.FloatingPointInstruction -> false
        | _ -> failwith "Can only issue floating point instructions to floating point unit"

    override iu.Execute() = false

    override iu.Write(cdb:CDB) = ()
    