//module FsDLX.Tomasulo.FunctionalUnits
namespace FsDLX.Tomasulo

open System.Collections

type IFunctionalUnit =
    abstract RS             : ReservationStation[]
    abstract Busy           : bool
    abstract MaxCycles      : int
    abstract CyclesRemaining: int

type IIntegerUnit =
    inherit IFunctionalUnit
    abstract member ADDI    : string -> bool
    abstract member NOP     : string -> bool
    abstract member ADD     : string -> bool
    abstract member SUB     : string -> bool
    abstract member AND     : string -> bool
    abstract member OR      : string -> bool
    abstract member XOR     : string -> bool
    abstract member MOVF    : string -> bool
    abstract member MOVFP2I : string -> bool
    abstract member MOVI2FP : string -> bool

type ITrapUnit =
    inherit IFunctionalUnit
    abstract member TRAP    : string -> bool

type IBranchUnit =
    inherit IFunctionalUnit
    abstract member BEQZ    : string -> bool

type IMemoryUnit =
    inherit IFunctionalUnit
    abstract member LW      : string -> bool

type IFloatingPointUnit =
    inherit IFunctionalUnit
    abstract member ADDF    : string -> bool
    

[<AbstractClass>]
type FU(cfg:Config.FU) =
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
        
    member fu.UpdateRS(cdb:CDB) =
        fu.RS |> Array.iter (fun r -> 
            if r.Busy && cdb.Src = r.Qj.Value then 
                r.Qj <- None; r.Vj <- cdb.Result.Value
            if r.Busy && cdb.Src = r.Qk.Value then
                r.Qk <- None; r.Vk <- cdb.Result.Value)


    member fu.Write() =
        let cdb = CDB()
        let r = fu.RS |> Array.findIndex (fun r -> r.ResultReady)
        fu.RS.[r].ResultWritten <- true
        cdb.Src     <- fu.RS.[r].Name
        cdb.Result  <- fu.RS.[r].Result |> Some
        cdb

    abstract member ComputeResult : int -> unit

    



    //abstract member ArrayInit : unit -> FU[]


and IntegerUnit() =
    inherit FU(Config.FU.IntegerUnit)

    //override iu.ArrayInit() = 

and TrapUnit() =
    inherit FU(Config.FU.TrapUnit)

and BranchUnit() =
    inherit FU(Config.FU.BranchUnit)

and MemoryUnit() =
    inherit FU(Config.FU.MemoryUnit)

    let mutable xQueue = List.empty<int>
    let mutable wQueue = List.empty<int>

    member val LoadBuffer   = ReservationStation.ArrayInit Config.FU.MemoryUnit with get, set
    member val StoreBuffer  = ReservationStation.ArrayInit Config.FU.MemoryUnit with get, set

and FloatingPointUnit() =
    inherit FU(Config.FU.FloatingPointUnit)


    