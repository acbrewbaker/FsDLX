module FsDLX.Tomasulo.FunctionalUnit

[<AbstractClass>]
type FU(cfg:FunctionalUnitConfig, rsPrefix:string) =
    let rs = Array.init<ReservationStation> cfg.nReservationStations (fun i -> ReservationStation.Init (rsPrefix + string i))
    
    member fu.MaxCycles = cfg.nExecutionTime
    member fu.RSCount   = cfg.nReservationStations
     
    member val RS = rs with get
    member val CyclesRemaining = 0 with get, set
    member val Busy = false with get, set



type IntegerUnit() =
    inherit FU(FunctionalUnitConfig.IntegerUnit, "Integer")


type TrapUnit() =
    inherit FU(FunctionalUnitConfig.TrapUnit, "Trap")

type BranchUnit() =
    inherit FU(FunctionalUnitConfig.BranchUnit, "Branch")

type MemoryUnit() =
    inherit FU(FunctionalUnitConfig.MemoryUnit, "Memory")

type FloatingPointUnit() =
    inherit FU(FunctionalUnitConfig.FloatingPointUnit, "FloatingPoint")