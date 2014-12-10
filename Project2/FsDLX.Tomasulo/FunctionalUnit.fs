module FsDLX.Tomasulo.FunctionalUnit

[<AbstractClass>]
type FU(cfg:FunctionalUnitConfig) =
    
    abstract RS : ReservationStation[]

    member fu.MaxCycles = cfg.nExecutionTime
    member fu.RSCount   = cfg.nReservationStations
     

    member val CyclesRemaining = 0 with get, set
    member val Busy = false with get, set