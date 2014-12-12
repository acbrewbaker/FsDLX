//[<AutoOpen>]
//module FsDLX.Tomasulo.Interface
namespace FsDLX.Tomasulo

type Hardware(memSize:int) =
    let cdb = CDB()
    member val Clock            = Clock.GetInstance with get
    member val PC               = PC.GetInstance with get
    member val CDB              = cdb with get
    member val Mem              = Memory.GetInstance memSize
    member val GPR              = RegisterFile.InitGPR with get
    member val FPR              = RegisterFile.InitFPR with get
    member val FunctionalUnits  = FU.InitAll cdb with get
    
    new() = Hardware(Config.DefaultMemorySize)

    member h.UpdateReservationStations() =
        h.FunctionalUnits |> Array.iter (fun fu -> fu.UpdateRS())
            


type Simulator() =
    let h = Hardware()
    
    member val x = 0 with get, set


    member s.Run() = ()



