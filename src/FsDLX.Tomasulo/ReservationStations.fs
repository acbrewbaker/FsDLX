namespace FsDLX.Tomasulo

open System.Collections.Generic
open FsDLX.Common

type RSGroup = RSGroup of ReservationStation[] with
    member rsg.Length = (RSGroup.Value rsg) |> Array.length
    member rsg.Iter f = (RSGroup.Value rsg) |> Array.iter f
    member rsg.Iteri f = (RSGroup.Value rsg) |> Array.iteri f
    member rsg.ForAll f = (RSGroup.Value rsg) |> Array.forall f
    member rsg.TryFind f = (RSGroup.Value rsg) |> Array.tryFind f
    member rsg.TryPick f = (RSGroup.Value rsg) |> Array.tryPick f
    member rsg.Fold f s = (RSGroup.Value rsg) |> Array.fold f s
    member rsg.Filter f = (RSGroup.Value rsg) |> Array.filter f
    member rsg.BusyOnly = rsg.Filter (fun r -> r.Busy)
    
    member rsg.TryFindEmpty() = rsg.TryFind (fun r -> r.IsEmpty())
    member rsg.TryFindResultReady() = rsg.TryFind (fun r -> r.ResultReady)
    member rsg.TryFindReadyStation() = rsg.TryFind (fun r -> r.OperandsAvailable())

    member rsg.Clear() = rsg.Iter (fun r -> r.Clear())
    member rsg.Finished() = rsg.ForAll (fun r -> r.Busy = false)
    
    member rsg.FilterByPrefix p = 
        rsg.Filter (fun r -> r.Name.StartsWith(p)) |> RSGroup

    member private rsg.GetMap() =
        let kvp (r:ReservationStation) = (r.Name, r)
        rsg |> RSGroup.Value |> Array.map kvp |> Map.ofArray

    member rsg.Item with get(r:ReservationStation) = rsg.GetMap().[r.Name]

    member rsg.Update() = 
        let cdb = CDB.GetInstance
        rsg.Iter (fun r ->
            match r.Qj with Some Qj -> if r.Busy && cdb.Src = Qj then   
                                            r.Qj <- None
                                            r.Vj <- cdb.Result 
                                        | _ -> ()
            
            match r.Qk with Some Qk -> if r.Busy && cdb.Src = Qk then
                                            r.Qk <- None
                                            r.Vk <- cdb.Result
                                        | _ -> ()
            )

    static member Value (RSGroup rsg) = rsg
    static member Init(cfg:Config.FunctionalUnit) =
        Array.init cfg.rsCount (fun i -> ReservationStation.Init (cfg.rsPrefix + string i))
        |> RSGroup

    static member InitAll =
        let init (cfg:Config.FunctionalUnit) = Array.init cfg.rsCount (fun i -> ReservationStation.Init (cfg.rsPrefix + string i))
        Config.FunctionalUnit.All |> Array.map init |> Array.concat |> RSGroup

    static member IntUnitInit() = RSGroup.Init Config.FunctionalUnit.IntegerUnit
    static member TrapUnitInit() = RSGroup.Init Config.FunctionalUnit.TrapUnit
    static member MemoryUnitInit() = RSGroup.Init Config.FunctionalUnit.MemoryUnit
    static member BranchUnitInit() = RSGroup.Init Config.FunctionalUnit.BranchUnit
    static member FloatingPointUnitInit() = RSGroup.Init Config.FunctionalUnit.FloatingPointUnit

// The ReservationStation class contains the fields of an individual reservation station: 
// name, busy, opcode, Vj, Vk, Qj, Qk, A, result, resultReady, resultWritten.  It also 
// contains methods to access or modify an individual reservation station.
and ReservationStation =
    {
        Name                    : string
        mutable Busy            : bool
        mutable Op              : Opcode option
        mutable Vj              : int
        mutable Vk              : int
        mutable Qj              : string option
        mutable Qk              : string option
        mutable A               : int option
        mutable ResultReady     : bool
        mutable ResultWritten   : bool
        mutable Result          : int
    }

    member rsg.Clear() =
        if rsg.ResultWritten then
            rsg.Busy <- false
            rsg.Op <- None
            rsg.Vj <- 0; rsg.Vk <- 0
            rsg.Qj <- None; rsg.Qk <- None
            rsg.A <- None
            rsg.ResultReady <- false
            rsg.ResultWritten <- false
            rsg.Result <- 0

    member rsg.OperandsAvailable() =
        rsg.Busy                 &&
        rsg.Qj.IsNone            &&
        rsg.Qk.IsNone            &&
        not(rsg.ResultReady)

    member rsg.IsEmpty() = 
        rsg.Busy = false         &&
        rsg.Op.IsNone            &&
        rsg.Vj   = 0             &&
        rsg.Vk   = 0             &&
        rsg.Qj   = None          &&
        rsg.Qk   = None          &&
        rsg.A    = None

    member rsg.Dump() =
        sprintf "%s  %O  %O  %s  %s  %O  %O  %O  %O  %O  %s"
            rsg.Name rsg.Busy rsg.Op
            (Convert.int2hex rsg.Vj)
            (Convert.int2hex rsg.Vk) 
            rsg.Qj rsg.Qk
            rsg.A
            rsg.ResultReady rsg.ResultWritten
            (Convert.int2hex rsg.Result)

    override rsg.ToString() =
        sprintf "%s  %O  %O  %s  %s  %s  %s  %s"
            rsg.Name rsg.Busy (Opcode.Opt2String(rsg.Op))
            (Convert.int2hex rsg.Vj)
            (Convert.int2hex rsg.Vk) 
            (Convert.strOption2str(rsg.Qj)) (Convert.strOption2str(rsg.Qk))
            (Convert.intOption2str rsg.A)

    static member Init name =
        {   Name = name; Busy = false; Op = None; 
            Vj = 0; Vk = 0; Qj = None; Qk = None; A = None
            ResultReady = false;
            ResultWritten = false;
            Result = 0 }

and ReservationStationQueue = Queue<ReservationStation>