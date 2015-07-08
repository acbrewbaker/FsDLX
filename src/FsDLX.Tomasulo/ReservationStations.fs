namespace FsDLX.Tomasulo

open System.Collections.Generic


type RSGroup = RSGroup of ReservationStation[] with
    //member rsg.Length = (RSGroup.Value rsg) |> Array.length
    member rsg.Iter f = (RSGroup.Value rsg) |> Array.iter f
    //member rsg.Iteri f = (RSGroup.Value rsg) |> Array.iteri f
    //member rsg.Map f = (RSGroup.Value rsg) |> Array.map f
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

    member rsg.Update(cdb:CDB option) = 
        match cdb with
        | Some cdb ->
            rsg.Iter (fun r ->
                match r.Qj with Some Qj -> if r.Busy && cdb.Src = Qj then   
                                                r.Qj <- None
                                                r.Vj <- cdb.Result 
                                            | _ -> ()
            
                match r.Qk with Some Qk -> if r.Busy && cdb.Src = Qk then
                                                r.Qk <- None
                                                r.Vk <- cdb.Result
                                            | _ -> ())
        | _ -> ()
    
    static member Value (RSGroup rsg) = rsg
    static member Init cfg = cfg |> ReservationStation.ArrayInit |> RSGroup

    static member InitAll =
        Config.FunctionalUnit.All |> Array.map ReservationStation.ArrayInit |> Array.concat |> RSGroup

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

    member rs.Clear() =
        if rs.ResultWritten then
            rs.Busy <- false
            rs.Op <- None
            rs.Vj <- 0; rs.Vk <- 0
            rs.Qj <- None; rs.Qk <- None
            rs.A <- None
            rs.ResultReady <- false
            rs.ResultWritten <- false
            rs.Result <- 0

    member rs.OperandsAvailable() =
        rs.Busy                 &&
        rs.Qj.IsNone            &&
        rs.Qk.IsNone            &&
        not(rs.ResultReady)

    member rs.IsEmpty() = 
        rs.Busy = false         &&
        rs.Op.IsNone            &&
        rs.Vj   = 0             &&
        rs.Vk   = 0             &&
        rs.Qj   = None          &&
        rs.Qk   = None          &&
        rs.A    = None

    static member Init name =
        {   Name = name; Busy = false; Op = None; 
            Vj = 0; Vk = 0; Qj = None; Qk = None; A = None
            ResultReady = false;
            ResultWritten = false;
            Result = 0 }

    static member ArrayInit (cfg:Config.FunctionalUnit) =
        let name pfx i = sprintf "%sUnit%d" pfx i
        Array.init cfg.rsCount (fun i -> ReservationStation.Init (name cfg.rsPrefix i))

and ReservationStationQueue = Queue<ReservationStation>
and RSMapping = ReservationStation -> ReservationStation