namespace FsDLX.Tomasulo

open System
open System.Collections.Generic


type RSGroup = RSGroup of ReservationStation[] with
    //member rsg.Length = (RSGroup.Value rsg) |> Array.length
    member rsg.Iter f = (RSGroup.value rsg) |> Array.iter f
    //member rsg.Iteri f = (RSGroup.Value rsg) |> Array.iteri f
    //member rsg.Map f = (RSGroup.Value rsg) |> Array.map f
    member rsg.ForAll f = (RSGroup.value rsg) |> Array.forall f
    member rsg.TryFind f = (RSGroup.value rsg) |> Array.tryFind f
    member rsg.TryPick f = (RSGroup.value rsg) |> Array.tryPick f
    member rsg.Fold f s = (RSGroup.value rsg) |> Array.fold f s
    member rsg.Filter f = (RSGroup.value rsg) |> Array.filter f
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
        rsg |> RSGroup.value |> Array.map kvp |> Map.ofArray

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
    
    override rsg.ToString() = 
        let active = rsg.BusyOnly
        if active.Length > 0 then active |> Array.map (sprintf "%O") |> Convert.lines2str else ""

    static member value (RSGroup rsg) = rsg
    static member init (cfg:Config.FunctionalUnit) = cfg |> ReservationStation.init |> RSGroup

    static member initAll() =
        Config.FunctionalUnit.All |> Array.map ReservationStation.init |> Array.concat |> RSGroup

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

    override rs.ToString() = 
        String.Format("{0,10}{1,10}{2,10}{3,10}{4,10}{5,10}{6,10}{7,10}{8,10}{9,10}{10,10}",
            rs.Name,rs.Busy,rs.Op,rs.Vj,rs.Vk,rs.Qj,rs.Qk,rs.A,rs.ResultReady,rs.ResultWritten,rs.Result)

    static member init (name:string) =
        {   Name = name; Busy = false; Op = None; 
            Vj = 0; Vk = 0; Qj = None; Qk = None; A = None
            ResultReady = false;
            ResultWritten = false;
            Result = 0 }

    static member init (cfg:Config.FunctionalUnit) =
        let name pfx i = sprintf "%sUnit%d" pfx i
        Array.init cfg.rsCount (fun i -> ReservationStation.init (name cfg.rsPrefix i))

    static member headers() =
        String.Format("{0,10}{1,10}{2,10}{3,10}{4,10}{5,10}{6,10}{7,10}{8,10}{9,10}{10,10}",
            "Name","Busy","Opcode","Vj","Vk","Qj","Qk","A","R.Ready","R.Written","Result")


and ReservationStationQueue = Queue<ReservationStation>
and RSMapping = ReservationStation -> ReservationStation