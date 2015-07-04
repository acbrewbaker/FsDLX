namespace FsDLX.Tomasulo

open System.Collections.Generic
open FsDLX.Common

type RSGroup = RSGroup of ReservationStation[] with
    member rsg.Length = (RSGroup.Value rsg) |> Array.length
    member rsg.Iter f = (RSGroup.Value rsg) |> Array.iter f
    member rsg.Iteri f = (RSGroup.Value rsg) |> Array.iteri f
    member rsg.Map f = (RSGroup.Value rsg) |> Array.map f
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
    
    member private rsg.DumpActive() = 
        let active = rsg.BusyOnly 
        if      active.Length > 0 
        then    active |> Array.map (fun r -> r.Dump())
        else    [|""|]

    member rsg.Dump() = 
        let active = rsg.BusyOnly 
        if active.Length > 0 then 
            [|  [|ReservationStation.Headers()|]; 
                active |> Array.map (fun r -> r.Dump()) |]
            |> Array.concat |> Array.map ((+) "\n") |> Array.reduce (+)
        else    ""
    
    static member Value (RSGroup rsg) = rsg
    static member Init cfg = cfg |> ReservationStation.ArrayInit |> RSGroup

    static member InitAll =
        Config.FunctionalUnit.All |> Array.map ReservationStation.ArrayInit |> Array.concat |> RSGroup

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

    static member private Format(name,busy,opcode,vj,vk,qj,qk,a,rr,rw,r) =
        System.String.Format("{0,10}{1,10}{2,10}{3,10}{4,10}{5,10}{6,10}{7,10}{8,10}{9,10}{10,10}",
            name,busy,opcode,vj,vk,qj,qk,a,rr,rw,r)

    static member Headers() = 
        ReservationStation.Format("Name", "Busy", "Opcode", "Vj", "Vk", "Qj", "Qk", "A", "R.Ready", "R.Written", "Result")
    
    member rs.Dump() =
        ReservationStation.Format(
            rs.Name, rs.Busy, (Opcode.Opt2String(rs.Op)),
            (Convert.int2hex rs.Vj),
            (Convert.int2hex rs.Vk), 
            (Convert.strOption2str(rs.Qj)), (Convert.strOption2str(rs.Qk)),
            (Convert.intOption2str rs.A),
            rs.ResultReady,
            rs.ResultWritten,
            rs.Result)

    override rs.ToString() =
        System.String.Format("{0,10}{1,10}{2,10}{3,10}{4,10}{5,10}{6,10}{7,10}",
            rs.Name, rs.Busy, (Opcode.Opt2String(rs.Op)),
            (Convert.int2hex rs.Vj),
            (Convert.int2hex rs.Vk), 
            (Convert.strOption2str(rs.Qj)), (Convert.strOption2str(rs.Qk)),
            (Convert.intOption2str rs.A))

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