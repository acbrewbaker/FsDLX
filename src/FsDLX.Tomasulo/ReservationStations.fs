namespace FsDLX.Tomasulo

open System.Collections.Generic
open FsDLX.Common

type RSGroup = RSGroup of ReservationStation[] with
    member rs.Length = match rs with RSGroup rsg -> rsg.Length
    member rs.Iter f = match rs with RSGroup rsg -> rsg |> Array.iter f
    member rs.Iteri f = match rs with RSGroup rsg -> rsg |> Array.iteri f
    member rs.ForAll f = match rs with RSGroup rsg -> rsg |> Array.forall f
    member rs.TryFind f = match rs with RSGroup rsg -> rsg |> Array.tryFind f
    member rs.Fold f s = match rs with RSGroup rsg -> rsg |> Array.fold f s
    member rs.Filter f = match rs with RSGroup rsg -> rsg |> Array.filter f
    member rs.BusyOnly = rs.Filter (fun r -> r.Busy)
    
    static member Value (RSGroup rsg) = rsg
    static member Init(cfg:Config.FunctionalUnit) =
        Array.init cfg.rsCount (fun i -> ReservationStation.Init (cfg.rsPrefix + string i))
        |> RSGroup

    static member IntUnitInit() = RSGroup.Init Config.FunctionalUnit.IntegerUnit
    static member TrapUnitInit() = RSGroup.Init Config.FunctionalUnit.TrapUnit
    static member MemoryUnitInit() = RSGroup.Init Config.FunctionalUnit.MemoryUnit
    static member BranchUnitInit() = RSGroup.Init Config.FunctionalUnit.BranchUnit
    static member FloatingPointUnitInit() = RSGroup.Init Config.FunctionalUnit.FloatingPointUnit

and RSGroupRef = RSGroup ref
and RS =
    | IntegerUnit of RSGroupRef
    | TrapUnit of RSGroupRef
    | BranchUnit of RSGroupRef
    | MemoryUnit of RSGroupRef
    | FloatingPointUnit of RSGroupRef

    static member ApplyFunction (f:RSGroupRef -> 'T) = function
        | IntegerUnit rs 
        | TrapUnit rs
        | BranchUnit rs
        | MemoryUnit rs
        | FloatingPointUnit rs -> f rs

    member rs.Contents = rs |> RS.ApplyFunction (!)

    member private rs.GetMap() =
        let kvp (r:ReservationStation) = (r.Name, r)
        rs.Contents |> RSGroup.Value |> Array.map kvp |> Map.ofArray

    member rs.Item with get(r:ReservationStation) = rs.GetMap().[r.Name]
    
    member rs.Length = rs.Contents.Length

    member rs.Update() = 
        let cdb = CDB.GetInstance
        rs.Contents.Iter (fun r ->
            match r.Qj with Some Qj -> if cdb.Src = Qj then //if r.Busy && cdb.Src = Qj then   
                                            r.Qj <- None
                                            r.Vj <- cdb.Result 
                                        | _ -> ()
            
            match r.Qk with Some Qk -> if cdb.Src = Qk then //if r.Busy && cdb.Src = Qk then
                                            r.Qk <- None
                                            r.Vk <- cdb.Result
                                            //printfn "Vk ==> %A" (r.Vk) 
                                        | _ -> ()
            )
    
    member rs.Clear() = rs.Contents.Iter (fun r -> r.Clear())

    member rs.AllBusy() = rs.Contents.ForAll (fun r -> r.Busy)
    member rs.AllNotBusy() = rs.Contents.ForAll (fun r -> not(r.Busy))

    member rs.Iter = rs.Contents.Iter
    member rs.Filter = rs.Contents.Filter
    member rs.TryFind = rs.Contents.TryFind
    member rs.TryFindOperandsAvailable() = rs.TryFind (fun r -> r.OperandsAvailable())
    member rs.TryFindResultReady() = rs.TryFind (fun r -> r.ResultReady)
    member rs.TryFindEmpty() = rs.TryFind (fun r -> r.IsEmpty())
    member rs.TryFindNotBusy() = rs.TryFind (fun r -> not(r.Busy))
    
    member rs.Dump() =
        rs.Contents.Fold (fun s r -> s + "\n" + (r.Dump()))
            ("Name  Busy  Opcode   Vj  Vk  Qj  Qk  A  ResultReady  ResultWritten  Result")
    
    override rs.ToString() =
        let busyOnly = rs.Contents.BusyOnly
        if busyOnly.Length <> 0 
        then (busyOnly |> Array.map (sprintf "%O\n") |> Array.reduce (+)).Trim()
        else ""

    static member Update(rs:RS[]) = rs |> Array.iter (fun r -> r.Update())

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

    member rs.Dump() =
        sprintf "%s  %O  %O  %s  %s  %O  %O  %O  %O  %O  %s"
            rs.Name rs.Busy rs.Op
            (Convert.int2hex rs.Vj)
            (Convert.int2hex rs.Vk) 
            rs.Qj rs.Qk
            rs.A
            rs.ResultReady rs.ResultWritten
            (Convert.int2hex rs.Result)

    override rs.ToString() =
        sprintf "%s  %O  %O  %s  %s  %s  %s  %s"
            rs.Name rs.Busy (Opcode.Opt2String(rs.Op))
            (Convert.int2hex rs.Vj)
            (Convert.int2hex rs.Vk) 
            (Convert.strOption2str(rs.Qj)) (Convert.strOption2str(rs.Qk))
            (Convert.intOption2str rs.A)

    static member Init name =
        {   Name = name; Busy = false; Op = None; 
            Vj = 0; Vk = 0; Qj = None; Qk = None; A = None
            ResultReady = false;
            ResultWritten = false;
            Result = 0 }

and ReservationStationQueue = Queue<ReservationStation>

and RSId = RSId of string option

