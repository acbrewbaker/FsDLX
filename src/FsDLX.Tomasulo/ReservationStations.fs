namespace FsDLX.Tomasulo

open FsDLX.Common


type RSGroup = ReservationStation[]
and RSGroupRef = RSGroup ref

and RS =
    | IntegerUnit of RSGroupRef
    | TrapUnit of RSGroupRef
    | BranchUnit of RSGroupRef
    | MemoryUnit of RSGroupRef
    | FloatingPointUnit of RSGroupRef

    static member ApplyFunction (f:RSGroupRef -> 'T) = function
        | IntegerUnit rs -> f rs
        | TrapUnit rs -> f rs
        | BranchUnit rs -> f rs
        | MemoryUnit rs -> f rs
        | FloatingPointUnit rs -> f rs

    member rs.Contents = rs |> RS.ApplyFunction (!)

    member private rs.GetMap() =
        let kvp (r:ReservationStation) = (r.Name, r)
        rs.Contents |> Array.map kvp |> Map.ofArray

    member private rs.Item
        with get(i) = rs.Contents.[i]            
        and set i value = rs.Contents.[i] <- value

    member rs.Item with get(r:ReservationStation) = rs.GetMap().[r.Name]
    
    member rs.Length = rs.Contents.Length

    member rs.Update() = 
        let cdb = CDB.GetInstance
        rs.Contents |> Array.iteri (fun i r ->
            (r.Qj, r.Qk) |> function
            | Some Qj, _ ->
                if r.Busy && cdb.Src = Qj then 
                    r.Qj <- None; r.Vj <- cdb.Result
            | _, Some Qk ->
                if r.Busy && cdb.Src = Qk then
                    r.Qk <- None; r.Vk <- cdb.Result
            | None, None -> () )

    member rs.Clear() = rs.Contents |> Array.iter (fun r -> r.Clear())

    member rs.AllBusy() = rs.Contents |> ReservationStation.AllBusy
    member rs.AllNotBusy() = rs.Contents |> ReservationStation.AllNotBusy

    member rs.TryFind f = rs.Contents |> Array.tryFind f

    member rs.TryFindOperandsAvailable() = rs.Contents |> Array.tryFind (fun r -> r.OperandsAvailable())
    member rs.TryFindResultReady() = rs.Contents |> Array.tryFind (fun r -> r.ResultReady)
    member rs.TryFindEmpty() = rs.Contents |> Array.tryFind (fun r -> r.IsEmpty())

    member rs.TryFindNotBusy() = rs.Contents |> Array.tryFind (fun r -> not(r.Busy))

    member rs.Filter(f:ReservationStation -> bool) = rs.Contents |> Array.filter f

    member rs.Dump() =
        rs.Contents |> Array.fold (fun s r -> s + "\n" + (r.Dump()))
            ("Name  Busy  Opcode   Vj  Vk  Qj  Qk  A  ResultReady  ResultWritten  Result")


    override rs.ToString() =
        //rs.Contents |> Array.map (sprintf "%O\n") |> Array.reduce (+)
        let onlyBusy = rs.Contents |> Array.filter (fun r -> r.Busy)
        if onlyBusy.Length <> 0 
        then (onlyBusy |> Array.map (sprintf "%O\n") |> Array.reduce (+)).Trim()
        else ""



    static member Filter(rs:RS[], f) =
        rs |> Array.map (fun r -> r.Filter f) |> Array.concat

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
        if rs.Busy then
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

    static member ArrayInit(n, namePrefix) =
        Array.init n (fun i -> ReservationStation.Init (namePrefix + string i))

    static member ArrayInit(cfg:Config.FunctionalUnit) =
        Array.init cfg.rsCount (fun i -> ReservationStation.Init (cfg.rsPrefix + string i))

    static member IntUnitInit() = ReservationStation.ArrayInit Config.FunctionalUnit.IntegerUnit
    static member TrapUnitInit() = ReservationStation.ArrayInit Config.FunctionalUnit.TrapUnit

    static member Clear (r:ReservationStation) = r.Clear()

    static member AllBusy (RS:RSGroup) = RS |> Array.forall (fun r -> r.Busy)
    static member AllNotBusy (RS:RSGroup) = RS |> Array.forall (fun r -> not(r.Busy))

and RSId = RSId of string option


