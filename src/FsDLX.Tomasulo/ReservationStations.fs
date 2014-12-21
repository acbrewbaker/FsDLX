﻿namespace FsDLX.Tomasulo

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

    member rs.Item
        with get(i) = rs.Contents.[i]            
        and set i value = rs.Contents.[i] <- value
    
    member rs.Length = rs.Contents.Length

    member rs.Update() =
        let cdb = CDB.GetInstance
        //printfn "Update Reservation Stations"
        //printfn "RS contents length: %d" (rs.Contents.Length)
        rs.Contents |> Array.iteri (fun i r ->
//        let update (rsGroupRef:RSGroupRef) = !rsGroupRef |> Array.iter (fun r ->
            //printfn "RS(%d):%O  r.Qj.IsSome?  %O" i r (r.Qj)
            (r.Qj, r.Qk) |> function
            | Some Qj, _ ->
                if r.Busy && cdb.Src = Qj then 
                    r.Qj <- None; r.Vj <- cdb.Result
            | _, Some Qk ->
                if r.Busy && cdb.Src = Qk then
                    r.Qk <- None; r.Vk <- cdb.Result
            | None, None -> () )
//            if r.Qj.IsSome then 
//                //printfn "Qj.IsSome\nUpdate RS(%d), cdb:\n%O" i cdb
//                if r.Busy && cdb.Src = r.Qj.Value then 
//                    r.Qj <- None; r.Vj <- cdb.Result
//            if r.Qk.IsSome then
//                //printfn "Qk.IsSome\nUpdate RS(%d), cdb:\n%O" i cdb
//                if r.Busy && cdb.Src = r.Qk.Value then
//                    r.Qk <- None; r.Vk <- cdb.Result )
        //RS.ApplyFunction rs update


    member rs.TryFindReady() = rs.Contents |> Array.tryFindIndex (fun r -> r.IsReady())
//        let tryFindReady (rsGroupRef:RSGroupRef) = !rsGroupRef |> Array.tryFindIndex (fun r -> r.IsReady())
//        RS.ApplyFunction rs tryFindReady

//        rs |> function
//        | IntegerUnit rs -> update rs
//        | TrapUnit rs -> update rs
//        | BranchUnit rs -> update rs
//        | MemoryUnit rs -> update rs
//        | FloatingPointUnit rs -> update rs
    member rs.TryFindNotBusy() = rs.Contents |> Array.tryFindIndex (fun r -> not(r.Busy))

    member rs.Filter(f:ReservationStation -> bool) = rs.Contents |> Array.filter f

    member rs.Dump() =
        rs.Contents |> Array.fold (fun s r -> s + "\n" + (r.Dump()))
            ("Name  Busy  Opcode   Vj  Vk  Qj  Qk  A  ResultReady  ResultWritten  Result")


    override rs.ToString() =
        let onlyBusy = rs.Contents |> Array.filter (fun r -> r.Busy)
        if onlyBusy.Length <> 0 
        then onlyBusy |> Array.map (sprintf "%O") |> Array.reduce (+)
        else ""

    static member Filter(rs:RS[], f) =
        rs |> Array.map (fun r -> r.Filter f) |> Array.concat

    static member Update (rs:RS[]) = rs |> Array.iter (fun r -> r.Update())

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
        rs.Busy <- false
        rs.Op <- None
        rs.Vj <- 0; rs.Vk <- 0
        rs.Qj <- None; rs.Qk <- None
        rs.A <- None
        rs.ResultReady <- false
        rs.ResultWritten <- false

    member rs.ClearIfResultWritten() = if rs.ResultWritten then rs.Clear()

    member rs.IsReady() =
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
        rs.A.IsNone

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
        sprintf "%s  %O  %O  %s  %s  %O  %O  %O"
            rs.Name rs.Busy rs.Op 
            (Convert.int2hex rs.Vj)
            (Convert.int2hex rs.Vk) 
            rs.Qj rs.Qk 
            rs.A

//    override rs.ToString() =
//        sprintf "%s  %O  %O  %s  %s  %O  %O  %s"
//            rs.Name rs.Busy rs.Op 
//            (Convert.int2hex rs.Vj)
//            (Convert.int2hex rs.Vk) 
//            rs.Qj rs.Qk 
//            (Convert.int2hex rs.Result)

//    override rs.ToString() =
//        sprintf "%s  %O  %O  %s  %s  %O  %O  %O  %O  %O  %s"
//            rs.Name rs.Busy rs.Op 
//            (Convert.int2hex rs.Vj)
//            (Convert.int2hex rs.Vk) 
//            rs.Qj rs.Qk 
//            rs.A
//            rs.ResultReady rs.ResultWritten 
//            (Convert.int2hex rs.Result)

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


    static member Clear (r:ReservationStation) = r.Clear()
    static member ClearIfResultWritten (r:ReservationStation) = r.ClearIfResultWritten()


