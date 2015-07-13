module FsDLX.Tomasulo.Display

open System

let lines2string = Convert.lines2string >> Some

//module CDB =
//    let dump() =
//        let cdb = CDB.GetInstance
//        sprintf "CDB: result: %s source: %s" (Convert.int2hex cdb.Result) cdb.Src


type LogEntry = LogEntry of Cycle * Info
and Cycle = string
and Info = string

module Registers =
    let out heading = 
        Array.map (sprintf "%O") >> Array.fold (fun s r -> s + " " + r) (sprintf "%s: " heading)
    
    let r0_r7() = GPR.GetInstance.[0..7] |> out "R0-R7"
    let r8_r15() = GPR.GetInstance.[8..15] |> out "R8-R15"
    let r16_r23() = GPR.GetInstance.[16..23] |> out "R16_R23"
    let r24_r31() = GPR.GetInstance.[24..31] |> out "R24_R31"
    let allgpr() = 
        sprintf "%s\n%s\n%s\n%s" (r0_r7()) (r8_r15()) (r16_r23()) (r24_r31())

    let f0_f7() = FPR.GetInstance.[0..7] |> out "F0-R7"
    let f8_f15() = FPR.GetInstance.[8..15] |> out "F8-R15"
    let f16_f23() = FPR.GetInstance.[16..23] |> out "F16_R23"
    let f24_f31() = FPR.GetInstance.[24..31] |> out "F24_R31"
    let allfpr() = 
        sprintf "%s\n%s\n%s\n%s" (f0_f7()) (f8_f15()) (f16_f23()) (f24_f31())

module Memory =
    let dumpBy (size:int) (by:int) (mem:byte[]) =
        let mem = [|0..4..size-4|] |> Array.map (fun e -> BitConverter.ToInt32(mem, e))
        let content = 
            [for i = 0 to mem.Length / (by*4) do 
                let m = mem.[i*by..(i*by + by) - 1]
                let hasContent = not (Array.forall ((=) 0) m) 
                if hasContent then yield (i*by*4, m)]
        let content = 
            let i = content.Length
            content @ [i*by*4, (mem.[i*by..(i*by+by) - 1])]
            |> List.map (fun (pc, vals) ->
            sprintf "%s:   %s"
                (Convert.int2nibble pc)
                ((vals |> Array.fold (fun s v -> s + (Convert.int2hex v) + " ") ("")).Trim()))
        content 
        |> List.mapi (function
            | i when i = content.Length - 1 -> fun s -> s
            | _                         -> fun s -> s + "\n")
        |> List.reduce (+)
        |> sprintf "MEMORY\n%s"

    let dump cols = 
        let m = Memory.GetInstance 
        dumpBy (m.Size) cols (m.AsBytes)

    let dump8() = dump 8

module ReservationStations =
    let format8 name busy opcode vj vk qj qk a =
        System.String.Format("{0,10}{1,10}{2,10}{3,10}{4,10}{5,10}{6,10}{7,10}",
            name,busy,opcode,vj,vk,qj,qk,a)

    let format10 name busy opcode vj vk qj qk a rr rw r =
        System.String.Format("{0,10}{1,10}{2,10}{3,10}{4,10}{5,10}{6,10}{7,10}{8,10}{9,10}{10,10}",
            name,busy,opcode,vj,vk,qj,qk,a,rr,rw,r)

    let headers8() =
        format8 "Name" "Busy" "Opcode" "Vj" "Vk" "Qj" "Qk" "A"

    let headers10() =
        format10 "Name" "Busy" "Opcode" "Vj" "Vk" "Qj" "Qk" "A" "R.Ready" "R.Written" "Result"

    let dump8 (r:ReservationStation) =
        sprintf "%s" (format8 r.Name r.Busy r.Op r.Vj r.Vk r.Qj r.Qk r.A)

    let dump10 (r:ReservationStation) =
        sprintf "%s" (format10 r.Name r.Busy r.Op r.Vj r.Vk r.Qj r.Qk r.A r.ResultReady r.ResultWritten r.Result)

    module RSGroup =
        let dumpActive (rsg:RSGroup) =
            let active = rsg.BusyOnly
            if      active.Length > 0
            then    active |> Array.map dump10
            else    [|""|]

        let dump rsg =
            [| [|headers10()|]; dumpActive rsg; |] |> Array.concat |> Convert.lines2str

module FunctionalUnits =
    module XUnits =
        let dump (funit:FunctionalUnit) =
            funit.ExecutionUnits |> Array.map (sprintf "%O") |> Convert.lines2str

    module ReservationStations =
        let dump (funit:FunctionalUnit) =
            funit.ReservationStations |> ReservationStations.RSGroup.dump

    let getExecuting() =
        let fu = FunctionalUnits.GetInstance
        fu.All |> Array.map (fun funit ->
            funit.ExecutionUnits |> Array.map (fun xunit ->
                if      xunit.Busy 
                then    match xunit.Station with Some station -> Some(station.Name) | _ -> None
                else    None))
        |> Array.concat |> Array.choose id

    let dumpReservationStations =
        let fu = FunctionalUnits.GetInstance
        fu.All |> Array.map ReservationStations.dump 
        |> Convert.lines2str

    let dumpExecutionUnits() =
        let fu = FunctionalUnits.GetInstance
        fu.All |> Array.map (fun funit -> sprintf "%s\n%s" funit.Name (XUnits.dump funit))
        |> Convert.lines2str