module FsDLX.Tomasulo.Display

open System
open FsDLX.Common

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