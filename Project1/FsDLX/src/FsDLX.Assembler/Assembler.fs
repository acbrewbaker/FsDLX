module FsDLX.Assembler

open System
open System.IO
open System.Text
open System.Text.RegularExpressions



let assemble (dlxfile:string) = 
    if Path.GetExtension(dlxfile) <> ".dlx" then
        failwith "Invalid file extension"
    else
        dlxfile |> File.ReadAllLines |> Seq.fold (fun (pc:uint32, hex:string list) line ->
            //let newhex = pc.ToString("x8") + ": "
            line |> function
            | Patterns.Directive pc hex result -> result //(fst input, sprintf "%s\n%s" hex (newhex + snd input))
            | _ -> (pc + 1u, hex)
        ) (0u, List.empty<string>)
        |> snd
        |> List.fold (fun r s -> r + s + "\n") ""
