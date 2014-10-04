module FsDLX.Assembler

open System
open System.IO
open System.Text
open System.Text.RegularExpressions



let assemble (dlxfile:string) = 
    if Path.GetExtension(dlxfile) <> ".dlx" then
        failwith "Invalid file extension"
    else
        let symbols, pc, hex =
            dlxfile |> File.ReadAllLines |> Seq.fold (fun (symbols:Map<string, string>, pc:uint32, hex:string list) line ->
                //let newhex = pc.ToString("x8") + ": "
                line |> function
                | Patterns.Label symbols pc hex result -> result
                | Patterns.Directive pc hex (newpc, newhex) ->
                    (symbols, newpc, newhex) //(fst input, sprintf "%s\n%s" hex (newhex + snd input))
    //            | Patterns.Instruction pc hex result -> 
                | _ -> (symbols, pc + 1u, hex)
            ) (Map.empty<string, string>, 0u, List.empty<string>)
        
        symbols,
        pc, 
        hex |> List.fold (fun r s -> r + s + "\n") ""
