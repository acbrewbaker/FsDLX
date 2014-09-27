module FsDLX.Assembler

open System
open System.IO

let assemble (dlxfile:string) = 
    if Path.GetExtension(dlxfile) <> ".dlx" then
        failwith "Invalid file extension"
    else
        dlxfile |> File.ReadAllLines |> Seq.fold (fun (pc:uint32, hex) line ->
            let newhex = pc.ToString("x8") + ": "
            line |> function
            | _ -> (pc + 1u, sprintf "%s\n%s" hex newhex)
        ) (0u, "")
        |> snd
