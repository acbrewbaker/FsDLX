namespace FsDLX.Tomasulo

open System
open System.IO
open FsDLX.Common

type Memory private () =
    static let mutable instance = Memory()

    let size = Config.Memory.DefaultMemorySize

    let dumpBy (by:int) (mem:int[]) =
        let content = 
            [for i = 0 to mem.Length / (by*4) do 
                let m = mem.[i*by..(i*by + by) - 1]
                let hasContent = not (Array.forall (fun x -> x = 0) m) 
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

    let mutable M = Array.zeroCreate<int> size
    
    let checkAddr = function
        | a when a % 4 <> 0 -> failwith "Invalid PC"
        | a -> a / 4 

    let loadRegular input = 
        input |> File.ReadAllLines
        |> Array.map (splitForHex >> Convert.hex2int)
        |> Array.iteri (fun i b -> M.[i] <- b)

    let loadVerbose = loadRegular

    let loadDebug input = 
        let lines = input |> File.ReadAllLines
        printfn "input lines"
        for l in lines do printfn "%s" l
        
        let hex = lines |> Array.map splitForHex
        printfn "hex instructions"
        for h in hex do printfn "%s" h

        let ints = hex |> Array.map Convert.hex2int
        printfn "int instructions"
        for i in ints do printfn "%d" i

        ints |> Array.iteri (fun i b -> M.[i] <- b)

    member val Size = M.Length with get, set
    
    member m.Load input = Config.Memory.outputLevel |> function
        | Regular -> loadRegular input
        | Verbose -> loadVerbose input
        | Debug -> loadDebug input

    member m.Item
        with get(address)     = M.[checkAddr address]
        and set address value = M.[checkAddr address] <- value

    member m.Dump(cols) = M |> dumpBy cols |> sprintf "%s"
    member m.Dump()     = m.Dump(8)

    override m.ToString() = m.Dump().Trim()

    static member GetInstance = instance
    static member Reset() = instance <- Memory()
    