namespace FsDLX.Tomasulo

open System.IO
open FsDLX.Common

type Memory private (size:int) =
    static let instance size = Memory(size)
    
    let dumpBy (by:int) (mem:int[]) =
        [for i = 0 to mem.Length / (by*4) do 
            let m = mem.[i*by..i*by + by]
            let hasContent = not (Array.forall (fun x -> x = 0) m) 
            if hasContent then yield (i*by*4, m)]
        |> List.map (fun (pc, vals) ->
            sprintf "%s: %s"
                (Convert.int2nibble pc)
                (vals |> Array.fold (fun s v -> s + (Convert.int2hex v) + " ") ("")))
        |> List.fold (fun s l -> s + l + "\n") ("")
            

    let mutable M = Array.zeroCreate<int> size
    let mutable M' = Array.init<Instruction> size

    
    let checkAddr = function
        | a when a % 4 <> 0 -> failwith "Invalid PC"
        | a -> M.[a / 4] 


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

    member val Size = 0 with get, set
        

    member m.Load input = Config.Memory.outputLevel |> function
        | Regular -> loadRegular input
        | Verbose -> loadVerbose input
        | Debug -> loadDebug input


    member m.Item
        with get(address)     = M.[address]
        and set address value = M.[address] <- value

    member m.Dump(cols) = M |> dumpBy cols |> sprintf "%s"
    member m.Dump()     = m.Dump(8)

    override m.ToString() = m.Dump()
//        M
//        |> Array.map Convert.int2hex
//        |> Array.fold (fun s h -> s + h + "\n") ("")


    static member GetInstance = instance
    
    

    