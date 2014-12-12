//module FsDLX.Tomasulo.Memory
namespace FsDLX.Tomasulo

open System.IO
open Convert

type T private (size:int) =
    static let instance size = T(size)
    
    let dumpBy (by:int) (mem:int[]) =
        [for i = 0 to mem.Length / (by*4) do 
            let m = mem.[i*by..i*by + by]
            let hasContent = not (Array.forall (fun x -> x = 0) m) 
            if hasContent then yield (i*by*4, m)]
        |> List.map (fun (pc, vals) ->
            sprintf "%s: %s"
                (int2hex pc)
                (vals |> Array.fold (fun s v -> s + (int2hex v) + " ") ("")))
        |> List.fold (fun s l -> s + l + "\n") ("")
            

    let mutable memory = Array.zeroCreate<int> size

    let checkAddr = function
        | a when a % 4 <> 0 -> failwith "Invalid PC"
        | a -> memory.[a / 4] 

    member val Size = 0 with get, set
        

    member m.Load(inputFile:string) =
        let split (s:string) = s.Split([|':'; '#'|]).[1].Trim()
        inputFile |> File.ReadAllLines
        |> Array.map (split >> hex2int)
        |> Array.iteri (fun i b -> m.[i] <- b)

    member m.Item
        with get(address)     = memory.[address]
        and set address value = memory.[address] <- value

    member m.Dump(cols) = memory |> dumpBy cols |> printfn "%s"
    member m.Dump()     = m.Dump(8)

    override m.ToString() =
        memory 
        |> Array.map int2hex
        |> Array.fold (fun s h -> s + h + "\n") ("")


    static member GetInstance = instance


    