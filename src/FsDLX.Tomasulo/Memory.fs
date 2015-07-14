namespace FsDLX.Tomasulo

open System
open System.IO
open System.Linq


type Memory private () =
    static let mutable instance = Memory()
    
    let size = Config.Memory.DefaultMemorySize
    let M = Array.zeroCreate<byte> size

    let dumpBy (by:int) =
        let mem = [|0..4..size-4|] |> Array.map (fun e -> BitConverter.ToInt32(M, e))
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

    member val Size = M.Length with get, set

    member m.Load =
        File.ReadAllLines
        >> Array.map (Input.line2ad >> fun (a,d) -> Convert.hex2int a,d)
        >> Array.iter m.Write

    member m.Item
        with get(address) = 
            let a = address in BitConverter.ToInt32(Array.rev M.[a..a+3], 0)
        and set address (v:int) = 
            let data = Convert.int2bytes v in Array.blit data 0 M address data.Length

    member m.AsBytes = M

    member m.Write(addr:int, data:byte[]) = Array.blit data 0 M addr data.Length
    member m.Write(addr, hex) = m.Write(addr, Convert.hex2bytes hex)
    member m.Write(addr, int':int) = m.Write(addr, Convert.int2hex int')

    member m.FetchString(addr) =
        M.Skip(addr).TakeWhile((<>) 0uy).ToArray() |> Convert.bytes2string

    override m.ToString() = dumpBy 8
    
    static member GetInstance = instance
    static member Reset() = instance <- Memory()
    