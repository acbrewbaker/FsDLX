namespace FsDLX.Tomasulo

open System
open System.IO
open System.Linq
open FsDLX.Common

type Memory private () =
    static let mutable instance = Memory()
    
    let size = Config.Memory.DefaultMemorySize
    
    let M = Array.zeroCreate<byte> size

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

    static member GetInstance = instance
    static member Reset() = instance <- Memory()
    