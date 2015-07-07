module Test.FsDLX.Tomasulo.Memory

open System
open System.IO
open System.Linq
open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

let size = 1000

[<Test>]
let ``step by step load`` () =
    let file = inputdir @@ "intUnit1.hex"
    let lines = file |> File.ReadAllLines

    let split (s:string) = s.Split([|':'; '#'|])
    let adc (s:string[]) = s.[0].Trim(), s.[1].Trim(), s.[2].Trim()

    let line2ad (s:string) = 
        let s = s.Split([|':'; '#'|]) in s.[0].Trim(), s.[1].Trim()

    let m = lines |> Array.map line2ad |> Map.ofArray

//    let addresses, data, comments =
//        lines |> Array.map split 
//        |> Array.fold (fun (a,d,c) x ->
//            (a @ [x.[0].Trim()], d @ [x.[1].Trim()], c @ [x.[2].Trim()])
//            ) (List.empty<string>, List.empty<string>, List.empty<string>)
//
//    let m = (addresses, data) ||> List.zip |> Map.ofList

    let data = lines |> Array.map splitForHex
    data 
    |> Array.map (fun e -> (e, e.Length / 2))
    |> Array.iteri (fun i e -> printfn "(%d) %A" i e)

    let addrs, data = 
        data 
        |> Array.fold (fun (addr, data) e ->
                        let b = e.Length / 2
                        (addr @ [b], data @ [e])) ([0],List.empty<string>)
    let addrs = addrs |> Array.ofList 
    let addrs = addrs.[1..] |> Array.scan (+) 0
    addrs
    |> Array.iteri (fun i e -> printfn "(%d) %A" i (Convert.int2hex e))
    data
    |> List.iteri (fun i e -> printfn "(%s) %A" (Convert.int2hex i) e)

//[<Test>]
//let ``simple load`` () =
//    let file = inputdir @@ "intUnit1.hex"
//    let memory = Memory.GetInstance
//    memory.Load file
//    memory.Dump() |> printfn "%A"

//[<Test>]
//let ``column dump`` () =
//    let file = inputdir @@ "fpUnit1.hex"
//    let memory = Memory.GetInstance
//    memory.Load file
//    memory.Dump(1) |> printfn "%A"

//
//type Mem() =
//    let mutable M = Array.zeroCreate<byte> 100
//
//    let hex2bytes (hex:string) =
//        //[|for i in [hex.Length-2..-2..0] -> Convert.ToByte(hex.Substring(i,2), 16)|]
//        [|for i in [0..2..hex.Length-2] -> Convert.ToByte(hex.Substring(i,2), 16)|]
//
//    member m.Item
//        with get(address) = let a = address in BitConverter.ToInt32(M,a)
//        and set address (v:int) =
//            Array.blit (BitConverter.GetBytes v) 0 M address 4
//
//    member m.AsBytes = M
//
//    member m.Write(addr:int, data:byte[]) = Array.blit data 0 M addr data.Length
//
//    member m.Write(addr, hex:string) = m.Write(addr, hex2bytes hex)
//
//    member m.Dump() =
//        for i in [0..4..M.Length-4] do printfn "%A" (BitConverter.ToString(M,i,4).Split('-') |> Array.rev |> Array.reduce (+))
//
//    member m.Dump2() =
//        for i in [0..4..M.Length-4] do printfn "%A" (M.[i..i+3] |> Convert.bytes2hex)
//
//[<Test>]
//let ``load string`` () =
//    let file = inputdir @@ "intUnit1.hex"
//    let lines = file |> File.ReadAllLines
//    
//    let split (s:string) = s.Split([|':'; '#'|])
//    let adc (s:string[]) = s.[0].Trim(), s.[1].Trim(), s.[2].Trim()
//
//    let line2ad (s:string) = 
//        let s = s.Split([|':'; '#'|]) in s.[0].Trim(), s.[1].Trim()
//
//    let ad = lines |> Array.map line2ad
//
//    let ad' = ad |> Array.map (fun (a,d) -> (Convert.hex2int a, d))
//
//    //printfn "%A" ad'
//
//    let memory = Mem()
//    ad' |> Array.iter memory.Write
//
//    let addr = "0000003c" |> Convert.hex2int
//    printfn "addr ==> %A" addr
//
//    let x = memory.AsBytes.Skip(addr).TakeWhile((<>) 0uy).ToArray()
//    printfn "x ==> %A" (x |> Convert.bytes2string)
//    //memory.Dump2()
//
////    let m = lines |> Array.map line2ad |> Map.ofArray
////    
////    let strdat = m.["0000003c"]
////    printfn "%A" (strdat |> Convert.hex2string)
//    