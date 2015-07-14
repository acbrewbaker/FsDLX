open System
open System.Linq

let lines = 
    [|
        "00000000: 20010003	#    addi r1, r0, 3"
        "00000004: 20020006	#    addi r2, r0, 6"
        "00000008: 00221820	#    add r3, r1, r2"
        "0000000c: 2004003c	#    addi r4, r0, s1"         
        "00000010: 44800003	#    trap r4, 3              ;dump string"
        "00000014: 44200001	#    trap r1, 1              ;dump register"
        "00000018: 2004004a	#    addi r4, r0, s2"         
        "0000001c: 44800003	#    trap r4, 3              ;dump string"
        "00000020: 44400001	#    trap r2, 1              ;dump register"
        "00000024: 20040050	#    addi r4, r0, s3"         
        "00000028: 44800003	#    trap r4, 3              ;dump string"
        "0000002c: 44600001	#    trap r3, 1              ;dump register"
        "00000030: 20040055	#    addi r4, r0, s4"         
        "00000034: 44800003	#    trap r4, 3              ;dump string"
        "00000038: 44000000	#    trap r0, 0"
        "0000003c: 0a5468652073756d206f662000	#\"\nThe sum of \""
        "0000004a: 20616e642000	#\" and \""
        "00000050: 2069732000	#\" is \""
        "00000055: 2e0a00	#\".\n\""
    |]

let splitForHex (s:string) = s.Split([|':';'#'|]).[1].Trim()

let hex = lines |> Array.map splitForHex

//let hex2bytes (hex:string) =
//    Enumerable.Range(0, hex.Length)
//                .Where(fun x -> x % 2 = 0)
//                .Select(fun x -> Convert.ToByte(hex.Substring(x,2), 16))
//                .ToArray()

//let bytes = hex |> Array.map hex2bytes |> Array.concat

let str = "0a5468652073756d206f662000"
let str2 = "20010003"
//let strbytes = str |> hex2bytes
//
//let a = "00000055"
//printfn "sumofaddr |> hex2bytes |> toint ===> %A" (BitConverter.ToInt32(a |> hex2bytes, 0))
//let hex2int hex = BitConverter.ToInt32(hex |> hex2bytes, 0) 

//let x = strbytes |> Seq.takeWhile (fun e -> e <> 0uy)

//strbytes |> Array.map char |> Array.fold (fun s r -> s + string r ) ("") |> printfn "String: %s"

//let f = Array.map char >> Array.fold (fun s r -> s + string r) ("")
//let b2c : byte -> char = char
let hex2byte hex = Convert.ToByte(hex, 16)
let byte2char : byte -> char = char
let byte2str : byte -> string = char >> string

let concatByte s b = s + (byte2str b)
let bytes2str = Array.fold concatByte ("")

//let f s = BitConverter.ToString(s).Split('-') |> Array.map (fun x -> Convert.ToByte(x,16)) |> 
let f (s:string) = 
    [for i in [0..2..s.Length-1] -> Convert.ToByte(s.Substring(i,2), 16)]
    |> List.map (char >> string) |> List.reduce (+)

//let g = 
//    Seq.windowed 2 >> Seq.map string >> Seq.map hex2byte >> Seq.map char >> string

//let x = str |> Seq.windowed 2
//let xx = x |> Seq.map (string << String.concat)

//printfn "str |> Seq.windowed 2 ==> %A" x
//printfn "str |> Seq.windowed 2 |> Seq.map string ==> %A" xx
//printfn "... |> Seq.map hex2byte ==> %A" (str |> Seq.windowed 2 |> Seq.map string |> Seq.map hex2byte)
//printfn "MEM TO STRRRRR  =====> %A" (strbytes |> bytes2str)
printfn "MEM TO STRRRRR #2  =====> %A" (f str)
//
//printfn "sumof ==> %A" (bytes.[hex2int a..].TakeWhile(fun x -> x <> 0uy).ToArrary() |> f)
//let b = 85
//printfn "at b ===> %A, %A, %A" (bytes.[b]) (bytes.[b..]) (bytes.[b..] |> f)
//printfn "bytes length ==> %A" (bytes.Length)
//printfn "bytes 0 ===> %A, %A, %A" (bytes.[0]) (bytes.[..3]) (BitConverter.ToInt32(bytes,0).ToString("x8"))