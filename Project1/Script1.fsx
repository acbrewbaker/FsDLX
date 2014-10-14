open System
open System.IO
open System.Text
open System.Text.RegularExpressions

let addi = "8"
let rs1 = "1"
let rd = "2"
let imm = "3"

Convert.ToUInt32(addi).ToString("x8")

let str2bin (s:string) (p:int) = Convert.ToString(Int32.Parse(s) |> int, 2).PadLeft(p, '0')
let revstr (s:string) = s.ToCharArray() |> Array.rev |> Array.fold(fun s c -> s + string c) ("")

let str2hex (str:string) =
    (Encoding.Default.GetBytes(str) |> BitConverter.ToString).Replace("-","").ToLower() + "00"

let b = [for i in 0..31 do yield 2.0**(float i)] |> Seq.map int64 |> Array.ofSeq |> Array.rev
let bin = @"00000100101000000001000000001000"

printfn "BIN BYTES: %A" (Encoding.Default.GetBytes(bin) |> BitConverter.ToString)
let bin2hex (bin:string) = 
    ((b, bin.ToCharArray()) ||> Seq.map2 (fun x y -> x * (int64 y)) |> Seq.reduce (+)).ToString("x8")

let rs1' = (str2bin rs1 5) //|> revstr
let rd' = (str2bin rd 5) //|> revstr
let addi' = (str2bin addi 6) //|> revstr
let imm' = (str2bin imm 16) //|> revstr

let f op rs1 rd imm = sprintf "%s%s%s%s" op rs1 rd imm
let s = f addi' rs1' rd' imm'
let bin'  = ("001000" + "00001" + "00010" + "0000000000000011")
printfn "%A" ((bin'.ToCharArray() |> Array.mapi (fun i c -> 2.0**(float i) * (float c)) |> Seq.reduce (+) |> uint32).ToString("x8"))
let bin'' = ("000100" + "10000" + "01000" + "1100000000000000")
printfn "%A" ((bin''.ToCharArray() |> Array.mapi (fun i c -> 2.0**(float i) * (float c)) |> Seq.reduce (+) |> uint32).ToString("x8"))
printfn "BIN' BIN'' SIZE ================ %A" (bin'.Length, bin''.Length)
printfn "BIN 2 HIZZE X ===> %A" (Convert.ToUInt64(bin',2).ToString("x8"))
printfn "BIN 2 HEX =====> %A" (bin2hex bin')

let expected = 20410003

let e = "0010 00|00 010|0 0001 |0000 0000 0000 0011"
let e' = "000100 00001"

let rtypePattern = @"(?<rru>[01]{6})(?<rs1>[01]{5})(?<rs2>[01]{5})(?<rd>[01]{5})[01]{5,6}(?<func>[01]{5})"


let cvtf2dHex = "x04a01008"




//printfn "binary: %A" (BitConverter.GetBytes(s))
printfn "len: %A" (s.Length)
//printfn "ToUint: %A" (Convert.ToString(s |> int, 2))

let s' = s.ToCharArray() |> Array.rev |> Array.fold(fun s c -> s + string c) ("")
printfn "dasdf: %A" (Convert.ToUInt32(s, 2).ToString("x8"))
printfn "derps: %A" (Convert.ToUInt32(s', 2).ToString("x8"))

let bytes2hex (b:byte[]) =
    (b |> Array.rev |> BitConverter.ToString)
        .Replace("-","").ToLower()

UInt32.Parse(addi) |> BitConverter.GetBytes |> bytes2hex |> printfn "%A"