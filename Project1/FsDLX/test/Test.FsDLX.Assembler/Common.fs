[<AutoOpen>]
module Test.FsDLX.Assembler.Common

open System
open System.IO
open FsDLX.Assembler
open NCrunch.Framework
open NUnit.Framework
open FsUnit

let srcdir = 
    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
        Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
    else 
        Environment.CurrentDirectory

let inputdir = srcdir @@ @"../../../Inputs"

let testdir    = srcdir @@ "../../../Tests"

let printi op rs1 rd imm = printfn "op,rs1,rd,imm ==> %s,%s,%s,%s" op rs1 rd imm


let tobin (s:string) = Convert.ToString(s |> int, 2)

let carray2string (c:char[]) = c |> Array.fold (fun s c -> string c + s) ("")

let revstr (s:string) = s.ToCharArray() |> Array.rev |> Array.fold(fun s c -> s + string c) ("")

let str2bytes (s:string) =
    [   s.[0..7]
        s.[8..15]
        s.[16..23]
        s.[24..31] ]

let hex = ['0'..'9'] @ ['a'..'f']
let bin = 
    [
        "0000"
        "0001"
        "0010"
        "0011"
        "0100"
        "0101"
        "0110"
        "0111"
        "1000"
        "1001"
        "1010"
        "1011"
        "1100"
        "1101"
        "1110"
        "1111"
    ]

let B2Hmap = 
    let hex = ['0'..'9'] @ ['a'..'f']
    let bin = [for i in 0..15 -> Convert.ToString(i, 2).PadLeft(4, '0')]
    (bin, hex) ||> List.zip 
    |> Map.ofList


let nibble2hex (s:string) = string B2Hmap.[s]    

let byte2hex (s:string) =
    let ln',hn' =
        nibble2hex (s.Substring(0,4)),
        nibble2hex (s.Substring(4,4))
    //printfn "ln', hn' = (%s, %s)" ln' hn'
    (string ln' + string hn')

let bin2hex (s:string) =
    
//    let s = s |> revstr
    //let b = s.ToCharArray()
    
    
    
    let byte0 = s.Substring(0, 8)
    let byte1 = s.Substring(8, 8)
    let byte2 = s.Substring(15, 8)
    let byte3 = s.Substring(24, 8)
    printfn "byte0 = %A" byte0
    printfn "byte1 = %A" byte1
    printfn "byte2 = %A" byte2
    printfn "byte3 = %A" byte3
    let b0 = byte2hex byte0
    let b1 = byte2hex byte1
    let b2 = byte2hex byte2
    let b3 = byte2hex byte3
    
    printfn "b0 = %s" b0
    printfn "b1 = %s" b1
    printfn "b2 = %s" b2
    printfn "b3 = %s" b3
    printfn "all = %s" (b0 + b1 + b2 + b3)
    (b0 + b1 + b2 + b3)
//    let b0' = revstr b0
//    let b1' = revstr b1
//    let b2' = revstr b2
//    let b3' = revstr b3
//    printfn "b0' = %s" b0'
//    printfn "b1' = %s" b1'
//    printfn "b2' = %s" b2'
//    printfn "b3' = %s" b3'
//    printfn "all = %s" (b0' + b1' + b2' + b3')
//    
//
//    let b0'' = Convert.ToUInt32(b0', 16)
//    let b1'' = Convert.ToUInt32(b1', 16)
//    let b2'' = Convert.ToUInt32(b2', 16)
//    let b3'' = Convert.ToUInt32(b3', 16)
//    printfn "b0'' = %A" b0''
//    printfn "b1'' = %A" b1''
//    printfn "b2'' = %A" b2''
//    printfn "b3'' = %A" b3''
//
//    let b0''' = b0''.ToString("x8")
//    let b1''' = b1''.ToString("x8")
//    let b2''' = b2''.ToString("x8")
//    let b3''' = b3''.ToString("x8")
//    printfn "b0''' = %A" b0'''
//    printfn "b1''' = %A" b1'''
//    printfn "b2''' = %A" b2'''
//    printfn "b3''' = %A" b3'''

//    sprintf "%s"
//        (byte0 |> Array.fold (fun s c -> string c + s) (""))


//let [<Test>] `` asm test `` () =
//    let op = "8"
//    let rs1 = "1"
//    let rd = "2"
//    let imm = "3"
//    
//    printi op rs1 rd imm
//    let op' = (op |> tobin).PadLeft(6,'0')
//    let rs1' = (rs1 |> tobin).PadLeft(5,'0')
//    let rd' = (rd |> tobin).PadLeft(5,'0')
//    let imm' = (imm |> tobin).PadLeft(16,'0')
//    printfn "op, rs1, rd, imm = %s, %s, %s, %s" op rs1 rd imm
//    printfn "op', rs1', rd', imm' = %s, %s, %s, %s" op' rs1' rd' imm'
//    let s = op' + rs1' + rd' + imm'
//    
//    let itypeIn = op, rd, rs1, imm
//    
//    printfn "S: %A" s
//    let b0 = s.Substring(0,8)
//    let b1 = s.Substring(8,8)
//    let b2 = s.Substring(16,8)
//    let b3 = s.Substring(24,8)
//    printfn "b0 ==> %A" b0
//    printfn "b1 ==> %A" b1
//    printfn "b2 ==> %A" b2
//    printfn "b3 ==> %A" b3
//    let b0' = byte2hex b0
//    let b1' = byte2hex b1
//    let b2' = byte2hex b2
//    let b3' = byte2hex b3
//    printfn "b0' ==> %A" b0'
//    printfn "b1' ==> %A" b1'
//    printfn "b2' ==> %A" b2'
//    printfn "b3' ==> %A" b3'
//    
//    let actual = bin2hex s
//    printi op' rs1' rd' imm'
//    let expected = "20410003"
//    expected |> should equal actual