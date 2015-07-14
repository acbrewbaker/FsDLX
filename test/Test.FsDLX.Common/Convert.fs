module Test.FsDLX.Common.Convert

open NUnit.Framework
open FsUnit
open FsDLX.Common

let addiRaw = "00000000: 20010003      #    addi r1, r0, 3"
let hex = splitForHex addiRaw

[<Test>]
let ``Convert.hex2int``() =
    printfn "Raw: %s" addiRaw
    printfn "Hex: %s" hex
    
    let i   = Convert.hex2int hex
    printfn "Convert.hex2int: %A" i 
    i |> should equal 536936451


[<Test>]
let ``Convert.bin2int`` () =
    printfn "Raw: %s" addiRaw
    printfn "Hex: %s" hex
    
    let i   = Convert.hex2int hex
    printfn "Convert.hex2int: %A" i 
    i |> should equal 536936451

//    let bits = Opcode.GetOpcodeBits hex
//    printfn "Opcode bits: %A" bits
//    bits |> should equal "001000"

//    let enc = Convert.bin2int bits
//    printfn "Convert.bin2int bits: %A" enc
//    enc |> should equal 8



