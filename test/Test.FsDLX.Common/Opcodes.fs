module Test.FsDLX.Common.Opcodes

open NUnit.Framework
open FsUnit
open FsDLX.Common


[<Test>]
let ``opcodes`` () =
    let addiRaw = "00000000: 20010003      #    addi r1, r0, 3"
    printfn "Raw: %s" addiRaw
    
    let hex = splitForHex addiRaw
    printfn "Hex: %s" hex
    
    let i   = Convert.hex2int hex
    printfn "Int: %A" i 

//    let bits = Opcode.GetOpcodeBits hex
//    printfn "Opcode bits: %A" bits
//    bits |> should equal "001000"
//
//    let enc = bits.Length |> function
//        | b when b = Constants.nOpcodeBits -> Convert.bin2int bits
//        | _ -> failwith "Invalid Binary Opcode Length"
//    printfn "enc: %A" enc
//
//    let op = Opcode.ofEncInt(8)
//    printfn "Opcode.ofEncInt(8): %A" op
//
//    let bin = Opcode.ofBin bits
//    printfn "Opcode.ofBin bits: %A" bin

    let op1 = Opcode.ofInstructionHex hex
    printfn "Opcode.ofInstructionHex: %A" op1



    let op = Opcode.ofName "addi"
    printfn "ToString: %O" op
    printfn "op.Name: %A" op.Name

