module Test.FsDLX.Tomasulo.Instructions

open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

let lines =
    [
        "00000000: 20010003      #    addi r1, r0, 3"
        "00000004: 20020006      #    addi r2, r0, 6"
        "00000008: 00221820      #    add r3, r1, r2"
        "0000000c: 44600001      #    trap r3, 1              ;dump register"
        "00000010: 44000000      #    trap r0, 0"
    ]


[<Test>]
let ``instruction init`` () =
    let hex = lines |> List.map splitForHex
    let h0 = hex.[0]
    
    let op0 = hex.[0] |> Opcode.ofInstructionHex
    let op1 = hex.[1] |> Opcode.ofInstructionHex
    let op2 = hex.[2] |> Opcode.ofInstructionHex
    let op3 = hex.[3] |> Opcode.ofInstructionHex
    let op4 = hex.[4] |> Opcode.ofInstructionHex
    
    
    printfn "Op0 Name: %A" op0.Name
    printfn "Op1 Name: %A" op1.Name
    printfn "Op2 Name: %A" op2.Name
    printfn "Op3 Name: %A" op3.Name
    printfn "Op4 Name: %A" op4.Name
    

    ()
//    printfn "%A" hex
