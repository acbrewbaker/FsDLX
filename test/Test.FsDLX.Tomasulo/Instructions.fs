module Test.FsDLX.Tomasulo.Instructions

open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo


[<Test>]
let ``instruction kind`` () =
    let addiRaw = "00000000: 20010003      #    addi r1, r0, 3"
    printfn "Raw: %s" addiRaw
    let hex = splitForHex addiRaw
    printfn "Hex: %s" hex
    let i   = Convert.hex2int hex
    printfn "Int: %A" i 

[<Test>]
let ``array init`` () =
    let instructions = 
        [| "addi", {    opcode = Opcode.ofName "addi"; funCode = FunCode.NONE; 
                        rd = DstReg.R(11,15)
                        rs = S1Reg.R(6,10)
                        rt = S2Reg.NONE
                        imm = Imm.A(16,31) }
                        
           "add", {     opcode  = Opcode.ofName "add"; funCode = FunCode.NONE;
                        rd      = DstReg.R(16,20)
                        rs      = S1Reg.R(6,10)
                        rt      = S2Reg.R(11,15)
                        imm     = Imm.NONE } 
        |] |> Map.ofArray

    printfn "%A" instructions
