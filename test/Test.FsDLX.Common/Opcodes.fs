module Test.FsDLX.Common.Opcodes

open NUnit.Framework
open FsUnit
open FsDLX.Common

let lines =
    [
        "00000000: 20010003      #    addi r1, r0, 3"
        "00000004: 20020006      #    addi r2, r0, 6"
        "00000008: 00221820      #    add r3, r1, r2"
        "0000000c: 44600001      #    trap r3, 1              ;dump register"
        "00000010: 44000000      #    trap r0, 0"
    ]

[<Test>]
let ``opcodes`` () =
    let addiRaw = "00000000: 20010003      #    addi r1, r0, 3"
    printfn "Raw: %s" addiRaw
    
    let hex = splitForHex addiRaw
    printfn "Hex: %s" hex
    
    let i   = Convert.hex2int hex
    printfn "Int: %A" i 

    let op1 = Opcode.ofInstructionHex hex
    printfn "Opcode.ofInstructionHex: %A" op1



    let op = Opcode.ofName "addi"
    printfn "ToString: %O" op
    printfn "op.Name: %A" op.Name


[<Test>]
let ``opcode of instruction hex`` () =
    let hex = lines |> List.map splitForHex
    let h0 = hex.[0]
    let h1 = hex.[1]
    let h2 = hex.[2]
    let h3 = hex.[3]
    let h4 = hex.[4]

    let isRType (hex:string) =
        Convert.hex2bits2int hex 0 Constants.nOpcodeBits |> function
            | rru when rru = 0 -> true, rru
            | rru when rru = 1 -> true, rru 
            | _ -> false, 2

    let getOpcodeBits (hex:string) =
        let bin = Convert.hex2bin hex
        let isRType, rru = isRType hex
        if      not(isRType)
        then    bin.[0..Constants.nOpcodeBits - 1], rru
        else    bin.[26..31], rru

    
    let bitsAndrru = hex |> List.map getOpcodeBits
    let encs = bitsAndrru |> List.map (fun (bits,rru) -> bits |> Convert.bin2int)

    bitsAndrru |> List.iteri (fun i (bits,rru) -> printfn "(h%d) bits, rru ==> %A, %A" i bits rru)
    encs |> List.iteri (fun i enc -> printfn "(h%d) enc ==> %A" i enc)

    let rruAndenc = (bitsAndrru, encs) ||> List.map2 (fun (bits,rru) enc -> (rru, enc))
    rruAndenc |> List.iteri (fun i (rru,enc) -> printfn "(h%d) rru, enc ==> %A, %A" i rru enc)

    let rruEnc0 = rruAndenc.[0]
    let rruEnc1 = rruAndenc.[1]
    let rruEnc2 = rruAndenc.[2]
    let rruEnc3 = rruAndenc.[3]
    let rruEnc4 = rruAndenc.[4]

    let lookup(rru, enc) = 
        if rru <> 2 then
            (OpcodeUtil.Lookup.rtypeByEnc.[rru].[enc])
        elif OpcodeUtil.Lookup.itypeByEnc.ContainsKey(enc) then
            (OpcodeUtil.Lookup.itypeByEnc.[enc])
        elif OpcodeUtil.Lookup.jtypeByEnc.ContainsKey(enc) then
            (OpcodeUtil.Lookup.jtypeByEnc.[enc])
        else
            failwith "opcode lookup failure"

    let lookups = rruAndenc |> List.map lookup
    lookups |> List.iter (printfn "%A")

    let opcodesOfName = lookups |> List.map (Opcode.ofName)
    opcodesOfName |> List.iter (printfn "%A")
    //printfn "lookup: %A" (lookup rru enc)