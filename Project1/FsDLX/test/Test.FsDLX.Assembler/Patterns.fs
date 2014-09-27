module Test.FsDLX.Patterns

open System
open System.IO
open System.Text.RegularExpressions
open FsDLX.Assembler
open NUnit.Framework
open FsUnit

open FsDLX
open Support

[<Test>]
let ``get opcode regex`` () =
    let opcodes = Opcodes()
    let itypecodes, rtypecodes, jtypecodes =
        Opcodes.ParseTypeFile itypesfile |> List.map (fun (op, rrid, enc) -> op + "|") |> List.reduce (+),
        Opcodes.ParseTypeFile rtypesfile |> List.map (fun (op, rrid, enc) -> op + "|") |> List.reduce (+),
        Opcodes.ParseTypeFile jtypesfile |> List.map (fun (op, rrid, enc) -> op + "|") |> List.reduce (+)

    printfn "%A" itypecodes
    printfn "%A" rtypecodes
    printfn "%A" jtypecodes


[<Test>]
let ``itype opcode patterns`` () =
    let it1 = "addi r1, r2, 4"
    let m = Regex(Patterns.Opcode.itype).Matches(it1)
    printfn "%A" m

[<Test>]
let ``rtype opcode patterns`` () =
    let rt1 = "add r1, r2, r3"
    let m = Regex(Patterns.Opcode.rtype).Matches(rt1)
    printfn "%A" m

[<Test>]
let ``jtype opcode patterns`` () =
    let jt1 = "jal label1"
    let m = Regex(Patterns.Opcode.jtype).Matches(jt1)
    printfn "%A" m

[<Test>]
let ``comment patterns`` () =
    let c1 = " ; encode set instructions"
    let m = Regex(Patterns.comment).Matches(c1)
    printfn "%A" m

[<Test>]
let ``register patterns`` () =
    let rp1 = "r1, r2"
    let rp2 = "f2, f3"
    let rp3 = "f3, f3, f5"
    let rp4 = "r2, r31"

    let m = Regex(Patterns.reg).Matches(rp1)
    printfn "%A" m

    let m = Regex(Patterns.freg).Matches(rp2)
    printfn "%A" m

    let m = Regex(Patterns.freg).Matches(rp3)
    printfn "%A" m

    let m = Regex(Patterns.reg).Matches(rp4)
    printfn "%A" m


[<Test>]
let ``label patterns`` () =
    let lp1 = "label:"
    let lp2 = "label3:"
    let lp3 = "label5: derp"

    let m = Regex(Patterns.label).Matches(lp1)
    printfn "%A" m

    let m = Regex(Patterns.label).Matches(lp2)
    printfn "%A" m

    let m = Regex(Patterns.label).Matches(lp3)
    printfn "%A" m


[<Test>]
let ``immediate patterns`` () =
    let ip1 = "5"
    let ip2 = "4(20)"
    let ip3 = "4(r1)"
    let ip4 = "4(label)"
    let ip5 = "4(label3)"
    let ip6 = "4(r21)"

    let m = Regex(Patterns.imm).Matches(ip1)
    printfn "ip1: %A" m

    let m = Regex(Patterns.imm).Matches(ip2)
    printfn "ip2: %A" m

    let m = Regex(Patterns.imm).Matches(ip3)
    printfn "ip3: %A" m

    let m = Regex(Patterns.imm).Matches(ip4)
    printfn "ip4: %A" m

    let m = Regex(Patterns.imm).Matches(ip5)
    printfn "ip5: %A" m

    let m = Regex(Patterns.Operands.Immediate.any).Matches(ip5)
    printfn "ip5: %A" m
    printfn "%A" ([for m' in m -> m'.Groups.["imm"]])

    let m = Regex(Patterns.Operands.Immediate.any).Matches(ip6)
    printfn "ip6: %A" m

[<Test>]
let ``match any opcode`` () =
    let pat = Patterns.Opcode.itypecg + "|" + Patterns.Opcode.rtypecg + "|" + Patterns.Opcode.jtypecg
    let str = "
        add r1, r2, r3
        addf f1, f2, f31
        seq r1, r24, r3
        jal label2
        seqi f23, f6, 8
        subi r3, r10, 4(label3)"
    
    let matches = Regex(pat).Matches(str)

    printfn "Matches:\n%A" matches

    printfn "Groups:"
    for m in matches do printfn "%A" m.Groups

    printfn "Derps:\n%A"
        [for m in matches ->
            (m.Groups.["itype"], m.Groups.["rtype"], m.Groups.["jtype"])]

[<Test>]
let ``match any operands`` () =
    let pat = Patterns.Operands.any //Operands.fff + "|" + Operands.rrr + "|" + Operands.rri  
//    let str = "
//        add r1, r2, r3
//        addf f1, f2, f31
//        seqi r1, r24, 4(label1)
//        jal label2
//        seqi f23, f6, 8"
    let str = "seqi r1, r24, 4(label2)"
    let matches = Regex(pat).Matches(str)

    printfn "Matches:\n%A" matches

    printfn "Groups:"
    for m in matches do printfn "%A" m.Groups

    printfn "IType Operands:"
    for m in matches do printfn "%A" (m.Groups.["rd"], m.Groups.["rs1"], m.Groups.["imm"])

[<Test>]
let ``match any instruction`` () =
    let pat = Patterns.instruction
    let str = "
        add r1, r2, r3
        addf f1, f2, f31
        seqi r1, r24, 4(label1)
        jal label2
        seqi f23, f6, 8"

    let matches = Regex(pat).Matches(str)
    printfn "Matches:\n%A" matches

