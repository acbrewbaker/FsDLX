module Test.FsDLX.Assembler.Patterns

open System
open System.IO
open System.Text.RegularExpressions
open FsDLX.Assembler
open NUnit.Framework
open FsUnit

open Support

[<Test>]
let ``get opcode regex`` () =
    let opcodes = OpcodeInfo()
    let itypecodes, rtypecodes, jtypecodes =
        Support.parseTypeFile itypesfile |> List.map (fun (op, rrid, enc) -> op + "|") |> List.reduce (+),
        Support.parseTypeFile rtypesfile |> List.map (fun (op, rrid, enc) -> op + "|") |> List.reduce (+),
        Support.parseTypeFile jtypesfile |> List.map (fun (op, rrid, enc) -> op + "|") |> List.reduce (+)

    printfn "%A" itypecodes
    printfn "%A" rtypecodes
    printfn "%A" jtypecodes


//[<Test>]
//let ``match itype`` () = 
//    let str1 = "addi r1, r21, 4(r5)"
//    let str2 = "seqi r4, r5, 4(label3)"
//
//    let matchIType input =
//        let opcode, operands = [
//            for m in Regex(Patterns.Instruction.itype).Matches(input) -> 
//                m.Groups.["itype"].Value, 
//                (m.Groups.["rd"].Value,
//                 m.Groups.["rs1"].Value,
//                 m.Groups.["imm"].Value)] |> List.unzip
//        (opcode.Head, operands, opcode.Length <> 0)
//
////    let r = Patterns.Instruction.matchIType str1 |||> Conversions.Instruction.itype
////    printfn "Result: %A" r
//
//    let opcode, operands, opvalid = matchIType str1 //Patterns.Instruction.matchIType str1
//    
//    printfn "Opcode:\t%A" opcode
//    printfn "Operands:\t%A" operands
//    printfn "Operands Count:\t%A" (operands.Head)
//    printfn "Op Valid?:\t%A" opvalid
//
//
//[<Test>]
//let ``MatchFunction test`` () =
//    let lblpat = @"((?<label>\w+):)?"
//    let lblregex = Regex(lblpat)
//    let itpar = Patterns.Instruction.itype
//    
//    let regex = Regex(lblpat + itpar)
//    
//    let str = "addi r4, r5, 100"
//    let matchFunction : MatchFunction =
//        fun (regex:Regex) (input:string) ->
//            [for m in regex.Matches(input) -> m.Groups].Head
//
//    let groups regex input (f:MatchFunction) = f regex input
//    
//    printfn "MatchFunction ==> %A" (groups regex str matchFunction)   
//
//[<Test>]
//let ``Immediate test`` () =
//    let regex = Regex(Patterns.Instruction.operands)
//    
//    let str1 = "addi r4, r5, 100"
//    let str2 = "addi r5, r21, label8"
//    let str3 = "seqi r7, r1, 4(label9)"
//    let str4 = "subi r8, r10, 8(r4)"
//    let str5 = "subi r21, r22, x"
//    let str6 = "add r1, r2, r3"
//
//    let strs = [str1;str2;str3;str4;str5]
//
//    let tryMatch str =
//        printfn "Operands ==> %A" (regex.Match(str).Groups |> Immediate.Init)   
//
//    strs |> List.iter (fun s -> tryMatch s)
//
//
//[<Test>]
//let ``Operands test - itype`` () =
//    let regex = Regex(Patterns.Instruction.operands)
//    
//    let str1 = "addi r4, r5, 100"
//    let str2 = "addi r5, r21, label8"
//    let str3 = "seqi r7, r1, 4(label9)"
//    let str4 = "subi r8, r10, 8(r4)"
//
//    let strs = [str1;str2;str3;str4]
//
//    let tryMatch str =
//        printfn "Operands ==> %A" (regex.Match(str).Groups |> Operands.Init)   
//
//    strs |> List.iter (fun s -> tryMatch s)
//
//[<Test>]
//let ``Operands test - rtype`` () =
//    let regex = Regex(Patterns.Instruction.operands)
//    
//    let str1 = "add r4, r5, r10"
//    let str2 = "addf f5, f21, f6"
//    let str3 = "subd f7, f1, f3"
//    let str4 = "divf f8, f10, f9"
//
//    let strs = [str1;str2;str3;str4]
//
//    let tryMatch str =
//        printfn "Operands ==> %A" (regex.Match(str).Groups |> Operands.Init)   
//
//    strs |> List.iter (fun s -> tryMatch s)
//
//[<Test>]
//let ``Operands test - jtype`` () =
//    let regex = Regex(Patterns.Instruction.Operands.jtype)
//    
//    let str1 = "j r4"
//    let str2 = "jal 4(label9)"
//    let str3 = "j 1000"
//    let str4 = "j 8(r5)"
//    let str5 = "jal label9"
//
//    let strs = [str1;str2;str3;str4;str5]
//
//    let tryMatch str =
//        printfn "Operands ==> %A" (regex.Match(str).Groups)   
//
//    strs |> List.iter (fun s -> tryMatch s)
//
//[<Test>]
//let ``Opcode test`` () =
//    let lblpat = @"((?<label>\w+):)?"
//    let lblregex = Regex(lblpat)
//    let itpar = Patterns.Instruction.itype
//    
//    let regex = Regex(lblpat + itpar)
//    
//    let str = "addi r4, r5, 100"
//    
//    let matchFunction : MatchFunction =
//        fun (regex:Regex) (input:string) ->
//            [for m in regex.Matches(input) -> m.Groups].Head
//
//    printfn "Opcode ==> %A" (Opcode.Init (matchFunction regex str))
//
//[<Test>]
//let ``Instruction test`` () =
//    let lblpat = @"((?<label>\w+):)?"
//    let lblregex = Regex(lblpat)
//    let itpar = Patterns.Instruction.itype
//    
//    let regex = Regex(lblpat + itpar)
//    
//    let str = "addi r4, r5, 100"
//    
//    let matchFunction : MatchFunction =
//        fun (regex:Regex) (input:string) ->
//            [for m in regex.Matches(input) -> m.Groups].Head
//
//    printfn "Instruction ==> %A" (Instruction.Init (matchFunction regex str))
//
//
//module Pat =
//    let (|Label|_|) input =
//        let matches = Regex(@"(?<=(\w+):.*)(\w+)").Matches(input)
//        printfn "Label Matches: %A" matches
//        (matches, matches.Count > 0) |> function
//        | matches, true -> Some [for m in matches -> m.Groups.[1].Value].Head
//        | _ -> None
//        
//[<Test>]
//let ``match new label`` () =
//    let dlxpath = inputdir @@ "setImmed.dlx"
//    let hexpath = inputdir @@ "setImmed.hex"
//    
//    let dlxlines = dlxpath |> getLines
//    let hexlines = hexpath |> getLines
//
//    let dlx = dlxlines.[0]
//    printfn "Trying to match new label of: %s" dlx
//
//    let lblpat = @"(?<newlabel>[a-zA-z]+\d+:)?"
//    let regex = Regex(lblpat)
//    
//    getDetailedMatchInfo regex dlx
//
//
//[<Test>]
//let ``advanced immediate match`` () =
//    let str1 = "addi r6, r3, label7"
//    let str2 = "addi r2, r10, 4(label5)"
//    let str3 = "addi r2, r7, 4(r8)"
//    let str4 = "addi r6, r9, r21"
//    let str5 = "addi r8, r22, 1000"

    //let regex = Regex(Patterns.Instruction.Operands.Immediate.any)

//    let matches1 = regex.Matches(str1)
//    let matches2 = regex.Matches(str2)
//    printfn "Matches1: %A" matches1
//    printfn "Matches2: %A" matches2
//
//    let groups1 = [for m in matches1 -> m.Groups]
//    let groups2 = [for m in matches2 -> m.Groups]
//    printfn "Groups1:  %A" groups1
//    printfn "Groups2:  %A" groups2
//
//    let disp (groups:GroupCollection list) =
//        printfn "Groups.imm:  %A" ([for g in groups -> g.["imm"]])
//        printfn "Groups.label:  %A" ([for g in groups -> g.["label"]])
//        printfn "Groups.register:  %A" ([for g in groups -> g.["register"]])
//        printfn "Groups.val:  %A" ([for g in groups -> g.["val"]])
//        printfn "Groups.baseplusoffset:  %A" ([for g in groups -> g.["baseplusoffset"]])
//
//    printfn "\nGroups 1"
//    disp groups1
//
//    printfn "\nGroups 2"
//    disp groups2
//
//    let match1 = regex.Match(str1)
//    printfn "\nMatch1: %A" match1

//    let tryEachImmMatch str =
//        let immLabelMatch = Regex(Patterns.Instruction.Operands.Immediate.immLabel).Match(str)
//        let immRegMatch = Regex(Patterns.Instruction.Operands.Immediate.immReg).Match(str)
//        let immValMatch = Regex(Patterns.Instruction.Operands.Immediate.immVal).Match(str)
//        let immBasePlusOffsetMatch = Regex(Patterns.Instruction.Operands.Immediate.immBasePlusOffset).Match(str)
//    
//        printfn "=================== %A ======================" str
//        printfn "label matches: %A" immLabelMatch.Groups.["label"]
//        printfn "reg matches: %A" immRegMatch.Groups.["register"]
//        printfn "val matches: %A" immValMatch.Groups.["val"]
//        printfn "base plus offset matches: %A + %A" 
//            immBasePlusOffsetMatch.Groups.["base"]
//            immBasePlusOffsetMatch.Groups.["offset"]
//        printfn "=================================================="
//
//
//    tryEachImmMatch str1
//    tryEachImmMatch str2
//    tryEachImmMatch str3
//    tryEachImmMatch str4
//    tryEachImmMatch str5
//
//    printfn "\n============MATCH ANY============"
//    let matchAny str =
//        let regex = Regex(Patterns.Instruction.Operands.any)
//        let matches = regex.Matches(str)
//        let groups = [for m in matches -> m.Groups]
//        let operands = [for m in matches -> m.Groups.["operands"]]
//        printfn "\n===================== %A ======================" str
//        printfn "Matches: %A" matches
//        printfn "Groups: %A" groups
//        printfn "Operands: %A" operands
//        for g in operands do printfn "%A" g.Captures
//
//    matchAny str1
//    matchAny str2
//    matchAny str3
//    matchAny str4
//    matchAny str5

//
//[<Test>]
//let ``itype conversion`` () =
//    let dlxpath = inputdir @@ "setImmed.dlx"
//    let hexpath = inputdir @@ "setImmed.hex"
//    
//    let dlxlines = dlxpath |> getLines
//    let hexlines = hexpath |> getLines
//
//    let dlxtext = dlxpath |> getText
//    let hextext = hexpath |> getText
//    
//    let expected = hextext.Replace("\r", "")
//    
//    let opcodes = OpcodeInfo()
//    
//    let lblpat = @"(?<newlabel>[a-zA-z]+\d+:)?"
//    let lblregex = Regex(lblpat)
//    let itpat = Patterns.Instruction.itype
//    
//    let regex = Regex(lblpat + itpat)
//    
//    let p x = printfn "%A" x
//
//
//    let instructions = getInstructions dlxpath regex
//    instructions |> showBinaryInstructions
//    
//    let asmInput = getAsmInputs dlxpath regex
//    
//    let st = SymbolTable()
////    updateSymbolTable st asmInput
//    
//    //let asmInput = updateInlineLabels st asmInput
//
//    let result = testAssemble st regex dlxlines
//
//    let _, _, hex = result
//    printfn "***************  Final Hex Output  **********************"
//    for l in hex do printfn "%A" l
//
//    let actual = concatLines hex
//    printContent dlxlines expected actual
//
//    actual |> should equal expected

//[<Test>]
//let ``rtype match`` () =
//    let dlx = (inputdir @@ "intArith.dlx") |> getLines
//    let str1 = dlx.[0]
//    let str2 = dlx.[1]
//    let str3 = dlx.[2]
//    let strs = [str1; str2; str3]
//
//    printfn "=== Strings ==="
//    for s in strs do printfn "%s" s
//
//    let regex = Regex(Patterns.Instruction.rtype)
//    
//    
//    let tryMatch str =
//        printfn "\ntry to match: %s" str
//        let matches = regex.Matches(str)
//        printfn "Matches: %A" matches
//        printfn "Groups: %A" [for m in matches -> m.Groups].Head
//
//    strs |> List.iter (fun s -> tryMatch s)
//
//[<Test>]
//let ``rtype conversion`` () =
//    let dlxpath = inputdir @@ "intArith.dlx"
//    let hexpath = inputdir @@ "intArith.hex"
//    
//    let dlxlines = dlxpath |> getLines
//    let hexlines = hexpath |> getLines
//    
//    let dlxtext = dlxpath |> getText
//    let hextext = hexpath |> getText
//    //titledDisplay "DLX" dlx
//    printfn "============= DLX =============="
//    //for l in dlx do printfn "%s" l
//    printfn "%s" dlxtext
//
//    let expected = (inputdir @@ "intArith.hex") |> getText
//    titledDisplay "HEX (Expected)" expected
//    
//    let opcodes = OpcodeInfo()
//    
//    let lblpat = @"(?<newlabel>[a-zA-z]+\d+:)?.*"
//    let lblregex = Regex(lblpat)
//    let rtpat = Patterns.Instruction.rtype
//    
//    let regex = Regex(rtpat)
//
//    let instructions = getInstructions dlxpath regex
//    
//    instructions |> showBinaryInstructions
//
//    let asmInput = getAsmInputs dlxpath regex
//    
//
//    let st = SymbolTable()
//    updateSymbolTable st asmInput
//        
//    let asmInput = updateInlineLabels st asmInput
//        
//    let result = testAssemble st regex dlxlines
//    
//    let _, _, hex = result
//    printfn "***************  Final Hex Output  **********************"
//    for l in hex do printfn "%A" l
//
//    let actual = concatLines hex
//    printContent dlxlines expected actual
//
//    actual |> should equal expected
//
//[<Test>]
//let ``match jtype`` () = ()
//
//[<Test>]
//let ``match label`` () =
//    let newLabel = Regex(@"(?<=(\w+):.*)(\w+)")
//    
//
//
//    let matchLabel (regex:Regex) input =
//        let matches = regex.Matches(input)
//        ([for m in matches -> m.Groups.[0].Value], matches.Count > 0)
//
//    let (|Label|_|) (symbolTable:Map<string, string>) (pc:uint32) (hex:string list) input =
//        matchLabel newLabel input |> function
//        | matches, true -> 
//            (symbolTable.Add(matches.Head, Conversions.pc2hex pc), pc, hex) |> Some
//        | _ -> 
//            None
//
////    let (|Label|_|) pc hex = function
////        | NewLabel pc hex result -> Some result
////        | _ -> None
//
//    let pc = 0u
//    let hex = List.empty<string>
//    let str = "label1: j label3"
//    let strs = [str]
//
//    let r symbolTable = ((pc,hex), strs) ||> Seq.fold (fun (pc:uint32, hex:string list) line ->
//        line |> function
//        | Label symbolTable pc hex result -> 
//            printfn "Result: %A" result
//            (pc + 1u, hex)
//        | _ -> 
//            (pc + 1u, hex)
//            ) //(0u, hex)
//
//    let st = Map.empty<string, string>
//    printfn "%A" (r st)


//
//
//
//[<Test>]
//let ``match rtype`` () = 
//    let str1 = "add r2, r3, r7"
//    let str2 = "nop"
//
//    let opcode, operands, opvalid = Patterns.Instruction.matchRType str1
//    
//    printfn "Opcode:\t%A" opcode
//    printfn "Operands:\t%A" operands
//    printfn "Op Valid?:\t%A" opvalid
//
//[<Test>]
//let ``match jtype`` () = 
//    let str = "jal label3"
//    
//    let opcode, operands, opvalid = Patterns.Instruction.matchJType str
//    
//    printfn "Opcode:\t%A" opcode
//    printfn "Operands:\t%A" operands
//    printfn "Op Valid?:\t%A" opvalid

//[<Test>]
//let ``itype opcode patterns`` () =
//    let it1 = "addi r1, r2, 4"
//    let m = Regex(Patterns.Opcode.itype).Matches(it1)
//    printfn "%A" m
//
//[<Test>]
//let ``rtype opcode patterns`` () =
//    let rt1 = "add r1, r2, r3"
//    let m = Regex(Patterns.Opcode.rtype).Matches(rt1)
//    printfn "%A" m
//
//[<Test>]
//let ``jtype opcode patterns`` () =
//    let jt1 = "jal label1"
//    let m = Regex(Patterns.Opcode.jtype).Matches(jt1)
//    printfn "%A" m

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

//    let m = Regex(Patterns.Operands.Immediate.any).Matches(ip5)
//    printfn "ip5: %A" m
//    printfn "%A" ([for m' in m -> m'.Groups.["imm"]])
//
//    let m = Regex(Patterns.Operands.Immediate.any).Matches(ip6)
//    printfn "ip6: %A" m

[<Test>]
let ``immediate patterns 2`` () =
    let regs = 
        List.init 32 (fun i -> 
            "r" + i.ToString(), 
            Convert.ToString(i, 2).PadLeft(5, '0'))
        |> Map.ofList
    //printfn "%A" regs
    
    //let rmap = Map.ofList [ "r1", ]
    let str = "
        add r1, r2, r3
        addf f1, f2, f31
        seq r1, r24, r3
        jal label2
        seqi f23, f6, 8
        subi r3, r10, 4(label3)"

//    let (|RType|_|) input =
//        let m = Regex(@"(?<reg>r\d\d?)").Match(input)
//        if m.Success then Some(List.tail [for g in m.Groups -> g.Value])
//        else None

    let (|Regis|_|) input =
        let m = Regex(@"r(?<reg>\d\d?)").Match(input)
        if m.Success then (Convert.ToString(int m.Groups.["reg"].Value, 2).PadLeft(5, '0'), input) |> Some
        else None

//    let (|Rtype|_|) input =
//        let m = Regex(Patterns.Opcode.rtypecg).Match(input)
//        if m.Success then (m.Groups.["rtype"].Value, input) |> Some
//        else None

//    str |> function
////    | RType str -> printfn "RType: %A" str
//    | Regis x -> printfn "Regis: %A" x
//    | _ -> printfn "no match" 

//    let rec f strlist = strlist |> function
//        | Rtype head :: tail ->
//            printfn "%s" (fst head)
//            f tail
//        | Regis head :: tail -> 
//            printfn "%s" (fst head)
//            f tail
//        | head :: tail -> f tail
//        | [] -> printfn "donezord"
//
//    let str = 
//        [
//            "add r1, r2, r3"
//            "addf f1, f2, f31"
//            "seq r1, r24, r3"
//            "jal label2"
//            "seqi f23, f6, 8"
//            "subi r3, r10, 4(label3)"
//        ]
//
//    f str
//
//
//    printfn "******************asciiz stuff********************"
//    let asc = ".asciiz \"hello\", \"greetings\", \"earthling\""
//    //let pat1 = @".asciiz (?<ascii>.*)"
//    let pat = @"[\.asciiz ]?""(?<str>[^""]+)"""
//    let matches = Regex(pat).Matches(asc)
//    printfn "Num Matches: %d" matches.Count
//    for m in matches do 
//        //printfn "%s" (m.Groups.["ascii"].Value)
//        printfn "%s" (m.Groups.["str"].Value)

//    let rec (|Instruction|_|) = function
//        | RType(rrid, rs1, rd, func) ->
//            let rec aux e1 = function
//              | "add"::RType(e2, t) -> aux (e1 + e2) t
//              | "r"::RType(e2, t) -> aux (e1 - e2) t
//              | t -> Some(e1, t)
//            aux e1 t
//        | _ -> None
//      and (|RType|_|) = function
//        | _ -> None
//      and (|IType|_|) = function
//        | _ -> None
//      and (|JType|_|) = function
//        | _ -> None
//
////        let m = Regex(pattern).Match(input)
////        if m.Success then Some(List.tail [for g in m.Groups -> g.Value])
////        else None
//    let (Instruction ins) = ["add r1, r2, r3"]
//    printfn "%A" ins
    
    let str = "seqi r5, r21, 4(label5)"
    let pats = [@"(\d\d?^\()"; @"(\d\(\w+\))"]
    ()
//    pats |> List.map (fun pat ->
//        str |> function
//        | Immediate pat [label] -> sprintf "%s" label
//        | _ -> sprintf "Not a match")
//    |> printfn "%A"
    
//    let parsef str = 
//        match str with
//        | BasePlusOffset @"(\d\d?\(\w+\))" [label] -> label
//        | _ -> "no match"
//
//    let r = parsef str
//    printfn "%s" (r.ToString())

//[<Test>]
//let ``match any opcode`` () =
//    let pat = Patterns.Opcode.itype + "|" + Patterns.Opcode.rtypecg + "|" + Patterns.Opcode.jtypecg
//    let str = "
//        add r1, r2, r3
//        addf f1, f2, f31
//        seq r1, r24, r3
//        jal label2
//        seqi f23, f6, 8
//        subi r3, r10, 4(label3)"
//    
//    let matches = Regex(pat).Matches(str)
//
//    printfn "Matches:\n%A" matches
//
//    printfn "Groups:"
//    for m in matches do printfn "%A" m.Groups
//
//    printfn "Derps:\n%A"
//        [for m in matches ->
//            (m.Groups.["itype"], m.Groups.["rtype"], m.Groups.["jtype"])]
//
//[<Test>]
//let ``match any operands`` () =
//    let pat = Patterns.Operands.any //Operands.fff + "|" + Operands.rrr + "|" + Operands.rri  
////    let str = "
////        add r1, r2, r3
////        addf f1, f2, f31
////        seqi r1, r24, 4(label1)
////        jal label2
////        seqi f23, f6, 8"
//    let str = "seqi r1, r24, 4(label2)"
//    let matches = Regex(pat).Matches(str)
//
//    printfn "Matches:\n%A" matches
//    
//    printfn "Groups:"
//    for m in matches do printfn "%A" m.Groups
//
//    printfn "IType Operands:"
//    for m in matches do printfn "%A" (m.Groups.["rd"], m.Groups.["rs1"], m.Groups.["imm"])
//
//    printfn "IType Operands # 2:"
//    for m in matches do
//        printfn "%A" m.Captures
//        let imm = m.Groups.["label"]
//        printfn "%A" imm //.["baseplusoffset"]
//
//[<Test>]
//let ``match any instruction`` () =
//    let pat = Patterns.instruction
//    let str = "
//        add r1, r2, r3
//        addf f1, f2, f31
//        seqi r1, r24, 4(label1)
//        jal label2
//        seqi f23, f6, 8"
//
//    let matches = Regex(pat).Matches(str)
//    printfn "Matches:\n%A" matches
//
