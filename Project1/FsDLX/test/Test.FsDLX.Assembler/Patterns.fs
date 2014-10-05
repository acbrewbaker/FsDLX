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
let ``match itype`` () = 
    let str1 = "addi r1, r21, 4(r5)"
    let str2 = "seqi r4, r5, 4(label3)"

    let matchIType input =
        let opcode, operands = [
            for m in Regex(Patterns.Instruction.itypecg).Matches(input) -> 
                m.Groups.["itype"].Value, 
                (m.Groups.["rd"].Value,
                 m.Groups.["rs1"].Value,
                 m.Groups.["imm"].Value)] |> List.unzip
        (opcode.Head, operands, opcode.Length <> 0)

//    let r = Patterns.Instruction.matchIType str1 |||> Conversions.Instruction.itype
//    printfn "Result: %A" r

    let opcode, operands, opvalid = matchIType str1 //Patterns.Instruction.matchIType str1
    
    printfn "Opcode:\t%A" opcode
    printfn "Operands:\t%A" operands
    printfn "Operands Count:\t%A" (operands.Head)
    printfn "Op Valid?:\t%A" opvalid

    //printfn "Conversion: %A" ((4u, opcode, operands.Head) |||> Conversions.Instruction.itype)

//    let rd, rs1, imm = operands.Head
//    printfn "Operands Separated:\t%A, %A, %A" rd rs1 imm

//
//[<Test>]
//let ``match itype 2`` () =
//    let str1 = "addi r1, r21, 4(r5)"
//    let str2 = "seqi r4, r5, 4(label3)"

let opcodes = Opcodes()

type SymbolTable() =
    let table = Map.empty<string, string>
    let immLabels = Map.empty<string, int>
    member val private Table = table with get, set
    member val private ImmLabels = immLabels with get, set
    member st.Lookup(label, lineNumber) = 
        st.Table.TryFind(label) |> function
        | Some address -> 
            Some address
        | None -> 
            st.ImmLabels <- st.ImmLabels.Add(label, lineNumber)
            None
    member st.Add(label, pc) = 
        st.Table <- st.Table.Add(label, pc)
        st

    override st.ToString() =
        sprintf "=== Table ===\n%s\n=== Imm Labels ===\n%s" 
            (st.Table.ToString()) 
            (st.ImmLabels.ToString())

let str2option = function | "" -> None | str -> Some str

type Immediate =
    | Label of string
    | Value of string

    member imm.Convert(st:SymbolTable, lineNumber:int) = imm |> function
        | Label x -> st.Lookup(x, lineNumber) |> function | Some address -> address | None -> failwith "fail"
        | Value x -> x.PadLeft(16, '0')

    static member Create str =
        let imm = ref 0
        Int32.TryParse(str, imm) |> function
        | true -> Immediate.Value(Convert.ToString(!imm, 2))
        | false -> Immediate.Label(str)

type Operands =
    | IType of string * string * string
    | RType of string * string * string * string
    | JType of string

    static member Init (groups:GroupCollection) =
        (   groups.["rru"].Value |> str2option, 
            groups.["rd"].Value |> str2option, 
            groups.["rs1"].Value |> str2option, 
            groups.["rs2"].Value |> str2option, 
            groups.["imm"].Value |> str2option) |> function
        | None, Some rd, Some rs1, None, Some imm -> IType(rd, rs1, imm)
        | Some rru, Some rd, Some rs1, Some rs2, None -> RType(rru, rd, rs1, rs2)
        | None, None, None, None, Some imm -> JType(imm)
        | _ -> failwith "couldn't create valid instruction DU"
        

//type Instruction =
//    | IType of string * string * string * string
//    | RType of string * string * string * string
//    | JType of string * string
//
//    static member Create (groups:GroupCollection) =
//        let operands =
//            groups.["rru"].Value,
//            groups.["rd"].Value,
//            groups.["rs1"].Value,
//            groups.["rs2"].Value,
//            groups.["imm"].Value

[<Test>]
let ``itype conversion`` () =
    let dlx = File.ReadAllLines(Path.Combine(inputdir, "setImmed.dlx"))
    printfn "========  DLX  ========\n%A" dlx
    let hex = File.ReadAllText(Path.Combine(inputdir, "setImmed.hex")).Replace("\r", "")
    printfn "========  HEX (Expected) ========\n%A" hex
    
    printfn "none = > %A" (None)
    printfn "%A" (str2option "derp")

    let lblpat = @"((?<label>\w+):)?"
    let lblregex = Regex(lblpat)
    let itpar = Patterns.Instruction.itypecg
    
    let regex = Regex(lblpat + itpar)
    
    let matchLabel (regex:Regex) input =
        let matches = regex.Matches(input)
        ([for m in matches -> m.Groups.["label"].Value], matches.Count > 0)

    let (|NewLabel|_|) (symbolTable:SymbolTable) (pc:uint32) (hex:string list) input =
        matchLabel lblregex input |> function
        | matches, true -> 
            (symbolTable.Add(matches.Head, Conversions.pc2hex pc), pc, hex) |> Some
        | _ -> None

    let matchInstruction (regex:Regex) (input:string) =
        let matches = regex.Matches(input.Trim())
        let label = [for m in matches -> m.Groups.["label"].Value].Head
        
        let opcode, operands = [
            for m in matches ->
                m.Groups.["itype"].Value, 
                (m.Groups.["rd"].Value,
                 m.Groups.["rs1"].Value,
                 m.Groups.["rs2"].Value,
                 m.Groups.["imm"].Value)] |> List.unzip
        printfn "Operands ==> %A" operands.Head
        (opcode.Head, operands.Head, matches.Count > 0)

    
    let conversion (st:SymbolTable) (pc:uint32) (hex:string list) (opcode:string) (operands:string*string*string*string) =
        let rd, rs1, rs2, imm = operands
        let imm = Immediate.Create(imm).Convert(st, 1)
        let encoding = opcodes.Lookup(opcode)
        let newpc = pc + 4u
        let newstr =
            Conversions.pc2hex pc + ": " +
            encoding.PadLeft(6,'0') +
            (Conversions.reg2bin rd).PadLeft(5, '0') +
            (Conversions.reg2bin rs1).PadLeft(5, '0') +
            imm
        (st, newpc, hex @ [newstr])

    let (|IType|_|) (st:SymbolTable) (pc:uint32) (hex:string list) input =
        matchInstruction regex input |> function
        | opcode, operands, true -> conversion st pc hex opcode operands |> Some
        | _ -> None

    let st, pc, hex = SymbolTable(), 0u, List.empty<string>
    let r = ((st,pc,hex),dlx) ||> Seq.fold(fun (st,pc,hex) line ->
        line |> function
        | IType st pc hex line -> line
        | _ -> 
            (st, pc + 1u, hex))

    let st, pc, lines = r
    printfn "%A" st
    printfn "%A" lines
    ()

[<Test>]
let ``match rtype`` () = ()

[<Test>]
let ``match jtype`` () = ()

[<Test>]
let ``match label`` () =
    let newLabel = Regex(@"(?<=(\w+):.*)(\w+)")
    


    let matchLabel (regex:Regex) input =
        let matches = regex.Matches(input)
        ([for m in matches -> m.Groups.[0].Value], matches.Count > 0)

    let (|Label|_|) (symbolTable:Map<string, string>) (pc:uint32) (hex:string list) input =
        matchLabel newLabel input |> function
        | matches, true -> 
            (symbolTable.Add(matches.Head, Conversions.pc2hex pc), pc, hex) |> Some
        | _ -> 
            None

//    let (|Label|_|) pc hex = function
//        | NewLabel pc hex result -> Some result
//        | _ -> None

    let pc = 0u
    let hex = List.empty<string>
    let str = "label1: j label3"
    let strs = [str]

    let r symbolTable = ((pc,hex), strs) ||> Seq.fold (fun (pc:uint32, hex:string list) line ->
        line |> function
        | Label symbolTable pc hex result -> 
            printfn "Result: %A" result
            (pc + 1u, hex)
        | _ -> 
            (pc + 1u, hex)
            ) //(0u, hex)

    let st = Map.empty<string, string>
    printfn "%A" (r st)


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
//    let pat = Patterns.Opcode.itypecg + "|" + Patterns.Opcode.rtypecg + "|" + Patterns.Opcode.jtypecg
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
