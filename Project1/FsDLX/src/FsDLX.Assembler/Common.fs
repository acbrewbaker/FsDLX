[<AutoOpen>]
module FsDLX.Common

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open NCrunch.Framework


let divup num den = (num + den - 1) / den

let WORD_SIZE = 32

let bytes2hex (b:byte[]) =
    (b |> Array.rev |> BitConverter.ToString)
        .Replace("-","").ToLower()

let str2hex (str:string) =
    (Encoding.Default.GetBytes(str) |> BitConverter.ToString).Replace("-","").ToLower() + "00"

type SymbolTable = Map<string, string>
type ProgramCounter = uint32
type Hex = string list

module Support =
    let srcdir = 
        if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
            Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
        else 
            Environment.CurrentDirectory

    let inputdir = Path.Combine(srcdir, @"../../../Inputs")

    let itypesfile, rtypesfile, jtypesfile = 
        Path.Combine(srcdir, @"../../support/Itypes"),
        Path.Combine(srcdir, @"../../support/Rtypes"),
        Path.Combine(srcdir, @"../../support/Jtypes")

    let parseTypeFile filepath =
        let pattern = @"(?<opcode>[^\s]+)\s+(?<rrid>\d\s+)*\s*(?<encoding>\d+)"
        let regex = new Regex(pattern, RegexOptions.Multiline)
        let matches = File.ReadAllText(filepath) |> regex.Matches
        [for m in matches -> 
            m.Groups.["opcode"].Value, 
            m.Groups.["rrid"].Value, 
            m.Groups.["encoding"].Value]

    type Opcodes(?itypesfile:string, ?rtypesfile:string, ?jtypesfile:string) =
        let itypesfile, rtypesfile, jtypesfile =
            defaultArg itypesfile (Path.Combine(srcdir, @"../../support/Itypes")),
            defaultArg rtypesfile (Path.Combine(srcdir, @"../../support/Rtypes")),
            defaultArg jtypesfile (Path.Combine(srcdir, @"../../support/Jtypes"))

        let itypes, rtypes, jtypes =
            Opcodes.ParseTypeFile itypesfile,
            Opcodes.ParseTypeFile rtypesfile,
            Opcodes.ParseTypeFile jtypesfile    

        let lookup =
            itypes @ rtypes @ jtypes
            |> List.map (fun (op, rrid, enc) -> (op, enc))
            |> Map.ofList

        member val ITypes = itypes |> List.map (fun (op, rrid, enc) -> (op, enc)) |> Map.ofList with get, set 

        member val RTypes = rtypes |> List.map (fun (op, rrid, enc) -> (op, (rrid, enc))) |> Map.ofList with get, set

        member val JTypes = jtypes |> List.map (fun (op, rrid, enc) -> (op, enc)) |> Map.ofList with get, set
        
        member o.Lookup(code) = lookup.[code]

        static member ParseTypeFile filepath = parseTypeFile filepath

module Types =

//    type Label(label:string, reference:string) =
//        let v = reference |> uint32
//        member val String = label with get, set
//        member val Reference = v with get, set
//
//    type Immediate =
//        | Val of string
//        | Register of string
//        | Label of string
//        //| BasePlusOffset of string
//    //
//    //    static member Match str = 
//    //        let pat = @"r\d\d?^\(|^\d\w+|\d\d?\(\w+\)"
//    //        let matches = Regex(pat).Match(str)
//    //        printfn "%A" matches
//    
//    //        imm |> function
//    //        | Val str -> ()
//    //        | Register str -> ()
//    //        | Label str -> ()
//    //        | BasePlusOffset str -> ()
//
//
//    type PC =
//        | String of string
//        | Val of uint32
//
//        override pc.ToString() = pc |> function
//            | String pc -> pc
//            | Val pc -> pc.ToString("x8")

//    type Primitive = 
//        | Int of int
//        | UInt of uint32
//        | Single of float32
//        | Double of float
//        | Hex of string
//
//        member dp.ToHex = dp |> function
//            | Int x ->      x |> BitConverter.GetBytes |> bytes2hex
//            | UInt x ->     x |> BitConverter.GetBytes |> bytes2hex
//            | Single x ->   x |> BitConverter.GetBytes |> bytes2hex
//            | Double x ->   x |> BitConverter.GetBytes |> bytes2hex
//            | Hex x -> x
//
//        static member AsHex = function
//            | Int x ->      x |> BitConverter.GetBytes |> bytes2hex |> Primitive.Hex
//            | UInt x ->     x |> BitConverter.GetBytes |> bytes2hex |> Primitive.Hex
//            | Single x ->   x |> BitConverter.GetBytes |> bytes2hex |> Primitive.Hex
//            | Double x ->   x |> BitConverter.GetBytes |> bytes2hex |> Primitive.Hex
//            | Hex x ->      x                                       |> Primitive.Hex

    let unused = "000000"

//    type Opcode(name:string, ?opcodes:Support.Opcodes) =
//        let opcodes = defaultArg opcodes (Support.Opcodes())
//        let encoding = opcodes.Lookup(name) |> int
//        member val Name = name
//        member val IntEncoding = encoding
//        member val HexEncoding = Convert.ToString(encoding, 16).PadLeft(6, '0')

    //type Opcode(name:string, code:int) =
    //    member val Name = name with get, set
    //    member val Code = code with get, set


//    type RegisterT(?value:uint32, ?displayInBinary:bool) =
//        let mutable value = defaultArg value 0u
//        let displayInBinary = defaultArg displayInBinary false
//        let bytes = BitConverter.GetBytes(value)
//        member r.Byte0 = bytes.[0]
//        member r.Byte1 = bytes.[1]
//        member r.Byte2 = bytes.[2]
//        member r.Byte3 = bytes.[3]
//        member r.Value
//            with get() = value
//            and set(v) = value <- v
//        member r.Bit b = r.Value &&& (1u <<< b)
//        override r.ToString() = 
//            if displayInBinary then r.Value.ToString("2")
//            else r.Value.ToString("x8")

//    type Register =
//        | String of string
//        | Val of RegisterT
//
//        member r.Set(v) = r |> function
//            | String rid -> ()
//            | Val reg -> reg.Value <- v
//
    
//    type Opcode =
//        | Name of string
//        | Encoding of int
//
//
//
//    type Immediate =
//        | String of string
//        | Val of uint32
//
//    type RRid =
//        | String of string
//        | Val of int
//
//    type Instruction =
//        | IType of Opcode * Register * Register * Immediate
//        | RType of RRid * Register * Register * Register * Opcode
//        | JType of Opcode * Immediate
//
//        member private ins.Encode = ins |> function
//            | IType(opcode, rs1, rd, imm) ->
//                opcode  .PadLeft(6, '0') + 
//                rs1     .PadLeft(5, '0') + 
//                rd      .PadLeft(5, '0') + 
//                imm     .PadLeft(16, '0')
//                
//            | RType(rru, rs1, rs2, rd, func) ->
//                (string rru)    .PadLeft(6, '0') +
//                rs1             .PadLeft(5, '0') +
//                rs2             .PadLeft(5, '0') +
//                rd              .PadLeft(5, '0') +
//                unused                           +
//                func            .PadLeft(5, '0')
//            | JType(opcode, name) ->
//                opcode  .PadLeft(6, '0') +
//                name    .PadLeft(26, '0')
//
//        member ins.asBinaryString = ins.Encode
//        member ins.asUInt32 = Convert.ToUInt32(ins.Encode, 2)
//        member ins.asHexString = ins.asUInt32.ToString("x8")
//    
//        override ins.ToString() = ins.asHexString

    type Opcode =
        | Name of string
        | Encoding of int
        | Raw of string * string

    type Operands = Group list

    type Instruction = int

module Conversions =

    let pc2hex (pc:uint32) = pc.ToString("x8")
    let strAsComment str = "\t#\"" + str + "\""
    let asComment str = "    \t# " + str
    let addLeadingZero (str:string) =
        str |> function
        | _ when str.StartsWith("-.") -> str.Insert(str.IndexOf("-.") + 1, "0")
        | _ when str.StartsWith("+.") -> str.Insert(str.IndexOf("+.") + 1, "0")
        | _ when str.StartsWith(".") -> str.Insert(str.IndexOf("."), "0")
        | _ -> str
    let floatingPointAsComment = addLeadingZero >> asComment

    let reg2bin (r:string) =
        Convert.ToString(r.Substring(1) |> int, 2)

    let dlx2hex (convert:uint32 -> string -> uint32*string) (matches:string list) (pcAndHexState:uint32*(string list)) =
        (pcAndHexState, matches) 
        ||> List.fold (fun (pc, hex) str -> 
            let newpc, newhex = convert pc str
            (newpc, hex @ [newhex]))
 
    module Directive =
        let map = 
            [
                ".text", fun (pc:uint32) (str:string) ->
                    let newpc = uint32 str
                    let newstr =
                        pc2hex pc + ": " +
                        str +
                        asComment str
                    (newpc, newstr)
 
                ".data", fun (pc:uint32) (str:string) -> 0u, ""

                ".align", fun (pc:uint32) (str:string) ->
                    let n = Double.Parse str
                    let bytes = (2.0 ** n) |> int
                    printfn "%A" bytes
                    let align = (divup bytes WORD_SIZE) |> uint32
                    let newpc = pc + align    
                    let newstr = 
                        pc2hex pc + ": " +
                        (align |> BitConverter.GetBytes |> bytes2hex) +
                        asComment str
                    (newpc, newstr)

                ".asciiz", fun (pc:uint32) (str:string) ->
                    let bytes = Encoding.Default.GetBytes(str)
                    let newstr =
                        pc2hex pc + ": " + 
                        str2hex str +
                        strAsComment str
                    let newpc = pc + uint32 bytes.Length + 1u
                    (newpc, newstr)

                ".double", fun (pc:uint32) (str:string) ->
                    let newpc = pc + 8u
                    let newstr = 
                        pc2hex pc + ": " +
                        (Double.Parse(str) |> BitConverter.GetBytes |> bytes2hex) +
                        floatingPointAsComment str
                    (newpc, newstr)

                ".float", fun (pc:uint32) (str:string) -> 
                    let newpc = pc + 4u
                    let newstr = 
                        pc2hex pc + ": " +
                        (Single.Parse(str) |> BitConverter.GetBytes |> bytes2hex) +
                        floatingPointAsComment str
                    (newpc, newstr)

                ".word", fun (pc:uint32) (str:string) ->
                    let newpc = pc + 4u
                    let str, isNeg = if str.StartsWith("-") then str.Replace("-",""), true else str, false
                    let base' = if str.StartsWith("0x") then 16 else 10
                    let word = Convert.ToInt32(str, base') * (if isNeg then -1 else 1)
                    let newstr =
                        pc2hex pc + ": " +
                        (word |> BitConverter.GetBytes |> bytes2hex) +
                        asComment (string word)
                    (newpc, newstr)

                ".space", fun (pc:uint32) (str:string) -> 0u, ""
            ] |> Map.ofList
 
        let dlx2hex (directive:string) = dlx2hex (map.[directive])

    module Instruction =
        let opcodes = Support.Opcodes()
        let itype (pc:uint32) (opcode:string) (operands:GroupCollection) =
            let op = opcodes.ITypes.[opcode]
            printfn "OpCode: %A" (Convert.ToString(op |> int, 2))
            let rd, rs1, imm =
                operands.["rd"].Value |> reg2bin,
                operands.["rs1"].Value |> reg2bin,
                operands.["imm"].Value
            Convert.ToString(opcodes.ITypes.[opcode] |> int, 2) +
            rs1.PadLeft(5, '0') +
            rd.PadLeft(5, '0') +
            imm.PadLeft(16, '0')
            |> printfn "%A"

        let map =
            [
                "itype", fun (pc:uint32) (operands:GroupCollection) -> 
                    let rd, rs1, imm =
                        operands.["rd"].Value,
                        operands.["rs1"].Value,
                        operands.["imm"].Value
                    0u, ""
                "rtype", fun (pc:uint32) (operands:GroupCollection) -> 
                    let rru, rd, rs1, rs2, func =
                        operands.["rru"].Value,
                        operands.["rd"].Value,
                        operands.["rs1"].Value,
                        operands.["rs2"].Value,
                        operands.["func"].Value
                    0u, ""
                "jtype", fun (pc:uint32) (operands:GroupCollection) -> 
                    let label = operands.["label"].Value
                    0u, ""   
            ] |> Map.ofList

        let dlx2hex (opcode:string) = dlx2hex (Directive.map.[opcode])

module Patterns =
    let comment = @"(?<comment>.+;.*)"
    let opcode = @"(?<opcode>[^\s]+\s)"
    let reg = @"r\d\d?"
    let freg = @"f\d\d?"
    let label = @"\w+:"
    let imm = @"\d+|\w+|\d+\x40[r\d\d?|\w+]\x41"

    module Label =
        module private DlxRegex =
            let newlabel = Regex(@"(?<=(\w+):.*)(\w+)")
        
        let matchLabel (regex:Regex) input =
            let matches = regex.Matches(input)
            ([for m in matches -> m.Groups.["label"].Value], matches.Count > 0)

        let (|NewLabel|_|) (symbolTable:Map<string, string>) (pc:uint32) (hex:string list) input =
            matchLabel DlxRegex.newlabel input |> function
            | matches, true -> 
                (symbolTable.Add(matches.Head, Conversions.pc2hex pc), pc, hex) |> Some
            | _ -> None

    module Directive =
        module private DlxRegex =
            let text    = Regex(@"(?<=\.(text).*)(\d+)")
            let data    = Regex(@"(?<=\.(data).*)(\d+)")
            let align   = Regex(@"(?<=\.(align) )(\d)")
            let asciiz  = Regex(@"(\.asciiz )?""([^""]+)""")
            let double  = Regex(@"(?<=\.(double).*)([+-]?\d*\.\d+)")
            let float   = Regex(@"(?<=\.(float).*)([+-]?\d*\.\d+)")
            let word    = Regex(@"(?<=\.(word).*)((-)?(0x)?\d+)")
            let space   = Regex(@"(?<=\.(space).*)(\d+)")
        
        let matchDirective (regex:Regex) (input:string) = 
            let matches = regex.Matches(input)
            ([for m in matches -> m.Groups.[2].Value], matches.Count > 0)

        let (|Text|_|) (pc:uint32) (hex:string list) input =
            matchDirective DlxRegex.text input |> function
            | matches, true -> Conversions.Directive.dlx2hex ".text" matches (pc, hex) |> Some
            | _ -> None

        let (|Data|_|) (pc:uint32) (hex:string list) input =
            matchDirective DlxRegex.data input |> function
            | matches, true -> Conversions.Directive.dlx2hex ".data" matches (pc, hex) |> Some
            | _ -> None

        let (|Align|_|) (pc:uint32) (hex:string list) input =
            matchDirective DlxRegex.align input |> function
            | matches, true -> Conversions.Directive.dlx2hex ".align" matches (pc, hex) |> Some
            | _ -> None

        let (|Asciiz|_|) (pc:uint32) (hex:string list) input =
            matchDirective DlxRegex.asciiz input |> function
            | matches, true -> Conversions.Directive.dlx2hex ".asciiz" matches (pc, hex) |> Some
            | _ -> None

        let (|Double|_|) (pc:uint32) (hex:string list) input =
            matchDirective DlxRegex.double input |> function
            | matches, true -> Conversions.Directive.dlx2hex ".double" matches (pc, hex) |> Some
            | _ -> None

        let (|Float|_|) (pc:uint32) (hex:string list) input =
            matchDirective DlxRegex.float input |> function
            | matches, true -> Conversions.Directive.dlx2hex ".float" matches (pc, hex) |> Some
            | _ -> None

        let (|Word|_|) (pc:uint32) (hex:string list) input =
            matchDirective DlxRegex.word input |> function
            | matches, true -> Conversions.Directive.dlx2hex ".word" matches (pc, hex) |> Some
            | _ -> None

        let (|Space|_|) (pc:uint32) (hex:string list) input =
            matchDirective DlxRegex.space input |> function
            | matches, true -> Conversions.Directive.dlx2hex ".space" matches (pc, hex) |> Some
            | _ -> None


    module Instruction =
        let opcodes  = Support.Opcodes()

        module private DlxRegex =
            let itype = @"beqz|bnez|addi|addui|subi|subui|andi|ori|xori|lhi|trap|jr|jalr|slli|srli|srai|seqi|snei|slti|sgti|slei|sgei|lb|lh|lw|lbu|lhu|lf|ld|sb|sh|sw|sf|sd"
            let rtype = @"nop|sll|srl|sra|add|addu|sub|subu|and|or|xor|seq|sne|slt|sgt|sle|sge|movf|movd|movfp2i|movi2fp|addf|subf|multf|divf|addd|subd|multd|divd|cvtf2d|cvtf2i|cvtd2f|cvtd2i|cvti2f|cvti2d|mult|div|multu|divu"
            let jtype = @"j|jal"            

        module Operands =
            module Immediate =
                let immLabel = @"(?<label>\w+)"
                let immReg = reg
                let immVal = @"(?<val>\d\d?^[\(])"
                let immBasePlusOffset = @"(?<baseplusoffset>(?<label>\d\d?\(\w+\))|(?<register>\d\d?\(r\d\d?\)))"
                let any = @"(?<imm>" + immLabel + "|" + immReg + "|" + immVal + "|" + immBasePlusOffset + ")"
            
                let (|BasePlusOffset|_|) str =
                    let m = Regex.Match(@"\d\d?\(\w+\)", str)
                    if m.Success then Some(List.tail [for g in m.Groups -> g.Value])
                    else None

            let rrr = @"(?<rd>" + reg + "), (?<rs1>" + reg + "), (?<rs2>" + reg + ")"
            let fff = @"(?<rd>" + freg + "), (?<rs1>" + freg + "), (?<rs2>" + freg + ")"
            let rri = @"(?<rd>" + reg + "), (?<rs1>" + reg + "), " + Immediate.any //"(?<imm>" + imm + ")" //Immediate.any

            let any = @"(?<operands>" + rrr + "|" + fff + "|" + rri + ")"



        let operands = Operands.any

        let itypecg = @"(?<=(?<itype>" + DlxRegex.itype + ").*)(?<operands>" + Operands.any + ")"
        let rtypecg = @"(?<=(?<rtype>" + DlxRegex.rtype + ").*)(?<operands>" + operands + ")"
        let jtypecg = @"(?<=(?<jtype>" + DlxRegex.jtype + ").*)(?<operands>" + Operands.Immediate.any + ")"

        let matchIType input =
            let opcode, operands = [
                for m in Regex(itypecg).Matches(input) -> 
                    m.Groups.["itype"].Value, m.Groups] |> List.unzip
            (opcode.Head, operands, opcode.Length <> 0)

        let matchRType input =
            let opcode, operands = [
                for m in Regex(rtypecg).Matches(input) -> 
                    m.Groups.["rtype"].Value, m.Groups.["operands"].Value] |> List.unzip
            (opcode.Head, operands, opcode.Length <> 0)

        let matchJType input =
            let opcode, operands = [
                for m in Regex(jtypecg).Matches(input) -> 
                    m.Groups.["jtype"].Value, m.Groups.["operands"].Value] |> List.unzip
            (opcode.Head, operands, opcode.Length <> 0)


        let instruction = @"(?<opcode>" + itypecg + "|" + rtypecg + "|" + jtypecg + @"])\s" + Operands.any

        let (|IType|_|) (pc:uint32) (hex:string list) input =
            matchIType input |> function
            | opcode, operands, true -> 
                operands.Head |> printfn "%A"
                Conversions.Instruction.itype pc opcode operands.Head
//                let ops = [for g in operands -> g.]
                Conversions.Instruction.dlx2hex opcode [""] (pc, hex) |> Some
            | _ -> None

        let (|RType|_|) (pc:uint32) (hex:string list) input =
            matchRType input |> function
            | opcode, operands, true -> Conversions.Instruction.dlx2hex opcode operands (pc, hex) |> Some
            | _ -> None

        let (|JType|_|) (pc:uint32) (hex:string list) input =
            matchIType input |> function
            | opcode, operands, true -> Conversions.Instruction.dlx2hex opcode [""] (pc, hex) |> Some
            | _ -> None

    let (|Label|_|) symbolTable pc hex = function
        | Label.NewLabel symbolTable pc hex result -> Some result
        | _ -> None


    let (|Directive|_|) pc hex = function
        | Directive.Text pc hex result -> Some result
        | Directive.Data pc hex result -> Some result
        | Directive.Align pc hex result -> Some result
        | Directive.Asciiz pc hex result -> Some result
        | Directive.Double pc hex result -> Some result
        | Directive.Float pc hex result -> Some result
        | Directive.Word pc hex result -> Some result
        | Directive.Space pc hex result -> Some result
        | _ -> None
        

    let (|Instruction|_|) pc hex = function
        | Instruction.IType pc hex result -> Some result
        | Instruction.RType pc hex result -> Some result
        | Instruction.JType pc hex result -> Some result
        | _ -> None