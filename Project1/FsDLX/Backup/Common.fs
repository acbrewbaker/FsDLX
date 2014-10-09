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

let pc2hex (pc:uint32) = pc.ToString("x8")

let str2option = function | "" -> None | str -> Some str

let group2option (g:Group) = g.Value |> str2option

let reg2bin (r:string) = Convert.ToString(r.Substring(1) |> int, 2)

let concatLines lines = lines |> List.fold (fun r s -> r + s + "\n") ""

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

module Conversions =

    let pc2hex (pc:uint32) = pc.ToString("x8")
    let strAsComment str = "\t#\"" + str + "\""
    let asComment str = "\t#" + str
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
    let comment = @"(;.*)"
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
            let align   = Regex(@"(?<=\.(align).*)(\d)")
            let asciiz  = Regex(@"(?<=\.(asciiz).*)?""([^""]+)""")
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
                let immLabel = @"(,? )(?<label>[a-zA-Z]+\d*)"
                let immReg = @"(,? )(?<register>r\d\d?)"
                let immVal = @"(,? )(?<val>\d+)"
                let immBasePlusOffset = "(,? )(?<base>\d)\((?<offset>.*)\)"
                
                let any = @"(?<imm>" + immLabel + "|" + immReg + "|" + immVal + "|" + immBasePlusOffset + ")"
            

            let rrr = @"(?<rs1>r\d\d?), (?<rs2>r\d\d?), (?<rd>r\d\d?)"
            let fff = @"(?<rs1>f\d\d?), (?<rs2>f\d\d?), (?<rd>f\d\d?)"
            let rri = @"(?<rs1>r\d\d?), (?<rd>r\d\d?)" + Immediate.any

            
            let itype = @"(?<operands>" + rri + ")"
            let rtype = @"(?<operands>" + rrr + "|" + fff + ")"
            let jtype = @"(?<operands>" + Immediate.any + ")"
            let any = @"(?<operands>" + rrr + "|" + fff + "|" + rri + ")"

        let operands = Operands.any

        let itype = comment + "|" + @"(?<=(?<itype>" + DlxRegex.itype + ").*)" + Operands.itype + "|" + comment + "$"
        let rtype = comment + "|" + @"(?<=(?<rtype>" + DlxRegex.rtype + ").*)" + Operands.rtype + "|" + comment + "$"
        let jtype = comment + "|" + @"(?<=(?<jtype>" + DlxRegex.jtype + ").*)" + Operands.jtype + "|" + comment + "$"

        let matchIType input =
            let opcode, operands = [
                for m in Regex(itype).Matches(input) -> 
                    m.Groups.["itype"].Value, m.Groups] |> List.unzip
            (opcode.Head, operands, opcode.Length <> 0)

        let matchRType input =
            let opcode, operands = [
                for m in Regex(rtype).Matches(input) -> 
                    m.Groups.["rtype"].Value, m.Groups.["operands"].Value] |> List.unzip
            (opcode.Head, operands, opcode.Length <> 0)

        let matchJType input =
            let opcode, operands = [
                for m in Regex(jtype).Matches(input) -> 
                    m.Groups.["jtype"].Value, m.Groups.["operands"].Value] |> List.unzip
            (opcode.Head, operands, opcode.Length <> 0)


        
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


type ProgramCounter =
    | Value of uint32
    | Hex of string

    member pc.ForHexOutput = pc |> function
        | Value v -> pc.ToString() + ": "
        | Hex h -> h + ": "
    
    member pc.ToBinary() = pc |> function
        | _ -> Convert.ToString(pc.ToUint32() |> int, 2)

    member pc.ToUint32() = pc |> function
        | Value v -> v
        | Hex h -> Convert.ToUInt32(h, 16)

    member pc.GetNewPC(oldpc:uint32) = pc |> function
        | Value v -> ProgramCounter.Value(v + oldpc)
        | _ -> ProgramCounter.Value(pc.ToUint32() + oldpc)

    member pc.GetNewPC() = pc |> function
        | Value v -> ProgramCounter.Value(v + 4u)
        | _ -> ProgramCounter.Value(pc.ToUint32() + 4u)

    override pc.ToString() = pc |> function
        | Value v -> pc2hex v
        | Hex h -> h

type Label = string

type Address =
    | String of string
    | Value of uint32

type SymbolTable() =
    let table = Map.empty<Label, ProgramCounter>
    member val private Table = table with get, set

    member st.Lookup(label) =
        st.Table.TryFind(label) |> function | Some address -> Some address | None -> None

    member st.Add(label:Label, pc) =
        st.Table <- st.Table.Add(label, pc)
        st
    
    member st.Display() = 
        st.Table |> Map.iter (fun k v -> printfn "%A" (k,v,v.ToBinary()))


type HexOutput = string list

type AssemblerState = SymbolTable * ProgramCounter * HexOutput

type MatchFunction = Regex -> string -> GroupCollection
type ConversionFunction = AssemblerState -> string -> AssemblerState

type Opcode =
    | IType of string
    | RType of string
    | JType of string
    
    member o.Encode(?opcodes:Support.Opcodes) = 
        let opcodes = defaultArg opcodes (Support.Opcodes())
        let encoding = o |> function
            | IType op -> opcodes.Lookup(op)
            | RType op -> opcodes.Lookup(op)
            | JType op -> opcodes.Lookup(op)
        Convert.ToString(int encoding, 2).PadLeft(6, '0')

//    member o.ToBinary(?opcodes:Support.Opcodes) = 
//        let opcodes = defaultArg opcodes (Support.Opcodes())
//        o.Encode(opcodes)
//
//    member o.ToHex() = 
//        let opcodes = defaultArg opcodes (Support.Opcodes())
//        Convert.ToUInt32()

    static member Init (groups:GroupCollection) =
        printfn "%A" groups
        (   groups.["itype"].Value |> str2option,
            groups.["rtype"].Value |> str2option,
            groups.["jtype"].Value |> str2option) |> function
        | Some opcode, None, None -> IType(opcode)
        | None, Some opcode, None -> RType(opcode)
        | None, None, Some opcode -> JType(opcode)
        | _ -> failwith "couldn't create valid opcode DU"

//type RRU =
//    | Integer = 0
//    | FloatingPoint = 1

type Register =
    | String of string
    | Value of string

    member reg.Encode() = reg |> function
        | String r -> (reg2bin r).PadLeft(5, '0')
        | _ -> "Value type of Register DU not used yet"

type Immediate =
    | Value of string
    | Label of Label
    | Register of Register
    | BasePlusOffset of string * string

//    member imm.Convert(st:SymbolTable, lineNumber:int) = imm |> function
//        | Label lbl -> st.Lookup(x, lineNumber) |> function | Some address -> address | None -> failwith "fail"
//        | Value x -> x.PadLeft(16, '0')
//        | _ -> ""

    member imm.Encode() = 
        let encoding = imm |> function
            | Value value -> Convert.ToString((int value), 2)
            | Label label -> label
            | Register register -> register.Encode()
            | BasePlusOffset(b, offset) -> Convert.ToString((int b) + (int offset), 2)
        encoding.PadLeft(16, '0')
            
    member imm.LabelToAddress(st:SymbolTable) = 
        let update = function | Some (pc:ProgramCounter) -> pc.ToBinary() | None -> failwith ""
        imm |> function
        | Label label -> label |> st.Lookup |> update |> Label            
        | BasePlusOffset(b, offset) -> BasePlusOffset(b, offset |> st.Lookup |> update)
        | _ -> imm
        
    static member InitBasePlusOffset (groups:GroupCollection) =
        (   groups.["base"] |> group2option,
            groups.["offset"] |> group2option) |> function
        | Some b, Some offset -> BasePlusOffset(b, offset)
        | _ -> failwith "missing base and/or offset"

    static member Init (groups:GroupCollection) = 
        (   groups.["label"] |> group2option,
            groups.["register"] |> group2option,
            groups.["val"] |> group2option) |> function
        | Some label, None, None -> Label label
        | None, Some register, None -> register |> Register.String |> Register
        | None, None, Some value -> Value value
        | None, None, None -> Immediate.InitBasePlusOffset groups
        | _ -> failwith "couldn't construct a valid Immediate DU"
        
    static member Parse (regex:Regex) (input:string) = ()

type Operands =
    | IType of Register * Register * Immediate
    | RType of Register * Register * Register
    | JType of Immediate

    member ops.Encode() = ops |> function
        | IType(rs1, rd, imm) -> 
            rs1.Encode() +
            rd.Encode() +
            imm.Encode()
        | RType(rs1, rs2, rd) -> ""
        | JType(imm) -> ""

    member ops.LabelToAddress(st:SymbolTable) = ops |> function
        | IType(rs1, rd, imm) -> IType(rs1, rd, imm.LabelToAddress st)
        | JType(imm) -> JType(imm.LabelToAddress st)
        | RType(_) -> ops

    static member Init(groups:GroupCollection) =
        (   groups.["rs1"].Value |> str2option, 
            groups.["rs2"].Value |> str2option, 
            groups.["rd"].Value |> str2option, 
            groups.["imm"] |> group2option) |> function
        | Some rs1, None, Some rd, Some imm -> 
            IType(Register.String rd, Register.String rs1, Immediate.Init groups)
        | Some rs1, Some rs2, Some rd, _ -> 
            RType(Register.String rs1, Register.String rs2, Register.String rd)
        | None, None, None, Some imm -> JType(Immediate.Init groups)
        | _ -> failwith "couldn't create valid instruction DU"

type Instruction =
    | Raw of string 
    | Parsed of Opcode * Operands
    
    member i.Parse regex (f:Regex -> string -> GroupCollection) = i |> function
        | Raw instruction -> 
            let groups = f regex instruction
            Parsed(Opcode.Init groups, Operands.Init groups)
        | Parsed(opcode, operands) -> i
    
    member private i.Encode() = i |> function
        | Parsed(opcode, operands) -> 
            opcode.Encode() +
            operands.Encode()
        | Raw str -> failwith "cant encode raw function!"

    member i.ToHex() = Convert.ToUInt32(i.Encode(), 2).ToString("x8")
    member i.ToBinary() = i.Encode()

    member i.LabelToAddress(st:SymbolTable) = i |> function 
        | Parsed(opcode, operands) -> Parsed(opcode, operands.LabelToAddress st)
        | _ -> i

    static member Init (groups:GroupCollection) =
        Parsed(Opcode.Init groups, Operands.Init groups)

    static member Match (regex:Regex) (input:string) =
        Instruction.Init([for m in regex.Matches(input) -> m.Groups].Head)
        


type AssemblerInput =
    | Comment of string
    | Directive of ProgramCounter * Label option * string
    | Instruction of ProgramCounter * Label option * Instruction

    static member Parse (regex:Regex) (line:string) (pc:ProgramCounter) =
        let matches = regex.Matches(line)
        let groups = [for m in matches -> m.Groups].Head
        let label = [for m in matches -> m.Groups.["newlabel"].Value].Head.Replace(":","")
        printfn "new label: %A" label
        label |> str2option |> function
        | Some label -> Instruction(pc, Some label, Instruction.Init groups)
        | input -> Instruction(pc, None, Instruction.Init groups)
        