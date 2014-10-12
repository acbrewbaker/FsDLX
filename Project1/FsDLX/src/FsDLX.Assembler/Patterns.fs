module FsDLX.Assembler.Patterns

open System
open System.Text
open System.Text.RegularExpressions


let comment = @"(;.*)"
let opcode = @"(?<opcode>[^\s]+\s)"
let reg = @"r\d\d?"
let freg = @"f\d\d?"
let label = @"\w+:"
let imm = @"\d+|\w+|\d+\x40[r\d\d?|\w+]\x41"


let matches (r:Regex) (s:string) = r.IsMatch(s)
let groups (r:Regex) (s:string) = r.Match(s).Groups

type R (p:P) =
    new (oi) = R(P(oi))

    member val Comments         = CommentRegexes(p.Comments)
    member val Directives       = DirectiveRegexes(p.Directives)
    member val Instructions     = InstructionRegexes(p.Instructions)
    member val Opcodes          = OpcodeRegexes(p.Opcodes)
    member val Operands         = OperandRegexes(p.Operands)
    member val Immediates       = ImmediateRegexes(p.Immediates)
    member val Labels           = LabelRegexes(p.Labels)
    member val BasePlusOffsets  = BasePlusOffsetRegexes(p.BasePlusOffsets)
    member val Registers        = RegisterRegexes(p.Registers)

and P (opcodeInfo:OpcodeInfo) =
    member val Comments         = CommentPatterns()
    member val Directives       = DirectivePatterns()
    member val Instructions     = InstructionPatterns(opcodeInfo)
    member val Opcodes          = OpcodePatterns(opcodeInfo)
    member val Operands         = OperandPatterns()
    member val Immediates       = ImmediatePatterns()
    member val Labels           = LabelPatterns()
    member val BasePlusOffsets  = BasePlusOffsetPatterns()
    member val Registers        = RegisterPatterns()

and CommentRegexes(cp:CommentPatterns) =
    member val CommentOnly = Regex(cp.CommentOnly)

and DirectiveRegexes(dp:DirectivePatterns) =
    member val Text    = Regex(dp.Text)
    member val Data    = Regex(dp.Data)
    member val Align   = Regex(dp.Align)
    member val Asciiz  = Regex(dp.Asciiz)
    member val Double  = Regex(dp.Double)
    member val Float   = Regex(dp.Float)
    member val Word    = Regex(dp.Word)
    member val Space   = Regex(dp.Space)

and InstructionRegexes(ip:InstructionPatterns) =
    member val IType    = Regex(ip.IType)
    member val RType    = Regex(ip.RType)
    member val JType    = Regex(ip.JType)
    member val Any      = Regex(ip.Any)

and OpcodeRegexes(op:OpcodePatterns) =
    member val IType    = Regex(op.IType)
    member val RType    = Regex(op.RType)
    member val JType    = Regex(op.JType)
    member val Any      = Regex(op.Any)

and OperandRegexes(op:OperandPatterns) =
    member val IType    = Regex(op.IType)
    member val RType    = Regex(op.RType)
    member val JType    = Regex(op.JType)
    member val Any      = Regex(op.Any)

and ImmediateRegexes(ip:ImmediatePatterns) =
    member val Value            = Regex(ip.Value)
    member val Label            = Regex(ip.Label)
    member val Register         = Regex(ip.Register)
    member val BasePlusOffset   = Regex(ip.BasePlusOffset)
    member val Any              = Regex(ip.Any)

and LabelRegexes(lp:LabelPatterns) =
    member val New      = Regex(lp.New)
    member val Inline   = Regex(lp.Inline)

and BasePlusOffsetRegexes(bpop:BasePlusOffsetPatterns) =
    member val ValPlusReg   = Regex(bpop.ValPlusReg)
    member val ValPlusLabel = Regex(bpop.ValPlusLabel)
    member val RegPlusVal   = Regex(bpop.RegPlusVal)
    member val RegPlusLabel = Regex(bpop.RegPlusLabel)
    member val Any          = Regex(bpop.Any)

and RegisterRegexes(rp:RegisterPatterns) =
    member val R = Regex(rp.R)
    member val F = Regex(rp.F)

and CommentPatterns() =
    member val CommentOnly = ""

and DirectivePatterns() =
    member val Text    = (@"(?<=\.(text).*)(\d+)")
    member val Data    = (@"(?<=\.(data).*)(\d+)")
    member val Align   = (@"(?<=\.(align).*)(\d)")
    member val Asciiz  = (@"(?<=\.(asciiz).*)?""([^""]+)""")
    member val Double  = (@"(?<=\.(double).*)([+-]?\d*\.\d+)")
    member val Float   = (@"(?<=\.(float).*)([+-]?\d*\.\d+)")
    member val Word    = (@"(?<=\.(word).*)((-)?(0x)?\d+)")
    member val Space   = (@"(?<=\.(space).*)(\d+)")

and InstructionPatterns(op:OpcodePatterns, ops:OperandPatterns) =
    new(opcodeInfo:OpcodeInfo) = 
        InstructionPatterns(OpcodePatterns(opcodeInfo), OperandPatterns())
    
    member val IType    = op.IType + ops.IType
    member val RType    = op.RType + ops.RType
    member val JType    = op.JType + ops.JType
    member val Any      = op.Any + ops.Any

and OpcodePatterns(info:OpcodeInfo) =
    let itype, rtype, jtype =
        info.ITypes.Pattern,
        info.RTypes.Pattern,
        info.JTypes.Pattern

    member val IType = itype
    member val RType = rtype
    member val JType = jtype
    member val Any = itype + "|" + rtype + "|" + jtype

and OperandPatterns() =
    let immPat = ImmediatePatterns()
    let rrr = @"(?<rs1>r\d\d?), (?<rs2>r\d\d?), (?<rd>r\d\d?)"
    let fff = @"(?<rs1>f\d\d?), (?<rs2>f\d\d?), (?<rd>f\d\d?)"
    
    let itype, rtype, jtype =
        @"(?<rs1>r\d\d?), (?<rd>r\d\d?)" + immPat.Any,
        rrr + "|" + fff,
        immPat.Any

    member val IType = itype
    member val RType = rtype
    member val JType = jtype
    member val Any = itype + "|" + rtype + "|" + jtype

and ImmediatePatterns() =
    let v, l, r, bpo =
        "(?\d+)", LabelPatterns().Inline, RegisterPatterns().R, BasePlusOffsetPatterns()
    member val Value            = v
    member val Label            = l
    member val Register         = r
    member val BasePlusOffset   = bpo.Any
    member val Any = v + "|" + l + "|" + r + "|" + bpo.Any

and LabelPatterns() =
    member val New = @"(?<=(\w+):.*)(\w+)"
    member val Inline = @"(?[a-zA-Z]+\d*)"

and BasePlusOffsetPatterns() =
    let regpat = RegisterPatterns()
    let lblpat = LabelPatterns()
    let vpr, vpl, rpv, rpl =
        "(?\d)\((?" + regpat.R + ")\)",
        "(?\d)\((?" + lblpat.Inline + ")\)",
        "(?" + regpat.R + ")\((?\d+)\)",
        "(?" + regpat.R + ")\((?" + lblpat.Inline + ")\)"

    member val ValPlusReg   = vpr
    member val ValPlusLabel = vpl
    member val RegPlusVal   = rpv
    member val RegPlusLabel = rpl
    member val Any = vpr + "|" + vpl + "|" + rpv + "|" + rpl

and RegisterPatterns() =
    member val R = @"r\d\d?"
    member val F = @"f\d\d?"


let (|Comment|_|) (encoder:DLXEncoder) : CommentRegexes * string -> string option =
    let comment r s = let g = (groups r s).[1].Value in (g,g)
    let encode r s = s |> comment r ||> encoder.Encode |> Some
    function
    | r, s when s |> matches r.CommentOnly -> s |> encode r.CommentOnly
    | _ -> None

let (|Directive|_|) (encoder:DLXEncoder) : DirectiveRegexes * string -> string option = 
    let directive r s = let g = groups r s in (g.[1].Value, g.[2].Value)
    let encode r s = s |> directive r ||> encoder.Encode |> Some
    function
    | r, s when s |> matches r.Text    -> s |> encode r.Text
    | r, s when s |> matches r.Data    -> s |> encode r.Data
    | r, s when s |> matches r.Align   -> s |> encode r.Align
    | r, s when s |> matches r.Asciiz  -> s |> encode r.Asciiz
    | r, s when s |> matches r.Double  -> s |> encode r.Double
    | r, s when s |> matches r.Float   -> s |> encode r.Float
    | r, s when s |> matches r.Word    -> s |> encode r.Word
    | r, s when s |> matches r.Space   -> s |> encode r.Space
    | _ -> None

let (|Instruction|_|) (encoder:DLXEncoder) : InstructionRegexes * string -> string option =
    let instruction r s = let g = groups r s in (g.[1].Value, g.[1].Value)
    let encode r s = s |> instruction r ||> encoder.Encode |> Some
    function
    | _ -> None

let (|Opcode|_|) (encoder:DLXEncoder) : OpcodeRegexes * string -> string option =
    let opcode r s = let g = groups r s in (g.[1].Value, g.[1].Value)
    let encode r s = s |> opcode r ||> encoder.Encode |> Some
    function
    | _ -> None

let (|Operapnds|_|) (encoder:DLXEncoder) : OperandRegexes * string -> string option =
    let opcode r s = let g = groups r s in (g.[1].Value, g.[1].Value)
    let encode r s = s |> opcode r ||> encoder.Encode |> Some
    function
    | _ -> None

let (|Immediate|_|) (encoder:DLXEncoder) : ImmediateRegexes * string -> string option =
    let immediate r s = let g = groups r s in (g.[1].Value, g.[1].Value)
    let encode r s = s |> immediate r ||> encoder.Encode |> Some
    function
    | r, s when s |> matches r.Value            -> s |> encode r.Value
    | r, s when s |> matches r.Label            -> s |> encode r.Label
    | r, s when s |> matches r.Register         -> s |> encode r.Register
    | r, s when s |> matches r.BasePlusOffset   -> s |> encode r.BasePlusOffset
    | _ -> None

let (|Label|_|) (encoder:DLXEncoder) : LabelRegexes * string -> string option =
    let label r s = let g = groups r s in (g.[1].Value, g.[1].Value)
    let encode r s = s |> label r ||> encoder.Encode |> Some
    function
    | r, s when s |> matches r.New -> s |> encode r.New
    | r, s when s |> matches r.Inline -> s |> encode r.Inline
    | _ -> None

let (|BasePlusOffset|_|) (encoder:DLXEncoder) : BasePlusOffsetRegexes * string -> string option =
    let opcode r s = let g = groups r s in (g.[1].Value, g.[1].Value)
    let encode r s = s |> opcode r ||> encoder.Encode |> Some
    function
    | _ -> None

let (|Register|_|) (encoder:DLXEncoder) : RegisterRegexes * string -> string option =
    let register r s = let g = (groups r s).[1].Value in (g,g)
    let encode r s = s |> register r ||> encoder.Encode |> Some
    function
    | r, s when s |> matches r.R -> s |> encode r.R
    | r, s when s |> matches r.F -> s |> encode r.F
    | _ -> None

//module Register =
//    let (|R|F|) : string -> Choice<string, string> =
//        function
//        | s when s.StartsWith("r") -> R s
//        | s when s.StartsWith("f") -> F s
//        | _ -> failwith "" 
//       
//module BasePlusOffset =
//    let (|ValPlusLabel|ValPlusReg|) : string -> Choice<string*string, string*string> =
//        let basePlusOffset = Regex(@"(?<base>\d)\((?<offset>.*)\)")
//        let f s = 
//            let g = basePlusOffset.Match(s).Groups
//            (g.["base"].Value, g.["offset"].Value)
//        function
//        | s when basePlusOffset.IsMatch(s) -> 
//            let b, offset = f s
//            offset.StartsWith("r") |> function | true -> ValPlusReg (b, offset) | _ -> ValPlusLabel (b, offset)
//        | _ -> failwith "" 
//
//module Immediate =
//    let (|Value|Label|Register|BasePlusOffset|) : string -> Choice<string, string, string, string*string> =
//        let value = Regex(@"(?<value>\d+)")
//        let label = Regex(@"(?<label>[a-zA-Z]+\d*)")
//        let matches (r:Regex) (s:string) = r.IsMatch(s) 
//        function
//        | s when s |> matches value -> Value (value.Match(s).Groups.["value"].Value)
//        | s when s |> matches label -> Label (label.Match(s).Groups.["label"].Value)
//        | BasePlusOffset.ValPlusLabel(b,o) -> BasePlusOffset(b,o)
//        | _ -> failwith ""
//
//module Instruction =
//    let (|IType|RType|JType|) : string*string -> Choice<string*string[], string*string[], string*string[]> =
//        function
//        | op, ops -> IType ("", [|""|])

//
//let (|Input|Comment|) (str:string) = str.Split(';').Length |> function | 0 | 1 -> Comment | _ -> Input
//
//
//module Label =
//    module private DlxRegex =
//        let newlabel = Regex(@"(?<=(\w+):.*)(\w+)")
//        
//    let matchLabel (regex:Regex) input =
//        let matches = regex.Matches(input)
//        ([for m in matches -> m.Groups.["label"].Value], matches.Count > 0)
//
//    let (|NewLabel|_|) (symbolTable:Map<string, string>) (pc:uint32) (hex:string list) input =
//        matchLabel DlxRegex.newlabel input |> function
//        | matches, true -> 
//            (symbolTable.Add(matches.Head, Conversions.pc2hex pc), pc, hex) |> Some
//        | _ -> None
//    
//    let (|New|Inline|) label =
//        
//
//module Directive =
//    module private DlxRegex =
//        let text    = Regex(@"(?<=\.(text).*)(\d+)")
//        let data    = Regex(@"(?<=\.(data).*)(\d+)")
//        let align   = Regex(@"(?<=\.(align).*)(\d)")
//        let asciiz  = Regex(@"(?<=\.(asciiz).*)?""([^""]+)""")
//        let double  = Regex(@"(?<=\.(double).*)([+-]?\d*\.\d+)")
//        let float   = Regex(@"(?<=\.(float).*)([+-]?\d*\.\d+)")
//        let word    = Regex(@"(?<=\.(word).*)((-)?(0x)?\d+)")
//        let space   = Regex(@"(?<=\.(space).*)(\d+)")
//        
//    let matchDirective (regex:Regex) (input:string) = 
//        let matches = regex.Matches(input)
//        ([for m in matches -> m.Groups.[2].Value], matches.Count > 0)
//
//    let (|Text|_|) (pc:uint32) (hex:string list) input =
//        matchDirective DlxRegex.text input |> function
//        | matches, true -> Conversions.Directive.dlx2hex ".text" matches (pc, hex) |> Some
//        | _ -> None
//
//    let (|Data|_|) (pc:uint32) (hex:string list) input =
//        matchDirective DlxRegex.data input |> function
//        | matches, true -> Conversions.Directive.dlx2hex ".data" matches (pc, hex) |> Some
//        | _ -> None
//
//    let (|Align|_|) (pc:uint32) (hex:string list) input =
//        matchDirective DlxRegex.align input |> function
//        | matches, true -> Conversions.Directive.dlx2hex ".align" matches (pc, hex) |> Some
//        | _ -> None
//
//    let (|Asciiz|_|) (pc:uint32) (hex:string list) input =
//        matchDirective DlxRegex.asciiz input |> function
//        | matches, true -> Conversions.Directive.dlx2hex ".asciiz" matches (pc, hex) |> Some
//        | _ -> None
//
//    let (|Double|_|) (pc:uint32) (hex:string list) input =
//        matchDirective DlxRegex.double input |> function
//        | matches, true -> Conversions.Directive.dlx2hex ".double" matches (pc, hex) |> Some
//        | _ -> None
//
//    let (|Float|_|) (pc:uint32) (hex:string list) input =
//        matchDirective DlxRegex.float input |> function
//        | matches, true -> Conversions.Directive.dlx2hex ".float" matches (pc, hex) |> Some
//        | _ -> None
//
//    let (|Word|_|) (pc:uint32) (hex:string list) input =
//        matchDirective DlxRegex.word input |> function
//        | matches, true -> Conversions.Directive.dlx2hex ".word" matches (pc, hex) |> Some
//        | _ -> None
//
//    let (|Space|_|) (pc:uint32) (hex:string list) input =
//        matchDirective DlxRegex.space input |> function
//        | matches, true -> Conversions.Directive.dlx2hex ".space" matches (pc, hex) |> Some
//        | _ -> None
//
//
//module Instruction =
//    let opcodes  = Support.Opcodes()
//        
//    module private DlxRegex =
//        let itype = @"beqz|bnez|addi|addui|subi|subui|andi|ori|xori|lhi|trap|jr|jalr|slli|srli|srai|seqi|snei|slti|sgti|slei|sgei|lb|lh|lw|lbu|lhu|lf|ld|sb|sh|sw|sf|sd"
//        let rtype = @"nop|sll|srl|sra|add|addu|sub|subu|and|or|xor|seq|sne|slt|sgt|sle|sge|movf|movd|movfp2i|movi2fp|addf|subf|multf|divf|addd|subd|multd|divd|cvtf2d|cvtf2i|cvtd2f|cvtd2i|cvti2f|cvti2d|mult|div|multu|divu"
//        let jtype = @"j|jal"            
//
//    module Operands =
//        module Immediate =
//            let immLabel = @"(,? )(?<label>[a-zA-Z]+\d*)"
//            let immReg = @"(,? )(?<register>r\d\d?)"
//            let immVal = @"(,? )(?<val>\d+)"
//            let immBasePlusOffset = "(,? )(?<base>\d)\((?<offset>.*)\)"
//                
//            let any = @"(?<imm>" + immLabel + "|" + immReg + "|" + immVal + "|" + immBasePlusOffset + ")"
//            
//
//        let rrr = @"(?<rs1>r\d\d?), (?<rs2>r\d\d?), (?<rd>r\d\d?)"
//        let fff = @"(?<rs1>f\d\d?), (?<rs2>f\d\d?), (?<rd>f\d\d?)"
//        let rri = @"(?<rs1>r\d\d?), (?<rd>r\d\d?)" + Immediate.any
//
//            
//        let itype = @"(?<operands>" + rri + ")"
//        let rtype = @"(?<operands>" + rrr + "|" + fff + ")"
//        let jtype = @"(?<operands>" + Immediate.any + ")"
//        let any = @"(?<operands>" + rrr + "|" + fff + "|" + rri + ")"
//
//    let operands = Operands.any
//
//    let itype = comment + "|" + @"(?<=(?<itype>" + DlxRegex.itype + ").*)" + Operands.itype + "|" + comment + "$"
//    let rtype = comment + "|" + @"(?<=(?<rtype>" + DlxRegex.rtype + ").*)" + Operands.rtype + "|" + comment + "$"
//    let jtype = comment + "|" + @"(?<=(?<jtype>" + DlxRegex.jtype + ").*)" + Operands.jtype + "|" + comment + "$"
//
//    let matchIType input =
//        let opcode, operands = [
//            for m in Regex(itype).Matches(input) -> 
//                m.Groups.["itype"].Value, m.Groups] |> List.unzip
//        (opcode.Head, operands, opcode.Length <> 0)
//
//    let matchRType input =
//        let opcode, operands = [
//            for m in Regex(rtype).Matches(input) -> 
//                m.Groups.["rtype"].Value, m.Groups.["operands"].Value] |> List.unzip
//        (opcode.Head, operands, opcode.Length <> 0)
//
//    let matchJType input =
//        let opcode, operands = [
//            for m in Regex(jtype).Matches(input) -> 
//                m.Groups.["jtype"].Value, m.Groups.["operands"].Value] |> List.unzip
//        (opcode.Head, operands, opcode.Length <> 0)
//
//
//        
//    let (|IType|_|) (pc:uint32) (hex:string list) input =
//        matchIType input |> function
//        | opcode, operands, true -> 
//            operands.Head |> printfn "%A"
//            Conversions.Instruction.itype pc opcode operands.Head
////                let ops = [for g in operands -> g.]
//            Conversions.Instruction.dlx2hex opcode [""] (pc, hex) |> Some
//        | _ -> None
//
//    let (|RType|_|) (pc:uint32) (hex:string list) input =
//        matchRType input |> function
//        | opcode, operands, true -> Conversions.Instruction.dlx2hex opcode operands (pc, hex) |> Some
//        | _ -> None
//
//    let (|JType|_|) (pc:uint32) (hex:string list) input =
//        matchIType input |> function
//        | opcode, operands, true -> Conversions.Instruction.dlx2hex opcode [""] (pc, hex) |> Some
//        | _ -> None
//
//let (|Label|_|) symbolTable pc hex = function
//    | Label.NewLabel symbolTable pc hex result -> Some result
//    | _ -> None
//
//
//let (|Directive|_|) pc hex = function
//    | Directive.Text pc hex result -> Some result
//    | Directive.Data pc hex result -> Some result
//    | Directive.Align pc hex result -> Some result
//    | Directive.Asciiz pc hex result -> Some result
//    | Directive.Double pc hex result -> Some result
//    | Directive.Float pc hex result -> Some result
//    | Directive.Word pc hex result -> Some result
//    | Directive.Space pc hex result -> Some result
//    | _ -> None
//        
//
//let (|Instruction|_|) pc hex = function
//    | Instruction.IType pc hex result -> Some result
//    | Instruction.RType pc hex result -> Some result
//    | Instruction.JType pc hex result -> Some result
//    | _ -> None
//
//


