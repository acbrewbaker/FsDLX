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


module Input =
    let (|Comment|Instruction|Directive|) : string -> Choice<string, string, string> = function
        | s when s.StartsWith(";") -> Comment s
        | s when s.StartsWith("x") -> Instruction s
        | s when s.StartsWith(".") -> Directive s
        | _ -> failwith ""


module Directive =
    let (|Text|Asciiz|) : string -> Choice<string, string> = function
        | s when s.StartsWith(".text") -> 
            Text (s.Replace(".text","").Trim())
        | s when s.StartsWith(".asciiz") -> 
            Asciiz (s.Replace(".asciiz","").Trim())
        | _ -> failwith ""
    
module Instruction =
    let (|IType|RType|JType|) : string -> Choice<string, string, string> = function
        | _ -> IType ""