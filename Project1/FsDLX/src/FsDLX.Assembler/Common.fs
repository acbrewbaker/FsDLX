[<AutoOpen>]
module FsDLX.Common

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open NCrunch.Framework

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

            

module Patterns =
    let comment = @"(?<comment>.+;.*)"
    let opcode = @"(?<opcode>[^\s]+\s)"
    let reg = @"r\d\d?"
    let freg = @"f\d\d?"
    let label = @"\w+:"
    let imm = @"\d+|\w+|\d+\x40r\d\d?|\w+\x41"

    module Directive =
        let text = @".text (?<text>\d+)?"
        let data = @".data (?<data>\d+)?"
        let align = @".align (?<align>\d+)"
        let asciiz = @".asciiz (?<asciiz>"".*""+)"
        let double = @".double (?<double>\d*.\d+)+"
        let float = @".float (?<float>\d*.\d+)+"
        let word = @".word (?<word>[+-]?\d+,? ?)+"
        let space = @".space (?<space>\d+){1}"
        let directive = @".(text|data|align|asciiz|double|float|word|space)"

    module Opcode =
        let itype = @"beqz|bnez|addi|addui|subi|subui|andi|ori|xori|lhi|trap|jr|jalr|slli|srli|srai|seqi|snei|slti|sgti|slei|sgei|lb|lh|lw|lbu|lhu|lf|ld|sb|sh|sw|sf|sd"
        let rtype = @"nop|sll|srl|sra|add|addu|sub|subu|and|or|xor|seq|sne|slt|sgt|sle|sge|movf|movd|movfp2i|movi2fp|addf|subf|multf|divf|addd|subd|multd|divd|cvtf2d|cvtf2i|cvtd2f|cvtd2i|cvti2f|cvti2d|mult|div|multu|divu"
        let jtype = @"j|jal"

        let itypecg = @"(?<itype>" + itype + @")"
        let rtypecg = @"(?<rtype>" + rtype + @")"
        let jtypecg = @"(?<jtype>" + jtype + @")"

    module Operands =
        module Immediate =
            let ty1 = @"\d+"
            let ty2 = @"\d+\x40\d+\x41"
            let ty3 = @"\d+\x40" + reg + @"\x41"
            let ty4 = @"\d+\x40\w+\x41"
            let ty5 = @"\w+"
            //let any = @"(?<imm>" + imm + @")"
            let any = @"(?<imm>[" + ty1 + "|" + ty2 + "|" + ty3 + "|" + ty4 + "|" + ty5 + "])"

        let rrr = @"(?<rd>" + reg + "), (?<rs1>" + reg + "), (?<rs2>" + reg + ")"
        let fff = @"(?<rd>" + freg + "), (?<rs1>" + freg + "), (?<rs2>" + freg + ")"
        let rri = @"(?<rd>" + reg + "), (?<rs1>" + reg + "), " + Immediate.any //"(?<imm>" + imm + ")" //Immediate.any

        let any = @"(?<operands>" + rrr + "|" + fff + "|" + rri + ")"

    let instruction = 
        
        @"(?<opcode>" + Opcode.itypecg + "|" + Opcode.rtypecg + "|" + Opcode.jtypecg + @"])\s" + Operands.any

module Conversions =
    module DLX2HEX =
        let text _ = ""
        let data _ = ""
        let align (str:string) (pc:uint32) =
            let bits = uint32 str
            ((((pc - bits) ||| 32u) + bits) - pc
            |> BitConverter.GetBytes
            |> Array.rev
            |> BitConverter.ToString)
                .Replace("-", "")
                .ToLower()

        let asciiz (pc:uint32) (str:string) =
            let bytes       = str |> Encoding.Default.GetBytes
            let newpc       = pc + uint32 bytes.Length
            let hex         = BitConverter.ToString(bytes).Replace("-","").ToLower() + "00"
            let newstr      = sprintf "%s: %s #\"%s\"" (pc.ToString("x8")) hex str
            (newpc, newstr)

        let double (str:string) =
            (Double.Parse(str)
            |> BitConverter.GetBytes
            |> Array.rev
            |> BitConverter.ToString)
                .Replace("-", "")
                .ToLower()

        let float _ = ""
        let word _ = ""
        let space _ = ""

        let outputLine (pc:uint32) (hex:string) (str:string) =
            sprintf "%s: %s\t #%s"
                (pc.ToString("x8"))
                hex
                str


let unused = "000000"

type Opcode =
    | String of string
    | Val of uint32

//type Opcode(name:string, code:int) =
//    member val Name = name with get, set
//    member val Code = code with get, set


type RegisterT(?value:uint32, ?displayInBinary:bool) =
    let mutable value = defaultArg value 0u
    let displayInBinary = defaultArg displayInBinary false
    let bytes = BitConverter.GetBytes(value)
    member r.Byte0 = bytes.[0]
    member r.Byte1 = bytes.[1]
    member r.Byte2 = bytes.[2]
    member r.Byte3 = bytes.[3]
    member r.Value
        with get() = value
        and set(v) = value <- v
    member r.Bit b = r.Value &&& (1u <<< b)
    override r.ToString() = 
        if displayInBinary then r.Value.ToString("2")
        else r.Value.ToString("x8")

type Register =
    | String of string
    | Val of RegisterT

    member r.Set(v) = r |> function
        | String rid -> ()
        | Val reg -> reg.Value <- v


type Immediate =
    | String of string
    | Val of uint32

type RRid =
    | String of string
    | Val of int

type Instruction =
    | IType of Opcode * Register * Register * Immediate
    | RType of RRid * Register * Register * Register * Opcode
    | JType of Opcode * Immediate

//    member private ins.Encode = ins |> function
//        | IType(opcode, rs1, rd, imm) ->
//            opcode  .PadLeft(6, '0') + 
//            rs1     .PadLeft(5, '0') + 
//            rd      .PadLeft(5, '0') + 
//            imm     .PadLeft(16, '0')
//            
//        | RType(rru, rs1, rs2, rd, func) ->
//            (string rru)    .PadLeft(6, '0') +
//            rs1             .PadLeft(5, '0') +
//            rs2             .PadLeft(5, '0') +
//            rd              .PadLeft(5, '0') +
//            unused                           +
//            func            .PadLeft(5, '0')
//        | JType(opcode, name) ->
//            opcode  .PadLeft(6, '0') +
//            name    .PadLeft(26, '0')

//    member ins.asBinaryString = ins.Encode
//    member ins.asUInt32 = Convert.ToUInt32(ins.Encode, 2)
//    member ins.asHexString = ins.asUInt32.ToString("x8")
//
//    override ins.ToString() = ins.asHexString

type InstructionFunction = Register -> Register -> Register -> Immediate -> unit




//type Register =
//    | GPR of uint32
//    | SPR of float32
//
//    member r.Byte0 = r |> function
//        | GPR(value) -> BitConverter.GetBytes(value).[0]
//        | SPR(value) -> BitConverter.GetBytes(value).[0]
//
//    member r.Byte1 = r |> function
//        | GPR(value) -> BitConverter.GetBytes(value).[1]
//        | SPR(value) -> BitConverter.GetBytes(value).[1]
// 
//    member r.Byte2 = r |> function
//        | GPR(value) -> BitConverter.GetBytes(value).[2]
//        | SPR(value) -> BitConverter.GetBytes(value).[2]
//
//    member r.Byte3 = r |> function
//        | GPR(value) -> BitConverter.GetBytes(value).[3]
//        | SPR(value) -> BitConverter.GetBytes(value).[3]
//
//    override r.ToString() = r |> function
//        | GPR(value) -> value.ToString("x8")
//        | SPR(value) -> value.ToString("x8")

//type GeneralPurposeRegisters() =
//    let gpr = 
//        Array.init 32 (fun i -> ("r" + string i, Register.GPR(0u)))
//        |> Map.ofArray
//
//    member this.R i = gpr.["r" + string i]
//    member this.SetR i v = gpr.["r" + string i] <- Register.GPR(v)
//
