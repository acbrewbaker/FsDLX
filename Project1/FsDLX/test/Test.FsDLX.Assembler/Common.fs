[<AutoOpen>]
module Test.FsDLX.Common

open System
open System.IO
open System.Text.RegularExpressions
open NUnit.Framework
open NCrunch.Framework

open FsDLX

let dlxfiles = Directory.GetFiles(Support.inputdir) |> Array.filter(fun f -> f.EndsWith(".dlx"))
let hexfiles = Directory.GetFiles(Support.inputdir) |> Array.filter(fun f -> f.EndsWith(".hex"))


//    module DLX =
//        let aligns = dlxfiles |> Array.filter (fun f -> f.StartsWith("align"))
//        let arithImmeds = dlxfiles |> Array.filter (fun f -> f.StartsWith("arithImmed"))
//        let asciizs = dlxfiles |> Array.filter (fun f -> f.StartsWith("asciiz"))
//        let branches = dlxfiles |> Array.filter (fun f -> f.StartsWith("branches"))
//        let arithImmeds = dlxfiles |> Array.filter (fun f -> f.StartsWith("convert"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("data"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("directives"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("double"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("float"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("fpArith"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("intArith"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("intLogical"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("intSets"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("intShift"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("jump"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("loadImmed"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("loads"))
//        let arithImmed = dlxfiles |> Array.filter (fun f -> f.StartsWith("logicalImmed"))

//let srcdir = 
//    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
//        Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
//    else 
//        Environment.CurrentDirectory
//
//let itypes, rtypes, jtypes = 
//    Path.Combine(srcdir, @"../../support/Itypes"),
//    Path.Combine(srcdir, @"../../support/Rtypes"),
//    Path.Combine(srcdir, @"../../support/Jtypes")
//
//type Instruction =
//    | RawItype of string * int
//    | RawJtype of string * int
//    | RawRtype of string * int * int
//
//let parseTypeFile fp =
//    let pattern = @"(?<opcode>[^\s]+)\s+(?<alu>\d\s+)*\s*(?<encoding>\d+)"
//    let regex = new Regex(pattern, RegexOptions.Multiline)
//    let matches = File.ReadAllText(fp) |> regex.Matches
//    [for m in matches -> 
//        let opcode, alu, encoding = m.Groups.["opcode"].Value, m.Groups.["alu"].Value, m.Groups.["encoding"].Value
//        fp |> function
//        | _ when fp = itypes -> Instruction.RawItype(opcode, int encoding)
//        | _ when fp = jtypes -> Instruction.RawJtype(opcode, int encoding)
//        | _                  -> Instruction.RawRtype(opcode, int alu, int encoding) ]

//type Instruction<'Opcode, 'Register, 'Immediate> =
//    | IType of 'Opcode * 'Register * 'Register * 'Immediate
//    | RType of int * 'Register * 'Register * 'Register * 'Opcode
//    | JType of 'Opcode * 'Immediate 
//
//    member ins.Encode = (ins, typeof<'Opcode>) |> function
//        | IType(opcode, rs1, rd, imm), ty -> function
//            | _ when ty = typeof<string> -> ()
//            | _ -> failwith ""
//            
//        | RType(rru, rs1, rs2, rd, func), ty -> ()
//        | JType(opcode, name), ty -> ()


//type Instruction =
//    | IType of string * string * string * string
//    | RType of int * string * string * string * string
//    | JType of string * string
//
////    static member Invoke(symbolTable:Map<string, Instruction -> unit>) = f |> function
////        | IType(_) -> ins.asBinaryString |> f
////        | RType(_) -> ins.asBinaryString |> f
////        | JType(_) -> ins.asBinaryString |> f
//
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
//
//    member ins.asBinaryString = ins.Encode
//    member ins.asUInt32 = Convert.ToUInt32(ins.Encode, 2)
//    member ins.asHexString = ins.asUInt32.ToString("x8")
//
//    override ins.ToString() = ins.asHexString

//let ins = Instruction.IType("0", "0", "0", "0", null)

//type ITypeInstruction<'Opcode, 'Register, 'Immediate> =
//    abstract Opcode : 'Opcode
//    abstract RS1    : 'Register
//    abstract RD     : 'Register
//    abstract Imm    : 'Immediate
//
//type RTypeInstruction<'Opcode, 'Register> =
//    abstract RRKind : int
//    abstract RS1    : 'Register
//    abstract RS2    : 'Register
//    abstract RD     : 'Register
//    abstract Func   : 'Opcode
//
//type JTypeInstruction<'Opcode, 'Immediate> =
//    abstract Opcode : 'Opcode
//    abstract Name   : 'Immediate

//type ITypeInstruction2() =
//    member val Opcode   = "" with get, set
//    member val RS1      = "" with get, set
//    member val RD       = "" with get, set
//    member val Imm      = "" with get, set
////
//type Opcode =
//    | Raw of string
//    | Num of uint32
//
//    member o.GetNum = 
//        let zero4 = "0000"
//        let zero26 = "00000000000000000000000000"
//        o |> function
//        | Raw(opcode) ->
//            opcode.Length |> function 
//            | 2 -> zero4 + opcode + zero26 |> UInt32.Parse
//            | 1 -> "0" + zero4 + opcode + zero26 |> UInt32.Parse
//            | _ -> failwith "invalid opcode"
//        | Num(opcode) -> opcode

