open System
open System.IO
open System.Text.RegularExpressions

let itypes, rtypes, jtypes = 
    Path.Combine(__SOURCE_DIRECTORY__, @"../../support/Itypes"),
    Path.Combine(__SOURCE_DIRECTORY__, @"../../support/Rtypes"),
    Path.Combine(__SOURCE_DIRECTORY__, @"../../support/Jtypes")

type Instruction =
    | RawItype of string * int
    | RawJtype of string * int
    | RawRtype of string * int * int

let parseTypeFile fp =
    let pattern = @"(?<opcode>[^\s]+)\s+(?<alu>\d\s+)*\s*(?<encoding>\d+)"
    let regex = new Regex(pattern, RegexOptions.Multiline)
    let matches = File.ReadAllText(fp) |> regex.Matches
    [for m in matches -> 
        let opcode, alu, encoding = m.Groups.["opcode"].Value, m.Groups.["alu"].Value, m.Groups.["encoding"].Value
        fp |> function
        | _ when fp = itypes -> Instruction.RawItype(opcode, int encoding)
        | _ when fp = jtypes -> Instruction.RawJtype(opcode, int encoding)
        | _                  -> Instruction.RawRtype(opcode, int alu, int encoding) ]

//printfn "ITypes:"
//parseTypeFile itypes |> printfn "%A"
//printfn "JTypes:"
//parseTypeFile jtypes |> printfn "%A"
//printfn "RTypes:"
//parseTypeFile rtypes |> printfn "%A"


let simpleInput = Path.Combine(__SOURCE_DIRECTORY__, @"../../support/simpleInput.dlx")



type ITypeInstruction(opcode:string, rs1:string, rd:string, imm:string) =
    member ity.Opcode = opcode
    member ity.RS1 = rs1
    member ity.RD = rd
    member ity.Immediate = imm