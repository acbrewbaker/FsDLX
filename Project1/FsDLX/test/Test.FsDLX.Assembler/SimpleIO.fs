module Test.FsDLX.SimpleIO

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open NUnit.Framework
open NCrunch.Framework
open FsUnit

open FsDLX
open Support


[<Test>]
let ``parse support files`` () =
    let parseTypeFile filepath =
        let pattern = @"(?<opcode>[^\s]+)\s+(?<rrid>\d\s+)*\s*(?<encoding>\d+)"
        let regex = new Regex(pattern, RegexOptions.Multiline)
        let matches = File.ReadAllText(filepath) |> regex.Matches
        [for m in matches -> 
            m.Groups.["opcode"].Value, 
            m.Groups.["rrid"].Value, 
            m.Groups.["encoding"].Value]

    printfn "ITypes:"
    parseTypeFile itypesfile |> printfn "%A"
    printfn "JTypes:"
    parseTypeFile jtypesfile |> printfn "%A"
    printfn "RTypes:"
    parseTypeFile rtypesfile |> printfn "%A"

[<Test>]
let ``parse input files`` () =
    let dlxfiles = Directory.GetFiles(inputdir) |> Array.filter(fun f -> f.EndsWith(".dlx"))
    printfn "%A" dlxfiles
    let hexfiles = Directory.GetFiles(inputdir) |> Array.filter(fun f -> f.EndsWith(".hex"))
    printfn "%A" hexfiles

[<Test>]
let ``map support files`` () =
    let opcodes = Opcodes()

    printfn "%A" opcodes
    
    printfn "ITYPE INSTRUCTIONS"
    opcodes.ITypes
    |> Map.iter ( fun k v -> printfn "\"%s\", fun (rs1:Register) _ (rd:Register) (imm:Immediate) -> ()" k)

    printfn "RTYPE INSTRUCTIONS"
    opcodes.RTypes
    |> Map.iter ( fun k v -> printfn "\"%s\", fun (rs1:Register) (rs2:Register) (rd:Register) _ -> ()" k)

    printfn "JTYPE INSTRUCTIONS"
    opcodes.JTypes
    |> Map.iter ( fun k v -> printfn "\"%s\", fun _ _ _ (name:Immediate) -> ()" k)

[<Test>]
let ``pad test`` () =
    let regs = [0..31]
    let p x = printfn "%A" x
    regs |> p
    regs |> List.iter (fun r -> Convert.ToString(r, 2).PadLeft(5, '0') |> p)
    
