module Test.FsDLX.Foo


open System
open System.IO
open System.Text
open System.Text.RegularExpressions

open NUnit.Framework

open FsDLX

//let matches p s = Regex(@"(?<token>" + p + ")(?<rest>.*)").Match(s).Success
//let groups p s = Regex(@"(?<token>" + p + ")(?<rest>.*)").Match(s).Groups
//
//let pitype = "addi|subi"
//let prtype = "nop|add"
//
//type Pattern(p:string) =
//    let regex = Regex(@"(?<token>" + p + ")(?<rest>.*)")
//    member pat.Match s = regex.Match(s)
//    member pat.Groups s = (pat.Match s).Groups
//
//let parseTypeFile filepath =
//    let pattern = @"(?<opcode>[^\s]+)\s+(?<rrid>\d\s+)*\s*(?<encoding>\d+)"
//    let regex = new Regex(pattern, RegexOptions.Multiline)
//    let matches = File.ReadAllText(filepath) |> regex.Matches
//    [for m in matches -> 
//        m.Groups.["opcode"].Value, 
//        m.Groups.["rrid"].Value, 
//        m.Groups.["encoding"].Value]
//
//
//type OpcodeInfo(?itypesfile:string, ?rtypesfile:string, ?jtypesfile:string) = 
//    let itypesfile, rtypesfile, jtypesfile =
//        defaultArg itypesfile (srcdir @@ @"../../support/Itypes"),
//        defaultArg rtypesfile (srcdir @@ @"../../support/Rtypes"),
//        defaultArg jtypesfile (srcdir @@ @"../../support/Jtypes")
//
//    let parseTypeFile filepath =
//        let pattern = @"(?<opcode>[^\s]+)\s+(?<rrid>\d\s+)*\s*(?<encoding>\d+)"
//        let regex = new Regex(pattern, RegexOptions.Multiline)
//        let matches = File.ReadAllText(filepath) |> regex.Matches
//        [for m in matches -> 
//            m.Groups.["opcode"].Value, 
//            m.Groups.["rrid"].Value, 
//            m.Groups.["encoding"].Value]
//
//    let getPattern fp =
//        fp |>  parseTypeFile
//        |> List.map (fun (op, _, _) -> op)
//        |> List.fold (fun r s -> r + s + "|") ("")
//
//    let getLookup fp =
//        fp |> parseTypeFile
//        |> List.map (fun (op, _, enc) -> (op, enc))
//        |> Map.ofList
//
//
//    member val ITypesPattern = getPattern itypesfile
//    member val ITypes = getLookup itypesfile
//
//    member val RTypesPattern = getPattern rtypesfile
//    member val RTypes = getLookup rtypesfile
//
//    member val JTypesPattern = getPattern jtypesfile
//    member val JTypes = getLookup jtypesfile
//
//    static member Parse filepath = filepath |> parseTypeFile
//    
//    
//
//let opcodePattern = function
//    | OpcodeInfo.Parsed info -> 
//        info |> List.map (fun (op, _, _) -> op) 
//        |> List.fold (fun r s -> r + s + "|") ("")
//    | _ -> failwith ""
//            
//
//let (|Match|_|) (p:string) =
//    let regex = Regex(@"(?<token>" + p + ")(?<rest>.*)")
//    let matches s = regex.Match(s).Success
//    let groups s = 
//        let g = regex.Match(s).Groups
//        (g.["token"].Value, g.["rest"].Value)
//    function
//    | s when s |> matches ->  s |> groups |> Some
//    | _ -> None
//        
//let (|IType|RType|JType|) (input:string) = function
//    | Match ()
//
//
//[<Test>]
//let ``tokenizer test`` () = 
//    ()