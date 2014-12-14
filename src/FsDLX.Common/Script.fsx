
open System
open System.IO
open System.Text.RegularExpressions

let inline (@@) (a:string) (b:string) = Path.Combine(a,b)
let inline (++) (a:string) (b:string) = a + " " + b
let inline (+|+) (a:string) (b:string) = a + "|" + b

let inputdir = @"H:/FsDLX/info/"

let itypes, rtypes, jtypes = 
    inputdir @@ "Itypes",
    inputdir @@ "Rtypes",
    inputdir @@ "Jtypes"

let parseTypeFile filepath =
    let pattern = @"(?<opcode>[^\s]+)\s+(?<rrid>\d\s+)*\s*(?<encoding>\d+)"
    let regex = new Regex(pattern, RegexOptions.Multiline)
    let matches = File.ReadAllText(filepath) |> regex.Matches
    [for m in matches -> 
        m.Groups.["opcode"].Value.Trim(), 
        m.Groups.["rrid"].Value.Trim(), 
        m.Groups.["encoding"].Value.Trim()]

let getInfo = parseTypeFile
    
let getOpEncOnly =
    getInfo
    >> List.map (fun (op, _, enc) -> (op, enc))

let getPattern =
    getInfo
    >> List.map (fun (op, _, _) -> op)
    >> List.fold (+|+) ("")

let getLookupByOp =
    getOpEncOnly
    >> Map.ofList

let getLookupByEnc =
    getOpEncOnly
    >> List.map (fun (o,e) -> (int e, o))
    >> Map.ofList

let allOpEncPairs = 
    (itypes |> getOpEncOnly) @
    (rtypes |> getOpEncOnly) @
    (jtypes |> getOpEncOnly)

allOpEncPairs
|> List.map (fun (op, enc) -> sprintf "%d, %A" (int enc) op)
|> List.iter (printfn "%s")

//
//itypes 
//|> parseTypeFile
//|> List.map (fun (op, _, _) -> 
//    sprintf "| %s of string * int" (op.ToUpper()))
//|> List.map (printfn "%s")

//
//
//rtypes 
//|> parseTypeFile
//|> List.map (fun (op, _, enc) -> 
//    sprintf "| %A -> Some op" op)
//|> List.map (printfn "%s")


