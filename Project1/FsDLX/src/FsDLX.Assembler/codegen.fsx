open System
open System.IO
open System.Text
open System.Text.RegularExpressions

let inline (@@) (a:string) (b:string) = Path.Combine(a,b)

let srcdir = __SOURCE_DIRECTORY__
let inputdir = srcdir @@ "Inputs"
let supportdir = srcdir @@ "../"
let itypesfile, rtypesfile, jtypesfile =
    srcdir @@ "../Itypes",
    srcdir @@ "../Rtypes",
    srcdir @@ "../Jtypes"

let parseTypeFile filepath =
    let pattern = @"(?<opcode>[^\s]+)\s+(?<rrid>\d\s+)*\s*(?<encoding>\d+)"
    let regex = new Regex(pattern, RegexOptions.Multiline)
    let matches = File.ReadAllText(filepath) |> regex.Matches
    [for m in matches -> 
        m.Groups.["opcode"].Value, 
        m.Groups.["rrid"].Value, 
        m.Groups.["encoding"].Value]

let directives = [ ".text"; ".data"; ".align"; ".asciiz"; ".double"; ".float"; ".word"; ".space"]

let genMatchFunc (name:string) (pattern:string) = 
    sprintf "
let matches%s (s:string) = let r = Regex(\"%s\") in r.IsMatch(s)"
        (name.ToUpper()) pattern

let genEncodeFunc name =
    sprintf "
let encode%s (s:string ref) (f:string -> string) = function
	| s when s |> matches%s -> f s
	| _ -> s" name


itypesfile |> parseTypeFile |> List.map (fun (op, rrx, enc) -> genMatchFunc op op) |> List.iter (printfn "%s")