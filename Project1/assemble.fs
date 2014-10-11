open System
open System.IO
open System.Text
open System.Text.RegularExpressions

let inline (@@) (a:string) (b:string) = Path.Combine(a,b)
let srcdir = "/u/css/ab67597/5483/Project1"
let inputdir = srcdir @@ "Inputs"

let itypesfile, rtypesfile, jtypesfile =
    srcdir @@ "Itypes",
    srcdir @@ "Rtypes",
    srcdir @@ "Jtypes"

let itypes = itypesfile |> File.ReadAllText
let rtypes = rtypesfile |> File.ReadAllText
let jtypes = jtypesfile |> File.ReadAllText

[<EntryPoint>]
let main argv =
    let inputfile = inputdir @@ argv.[0]
    let input = inputfile |> File.ReadAllText
    printfn "%s" input
    //printfn "%A" argv
    0