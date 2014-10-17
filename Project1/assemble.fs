open System
open System.IO
open System.Text
open System.Text.RegularExpressions

open FsDLX.Assembler

let inline (@@) (a:string) (b:string) = Path.Combine(a,b)

let srcdir_student = "/u/css/ab67597/5483/Project1.Late/"
let srcdir_home = "X:/GitHub/FsDLX/Project1/"

[<EntryPoint>]
let main argv =
    let srcdir = 
        if argv.[0].StartsWith("-home") 
        then srcdir_home
        else srcdir_student

    let itypesfile, rtypesfile, jtypesfile =
        srcdir @@ "Itypes",
        srcdir @@ "Rtypes",
        srcdir @@ "Jtypes"

    let inputdir = srcdir @@ "Inputs"

    let inputfile = 
        if argv.[0].StartsWith("-home") 
        then inputdir @@ argv.[1]
        else inputdir @@ argv.[0]
        
    let info = new OpcodeInfo(srcdir, itypesfile, rtypesfile, jtypesfile)
    let assembler = new Assembler(inputfile, info)
    let outpath = srcdir @@ "Tests"
    assembler.Run(outpath)
    0