open System
open System.IO
open System.Text
open System.Text.RegularExpressions

open FsDLX.Assembler

let inline (@@) (a:string) (b:string) = Path.Combine(a,b)

let inputdir = "/u/css/ab67597/5483/Project1/Inputs"

[<EntryPoint>]
let main argv =
    let inputfile = inputdir @@ argv.[0]
    let assembler = new Assembler(inputfile)
    assembler.Run()
    0