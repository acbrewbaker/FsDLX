open System
open System.IO
open System.Text
open System.Text.RegularExpressions

open FsDLX.Tomasulo

let inline (@@) (a:string) (b:string) = Path.Combine(a,b)

let srcdir_student = "/u/css/ab67597/5483/Project2/"
let srcdir_home = "H:/FsDLX/Project2/"

[<EntryPoint>]
let main argv =
    let inputFile, verbose =
        if      argv.Length = 2 && argv.[0] = "-f" 
        then    (argv.[1], false)
        elif    argv.Length = 3 && argv.[0] = "-f" && argv.[2] = "-v" 
        then    (argv.[1], true)
        else    failwith "Invalid program arguments"; "", false


    let simulator = new Simulator(inputFile)
    simulator.Run()
    0