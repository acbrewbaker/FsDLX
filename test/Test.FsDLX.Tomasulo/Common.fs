[<AutoOpen>]
module Test.FsDLX.Tomasulo.Common

open System
open System.IO
open NCrunch.Framework


let inline (@@) (a:string) (b:string) = Path.Combine(a,b)

let srcdir = 
    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
        Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
    else 
        Environment.CurrentDirectory

let inputdir = 
    srcdir @@ "../../Project2/Inputs"



