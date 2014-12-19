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
    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then
        srcdir @@ "../../Project2/Inputs"
    else
        srcdir @@ "../../../../Project2/Inputs"



