[<AutoOpen>]
module FsDLX.Assembler.Common

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

let inline (@@) (a:string) (b:string) = Path.Combine(a, b)
let inline (++) (a:string) (b:string) = a + " " + b
let inline (+|+) (a:string) (b:string) = a + "|" + b

let matches (r:Regex) s = r.IsMatch(s)

let asComment str = "    \t# " + str

let addLeadingZero (str:string) =
    str |> function
    | _ when str.StartsWith("-.") -> str.Insert(str.IndexOf("-.") + 1, "0")
    | _ when str.StartsWith(".") -> str.Insert(str.IndexOf("."), "0")
    | _ -> str

let floatingPointAsComment = addLeadingZero >> asComment

let strAsComment str = "\t#\"" + str + "\""


