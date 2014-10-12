module FsDLX.Assembler.Interface

open System
open System.IO
open System.Text.RegularExpressions

let output pc hex comment = sprintf "%s: %s #%s" pc hex comment

let (|Comment|Directive|Instruction|) (s:string) : Choice<string, string*string, string*string> =
    let s = s.Trim()
    let comment = Regex(@"(^;)(.*)")
    let directive = Regex(@"(^\.)(.*)")
    let instruction = Regex(@"(^\w+) (.*)")
    let matches (r:Regex) s = r.IsMatch(s)
    let groups (r:Regex) s = let g = r.Match(s).Groups in (g.[0].Value, g.[1].Value)
    s |> function
    | _ when s |> matches comment -> Comment (groups comment s |> snd)
    | _ when s |> matches directive -> Directive (groups directive s)
    | _ when s |> matches instruction -> Instruction (groups instruction s)
    | _ -> failwith "Unable to match input!"

type Assembler(dlxin:string) =
    do dlxin |> function
    | _ when not (dlxin.EndsWith(".dlx")) -> failwith "invalid input file"
    | _ -> ()

    let hexout = dlxin.Split('.').[0] + ".hex"

    let assemble() = 
        dlxin |> File.ReadAllLines 
        |> Seq.fold (fun (hex:string list) line -> 
            line |> function
            | Comment(comment) -> (hex @ [comment])
            | Directive(name, info) -> (hex @ [name; info])
            | Instruction(opcode, operands) -> (hex @ [opcode; operands])
        ) (List.empty<string>)
    


    member val DlxFile = dlxin
    member val HexFile = hexout


    member asm.Run() = 
        let output = 
            assemble()
            |> List.fold (fun r s -> r + s + "\n") ("")
        File.AppendAllText(hexout, output)
    
    member asm.Run(dlxin:string) = ""

    interface IDisposable with
        member this.Dispose() = ()