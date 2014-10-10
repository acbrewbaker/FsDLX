[<AutoOpen>]
module Test.FsDLX.Common

open System
open System.IO
open System.Text.RegularExpressions
open NUnit.Framework
open NCrunch.Framework

open FsDLX


//
//let inline (@@) (a:string) (b:string) = Path.Combine(a, b)
//
//let dlxfiles = Directory.GetFiles(inputdir) |> Array.filter(fun f -> f.EndsWith(".dlx"))
//let hexfiles = Directory.GetFiles(inputdir) |> Array.filter(fun f -> f.EndsWith(".hex"))
//
//let titledDisplay title data =
//    let len = 70
//    let title = (sprintf " %s " title)
//    let left = (len - title.Length) / 2
//    printfn "%s" (title.PadRight(left, '=').PadLeft(len, '='))
//    printfn "%A" data
//    printfn "%s" ("=".PadLeft(len, '='))
//
//let getLines path = path |> File.ReadAllLines |> List.ofArray
//let getText path = path |> File.ReadAllText
//
//let getDetailedMatchInfo (regex:Regex) (input:string) =
//    let m = regex.Match(input)
//    let groups = m.Groups
//    printfn "Match: %A" m
//    printfn "Groups: %A" groups
//
//
//let getInstructions path regex =
//    let instructions = path |> getLines |> List.map (fun line -> Instruction.Match regex line)
//    printfn "\n**************   Instructions   **********************"
//    printfn "%A" instructions
//    instructions
//
//let showBinaryInstructions (instructions:Instruction list) =
//    printfn "\n**************   Encoded Instructions (Binary)  ******************"
//    for ins in instructions do ins.ToBinary() |> printfn "%s"
//
//let getAsmInputs path regex =
//    let asmInput = path |> getLines |> List.mapi (fun i line -> AssemblerInput.Parse regex line (ProgramCounter.Value(i * 4 |> uint32)))
//    printfn "\n*************** ASM Input   ***************************"
//    printfn "%A" asmInput
//    asmInput
//
//
//let updateSymbolTable (st:SymbolTable) (ai:AssemblerInput list) =
//    let update = function | Some lbl, pc -> st.Add(lbl, pc) |> ignore | None, _ -> ()
//    ai |> List.iter (fun input ->
//        input |> function
//        | Instruction(pc, label, instruction) -> update (label,pc)
//        | Directive(pc, label, directive) -> update (label, pc)
//        | _ -> ())
//    printfn "\n*************** Updated Symbol Table   ***************************"
//    st.Display()
//
//
//let updateInlineLabels (st:SymbolTable) (ir:AssemblerInput list) =
//    let asmInput =
//        (List.empty<AssemblerInput>, ir) ||> Seq.fold (fun (newinput) input ->
//            input |> function
//            | Instruction(pc, label, instruction) -> 
//                //printfn "Old Instruction: %A" instruction
//                //printfn "New Instruction: %A" (instruction.LabelToAddress st)
//                newinput @ [Instruction(pc, label, instruction.LabelToAddress st)]
//            | _ -> 
//                newinput )
//    printfn "\n******************** Updated Inputs ***************************"
//    printfn "%A" asmInput
//    asmInput
//
//let testAssemble (st:SymbolTable) (regex:Regex) (dlx:string list) =
//    let asmInput = dlx |> List.mapi ( fun i line -> AssemblerInput.Parse regex line (ProgramCounter.Value(i * 4 |> uint32)))
//    updateSymbolTable st asmInput
//    let asmInput = updateInlineLabels st asmInput
//    let state0 = st, ProgramCounter.Value(0u), List.empty<string>
//
//    let inputs = (asmInput, dlx) ||> List.zip
//    let result =
//        (state0, inputs) ||> Seq.fold (fun (state:AssemblerState) (asm, line) ->
//            let st, pc, hex = state
//            //printfn "State: %A" state
//            asm |> function
//            | Instruction(pc, label, instruction) ->
//                //printfn "Old Instruction: %A" instruction
//                let newins = instruction.LabelToAddress st
//                //printfn "New Instruction: %A" newins
//                let newhex = hex @ [pc.ForHexOutput + newins.ToHex() + Conversions.asComment line]
//                //printfn "PC: %A" pc
//                let newpc = pc //pc.GetNewPC()
//                (st, newpc, newhex)
//            | _ -> state )
//    printfn "***************  Last Assembler State  *****************" 
//    printfn "%A" result
//    result
//
//let printContent dlx expected actual =
//    printfn "==========  DLX  ==========" 
//    for l in dlx do printfn "%s" l
//    printfn "==========  HEX - Expected  =========="
//    printfn "%s" expected
//    printfn "==========  HEX - Actual  ==========" 
//    printfn "%s" actual