[<AutoOpen>]
module FsDLX.Assembler.Interface

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

open Grammar
   

type Assembler(dlxfile:string, itypesfile:string, rtypesfile:string, jtypesfile:string) =
    do dlxfile |> function
    | _ when not (dlxfile.EndsWith(".dlx")) -> failwith "invalid input file"
    | _ -> ()
    
    let hexfile = Path.ChangeExtension(dlxfile, ".hex")

    let symtab = new SymbolTable()

    let parseInputs (lines:string[]) (pc:uint32 ref) (st:SymbolTable) =
        let info = OpcodeInfo(itypesfile, rtypesfile, jtypesfile)
    
        let (|InlineComment|_|) : string -> (string*string) option = function
            | s when s.Contains(";") -> 
                let s = s.Split(';') 
                (s.[0].Trim(), s.[1].Trim()) |> Some
            | _ -> None
    
        let newState (inputs:DLXInput list) (dlxin:DLXInput list)  = inputs @ dlxin    

        let (|Comment|_|) inputs = function
            | Patterns.Comment comment ->
                [DLXInput.Comment comment]
                |> newState inputs
                |> Some
            | _ -> None
    
        let (|Instruction|_|) inputs comment = function
            | Patterns.Instruction info instruction ->
                [DLXInput.Instruction(!pc, instruction, comment)]
                |> newState inputs
                |> Some
            | _ -> None

        let (|Directive|_|) inputs = function
            | Patterns.Directive directive ->
                directive |> List.map DLXInput.Directive
                |> newState inputs
                |> Some
            | _ -> None

        lines |> Seq.fold (fun (inputs:DLXInput list) line ->
            (line.Trim(), pc) |> function
            | _ when line.Length <= 1 -> inputs
            | Comment inputs comment -> comment
            | Patterns.Label (ste, rest) ->
                st.Add(ste)
                let lbl = Label.Reference(ste)
                let rest = rest.Trim()
                let data, comment = rest |> function | InlineComment c -> c | _ -> rest, rest
            
                (data, pc) |> function
                | Instruction inputs comment i -> i
                | Directive inputs d -> d
                | _ when rest.StartsWith(";") ->
                    ("nop", pc) |> function
                    | Instruction inputs comment i -> i
                    | _ -> failwith "Failed creating nop"
                | _ -> failwith "Failed to match info after matching label"

            | Directive inputs d -> d
            | Instruction inputs line i -> i
            | _ -> failwith (sprintf "Failed to create DLXInput from %A" line)
            ) (List.empty<DLXInput>)

    let assemble() =
        let lines = dlxfile |> File.ReadAllLines 
        let pc = ref 0u
        let inputs = parseInputs lines pc symtab
    
        let newState (hex:string list) (dlxin:DLXInput) = hex @ [dlxin.ToString()]

        let updateLabels = 
            let procImmediate rs1 rd = function
                | Immediate.Label lbl ->
                    Operands.IType(rs1, rd, Immediate.Label (lbl.ReplaceWithAddress(symtab)))
                | Immediate.BasePlusOffset(b,o) -> 
                    Operands.IType(rs1, rd, Immediate.BasePlusOffset(b,o.RepalceWithAddress(symtab)))
                | immediate -> Operands.IType(rs1, rd, immediate)

            let procOperands = function
                | Operands.IType(rs1, rd, imm) -> (rs1, rd, imm) |||> procImmediate
                | operands -> operands

            function
            | Instruction.IType(opcode, operands) -> Instruction.IType(opcode, operands |> procOperands)
            | Instruction.JType(opcode, operands) -> Instruction.JType(opcode, operands |> procOperands)
            | instruction -> instruction

        inputs
        |> Seq.fold (fun (hex:string list) dlxin -> 
            dlxin |> function
            | DLXInput.Comment(_)   -> dlxin |> newState hex
            | DLXInput.Directive(_) -> dlxin |> newState hex
            | DLXInput.Instruction(pc, instruction, comment) ->
                DLXInput.Instruction(pc, instruction |> updateLabels, comment)
                |> newState hex
        ) (List.empty<string>)

    new (dlxfile:string) =
        let d = Support.Defaults.Init
        let i, r, j =
            d.Files.IType, d.Files.RType, d.Files.JType
        new Assembler(dlxfile, i, r, j)
        
    member val DlxFile = dlxfile
    member val HexFile = hexfile

    member asm.Run(outpath:string, ?verbose:bool) =
        let verbose = defaultArg verbose false
        //let assemble() = if verbose then assembleVerbose() else assemble()
        
        let outpath = outpath @@ Path.GetFileName(hexfile)
        
        let output = assemble() |> concatLines

        if File.Exists(outpath) then File.Delete(outpath)
        File.AppendAllText(outpath, output)
    
    member asm.Run() = asm.Run("/u/css/ab67597/5483/Project1/Tests/")

    interface IDisposable with
        member this.Dispose() = ()
        