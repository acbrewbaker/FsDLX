[<AutoOpen>]
module FsDLX.Assembler.Interface

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

open Grammar
   

type Assembler(dlxfile:string, info:OpcodeInfo) =
    do dlxfile |> function
    | _ when not (dlxfile.EndsWith(".dlx")) -> failwith "invalid input file"
    | _ -> ()
        
    let hexfile = Path.ChangeExtension(dlxfile, ".hex")

    let symtab = new SymbolTable()

    let parseInputs (lines:string[]) (pc:uint32 ref) (st:SymbolTable) =
    
        let (|InlineComment|_|) : string -> (string*string) option = function
            | s when s.Contains(";") -> 
                let s = s.Split(';') 
                (s.[0].Trim(), s.[1].Trim()) |> Some
            | _ -> None
    
        let newState (inputs:DLXInput list) (dlxin:DLXInput list)  = inputs @ dlxin    

        let (|Comment|_|) inputs = function
            | Patterns.Comment comment ->
                //[DLXInput.Comment comment]
                //|> newState inputs
                inputs
                |> Some
            | _ -> None
    
        let (|Instruction|_|) inputs comment = function
            | Patterns.Instruction info instruction ->
                //printfn "Instruction Comment: %A" comment
                [DLXInput.Instruction(!pc, instruction, comment)]
                |> newState inputs
                |> Some
            | _ -> None

        let (|Directive|_|) inputs = function
            | Patterns.Directive directive ->
                directive |> List.choose (fun d -> if d.Comment.Length > 1 then Some d else None)
                |> List.map DLXInput.Directive
                |> newState inputs
                |> Some
            | _ -> None

        lines |> Seq.fold (fun (inputs:DLXInput list) line ->
            //printfn "Line ===> %A" line
            let line = line.Trim()
            (line, pc) |> function
            | _ when line.Length <= 1 -> inputs
            | Comment inputs comment -> comment
            | Patterns.Label (ste, rest) ->
                //pc := !pc + 4u
                st.Add(ste)
                let lbl = Label.Reference(ste)
                //st.Dump()
                let rest = rest.Trim()
                //printfn "Rest: %A" rest
                let data, comment = rest |> function | InlineComment c -> c | _ -> rest, rest
                //printfn "Data, Comment ==> %A, %A" data comment
                (data.Trim(), pc) |> function
                | Instruction inputs line i -> pc := !pc + 4u; i
                | Directive inputs d -> d
                | _ when rest.StartsWith(";") ->
                    ("nop", pc) |> function
                    | Instruction inputs comment i -> pc := !pc + 4u; i
                    | _ -> failwith "Failed creating nop"
                | _ -> failwith "Failed to match info after matching label"

            | Directive inputs d -> d
            | Instruction inputs line i -> pc := !pc + 4u; i
            | _ -> failwith (sprintf "Failed to create DLXInput from %A" line)
            ) (List.empty<DLXInput>)

    let assemble() =
        let lines = dlxfile |> File.ReadAllLines 
        let pc = ref 0u
        let inputs = parseInputs lines pc symtab
        
        pc := 0u

        let newState (hex:string list) (dlxin:DLXInput) = hex @ [dlxin.ToString()]

        let updateLabels = 
            //printfn "Update Labels"
//            let procImmediate rs1 rd = function
//                | Immediate.Label lbl ->
//                    Operands.IType(rs1, rd, Immediate.Label (lbl.ReplaceWithAddress(symtab)))
//                | Immediate.BasePlusOffset(b,o) -> 
//                    Operands.IType(rs1, rd, Immediate.BasePlusOffset(b,o.RepalceWithAddress(symtab)))
//                | immediate -> Operands.IType(rs1, rd, immediate)
            let procOperands = 
                //printfn "Proc operands"
                function
                | Operands.IType(rs1, rd, imm) -> 
                    //printfn "Proc itype immediate"
                    imm |> function
                    | Immediate.Label lbl ->            Operands.IType(rs1, rd, Immediate.Value (lbl.ReplaceWithAddress(symtab)))
                    | Immediate.BasePlusOffset(b,o) ->  Operands.IType(rs1, rd, Immediate.Value ((b + o) |> int))
                    | immediate ->                      Operands.IType(rs1, rd, immediate)
                | Operands.JType(name) -> name |> function
                    | Immediate.Label label -> 
                        let a = label.ReplaceWithAddress(symtab, int !pc)
                        //printfn "label, addr from table: %A  ==> %A" label a
                        Operands.JType(Immediate.Name (Convert.ToString(a, 2)))
                    | _ -> Operands.JType(name)
                | operands -> operands

            
            function
            | Instruction.IType(opcode, operands) -> pc := !pc + 4u; Instruction.IType(opcode, operands |> procOperands)
            | Instruction.JType(opcode, operands) -> pc := !pc + 4u; Instruction.JType(opcode, operands |> procOperands)
            | instruction -> pc := !pc + 4u; instruction

        inputs
        |> Seq.fold (fun (hex:string list) dlxin -> 
            dlxin |> function
            | DLXInput.Comment(_)   -> dlxin |> newState hex
            | DLXInput.Directive(_) -> dlxin |> newState hex
            | DLXInput.Instruction(pc, instruction, comment) -> 
                //printfn "ins ==> %A" instruction;
                DLXInput.Instruction(pc, instruction |> updateLabels, comment)
                |> newState hex
        ) (List.empty<string>)

//    new (dlxfile:string) =
//        let info = new OpcodeInfo()
//        new Assembler(dlxfile, info)
        
    new (dlxfile:string, srcdir:string) =
        let itypesfile, rtypesfile, jtypesfile =
            srcdir @@ "Itypes",
            srcdir @@ "Rtypes",
            srcdir @@ "Jtypes"
        let info = new OpcodeInfo(srcdir, itypesfile, rtypesfile, jtypesfile)
        new Assembler(dlxfile, info)

    new (dlxfile, srcdir, itypesfile, rtypesfile, jtypesfile) =
        new Assembler(dlxfile, new OpcodeInfo(srcdir, itypesfile, rtypesfile, jtypesfile))

    member val SrcDir = info.SrcDir
    member val DlxFile = dlxfile
    member val HexFile = hexfile

    member asm.Run(outpath:string, ?verbose:bool) =
        let verbose = defaultArg verbose false
        //let assemble() = if verbose then assembleVerbose() else assemble()
        
        let outpath = outpath @@ Path.GetFileName(hexfile)
        
        let output = assemble() |> concatLines

        if File.Exists(outpath) then File.Delete(outpath)
        File.AppendAllText(outpath, output)
    
    member asm.Run() = asm.Run(asm.SrcDir @@ "Tests")

    interface IDisposable with
        member this.Dispose() = ()
        