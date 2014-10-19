module FsDLX.Assembler.Grammar
open System

type DLXInput =
    | Comment of string
    | Directive of Directive
    | Instruction of uint32 * Instruction * string

    override di.ToString() = di |> function
        | Comment c -> c
        | Directive d -> d.ToString()
        | Instruction(pc, instruction, comment) ->
            //printfn "Instruction length: %A" (instruction.ToString().Length)
            sprintf "%s: %s\t#\t%s"
                (pc.ToString("x8"))
                (instruction.ToString() |> bin2hex)
                (comment)

and Directive(pc:uint32, data:string, comment:string) =
    member val PC = pc
    member val Data = data
    member val Comment = comment

    override d.ToString() =
        sprintf "%s: %s"
            (d.PC.ToString("x8"))
            (d.Data + d.Comment)

and Instruction =
    | IType of Opcode * Operands
    | RType of RRX * Operands * Unused * Func
    | JType of Opcode * Operands
    
    override i.ToString() = i |> function
        | IType(op, ops) -> 
            sprintf "%s%s" (op.ToString()) (ops.ToString())
        | RType(rrx, ops, unused, func) -> 
            sprintf "%s%s%s%s" (rrx.ToString()) (ops.ToString()) (unused.ToString()) (func.ToString())
        | JType(op, ops) -> 
            sprintf "%s%s" (op.ToString()) (ops.ToString())

and Opcode =
    | IType of string * int
    | RType of string * int
    | JType of string * int
    override op.ToString() = op |> function
        | IType(o,e) | JType(o,e) -> Convert.ToString(e, 2).PadLeft(6, '0')
        | _ -> failwith "Invalid to-string call on opcode"

and Operands =
    | IType of Register * Register * Immediate
    | RType of Register * Register * Register 
    | JType of Immediate

    static member TRAP(tval:string) = Operands.JType(Immediate.Value(int tval))
    static member NOP() = Operands.RType(Register.Unused, Register.Unused, Register.Unused)

    override ops.ToString() = ops |> function
        | IType(rs1, rd, imm)   -> sprintf "%s%s%s" (rs1.ToString()) (rd.ToString()) (imm.ToString())
        | RType(rs1, rs2, rd)   -> sprintf "%s%s%s" (rs1.ToString()) (rs2.ToString()) (rd.ToString())
        | JType(name)           -> sprintf "%s" (name.ToString().PadLeft(26, '0'))

and Register =
    | RD of string | RS1 of string | RS2 of string | Unused
    override r.ToString() = r |> function
        | RD s | RS1 s | RS2 s -> 
            Convert.ToString(s.Substring(1) |> int, 2).PadLeft(5, '0')
        | Unused -> "00000"

and Immediate =
    | Label of Label
    | Register of Register
    | BasePlusOffset of Base * Offset
    | Value of int
    | Name of string
    | Unused
    override i.ToString() = i |> function
        | Label imm -> imm.ToString().PadLeft(16, '0')
        | Register imm -> imm.ToString().PadLeft(16, '0')
        | BasePlusOffset(b,o) -> Convert.ToString(b + o, 2).PadLeft(16, '0')
        | Value v -> Convert.ToString(v |> int16, 2).PadLeft(16, '0')
        | Name n -> Convert.ToString(int n, 2).PadLeft(26, '0')
        | Unused -> "0".PadLeft(16, '0')
        
and Base = int16
and Offset = int16
and Label =
    | Inline of string
    | Reference of SymbolTableEntry

    member l.ReplaceWithAddress(st:SymbolTable) = 
        //printfn "Replacing: %A" l
        l |> function
        | Inline l -> st.Lookup(l)
        | _ -> failwith "Can't replace label when not inline type"

and RRX =
    | RRalu of string | RRfpu of string
    override rrx.ToString() = rrx |> function
        | RRalu s | RRfpu s -> Convert.ToString(int s, 2).PadLeft(6, '0')

and Unused =
    | U5 | U6
    override u.ToString() = u |> function | U5 -> "00000" | U6 -> "000000"

and Func =
    | F6 of string | F5 of string
    override f.ToString() = f |> function
        | F6 s -> Convert.ToString(int s, 2).PadLeft(6, '0')
        | F5 s -> Convert.ToString(int s, 2).PadLeft(5, '0')

and SymbolTableEntry(symbol:string, value:int) =
    member val Symbol = symbol
    member val Value = value

    override ste.ToString() =
        sprintf "%s <==> %A" ste.Symbol ste.Value

and SymbolTable() =
    let mutable entries = List.empty<SymbolTableEntry>
    let mutable tab = Map.empty<string, int>
    member st.UpdateTable() =  
        tab <- 
            entries 
            |> List.map (fun ste -> (ste.Symbol, ste.Value)) 
            |> Map.ofList
    member st.Add(k,v) = tab <- tab.Add(k,v)
    member st.Add(ste:SymbolTableEntry) = tab <- tab.Add(ste.Symbol, ste.Value)
    member st.Lookup(k) = 
        //printfn "trying to lookup: %A" k
        tab.[k]
    member st.Lookup(ste:SymbolTableEntry) = 
        //printfn "trying to lookup: %A" ste
        st.Lookup(ste.Symbol)

    override st.ToString() =
        tab |> Map.toList |> List.fold (fun s (k,v) -> 
            let s' = sprintf "(k,v) ==> (%A, %A)" k (v.ToString("x8"))
            s + s' + "\n") ("")

    member st.Dump() =
        printfn "============  Symbole Table  ============"
        printfn "%s" (st.ToString())
