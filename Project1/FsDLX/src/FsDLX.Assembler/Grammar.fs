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
            sprintf "%s: %s#%s"
                (pc.ToString("x8") + ": ")
                (instruction.ToString())
                (comment)

and Directive(pc:uint32, data:string, comment:string) =
    member val PC = pc
    member val Data = data
    member val Comment = comment

and Instruction =
    | IType of Opcode * Operands
    | RType of RRX * Operands * Unused * Func
    | JType of Opcode * Operands
    
    override i.ToString() = i |> function
        | IType(opcode, operands) -> 
            sprintf "%s%s" 
                (opcode.ToString()) 
                (operands.ToString())
        | RType(rrx, operands, unused, func) -> 
            sprintf "%s%s%s%s" 
                (rrx.ToString()) 
                (operands.ToString()) 
                (unused.ToString()) 
                (func.ToString())
        | JType(opcode, operands) ->
            sprintf "%s%s" 
                (opcode.ToString())
                (operands.ToString())

and Opcode =
    | IType of int
    | RType of int
    | JType of int

    override o.ToString() = 
        o |> function
        | IType(op) | JType(op) -> Convert.ToString(op, 2).PadLeft(6, '0')
        | _ -> failwith "Invalid to-string call on opcode"

and Operands =
    | IType of Register * Register * Immediate
    | RType of Register * Register * Register
    | JType of Immediate

    static member NOP() = RType(Register.R "r0", Register.R "r0", Register.R "r0")
    static member TRAP(s:string) = JType(Immediate.Value (Convert.ToInt32(s)))

    override o.ToString() = o |> function
        | IType(rs1, rd, imm) -> 
            sprintf "%s%s%s" 
                (rs1.ToString()) 
                (rd.ToString())
                (imm.ToString())
        | RType(rs1, rs2, rd) -> 
            sprintf "%s%s%s" 
                (rs1.ToString()) 
                (rs2.ToString()) 
                (rd.ToString())
            
        | JType(name) -> 
            sprintf "%s" 
                (name.ToString())

and Register =
    | R of string
    | F of string
    | Unused

    static member (+) (b:Base, r:Register) = (b, r) |> function
        | b, R r -> b + int r
        | _ -> failwith "Cant add base to floating point register"

    override r.ToString() = r |> function
        | R s | F s -> 
            Convert.ToString(s.Substring(1) |> int, 2).PadLeft(5, '0')
        | Unused -> "00000"

and Immediate =
    | Value of Value
    | Label of Label
    | Register of Register
    | BasePlusOffset of Base * Offset
    | Unused

    override i.ToString() = i |> function
        | Value imm -> Convert.ToString(imm |> int, 2).PadLeft(16, '0')
        | Label imm -> imm.ToString().PadLeft(16, '0')
        | Register imm -> imm.ToString().PadLeft(16, '0')
        | BasePlusOffset(b,o) -> 
            printfn "Immediate BPO to string"
            let bpo = (b,o) |> function
                | bv, Offset.Value ov -> bv + ov
                | bv, Offset.Label ol -> bv + ol                
                | bv, Offset.Register oreg -> bv + (oreg.ToString() |> int)
            bpo.ToString()
        | Unused -> "0".PadLeft(16, '0')
        
and Base = int

and Offset =
    | Value of Value
    | Label of Label
    | Register of Register

    member o.RepalceWithAddress(st:SymbolTable) = o |> function
        | Label l -> Label (l.ReplaceWithAddress(st))
        | Value v -> Value v
        | Register r -> Register r

    static member (+) (b:Base, o:Offset) = (b, o) |> function
        | b, Value o -> b + o
        | b, Label o -> b + o
        | b, Register o -> b + (o.ToString() |> int) 

    override o.ToString() = o |> function
        | Value v -> v.ToString()
        | Label l -> l.ToString()
        | Register r -> r.ToString()

and Value = int

and Label =
    | Inline of string
    | Reference of SymbolTableEntry
    | Value of Value

    member l.ReplaceWithAddress(st:SymbolTable) = l |> function
        | Inline l -> Value (st.Lookup(l))
        | _ -> failwith "Can't replace label when not inline type"

    static member (+) (l1:Label, l2:Label) = (l1,l2) |> function
        | Reference r1, Reference r2 -> r1.Value + r2.Value
        | _ -> failwith "Can't add inline labels"

    static member (+) (l:Label, v:Value) = (l,v) |> function
        | Reference r, x -> r.Value + x
        | _ -> failwith "Can't add inline labels"

    static member (+) (v:Value, l:Label) = (v,l) |> function
        | x, Reference r -> x + r.Value
        | _ -> failwith "Can't add inline labels"

    override l.ToString() = l |> function
        | Inline s -> s
        | Reference r -> r.Symbol.ToString()
        | Value v -> Convert.ToString(v |> int, 2)

and RRX =
    | RRalu of string
    | RRfpu of string

    override rrx.ToString() = rrx |> function
        | RRalu s | RRfpu s -> Convert.ToString(int s, 2).PadLeft(6, '0')

and Unused =
    | U5
    | U6

    override u.ToString() = u |> function | U5 -> "00000" | U6 -> "000000"

and Func =
    | F6 of string
    | F5 of string

    override f.ToString() = f |> function
        | F6 s -> Convert.ToString(int s, 2).PadLeft(6, '0')
        | F5 s -> Convert.ToString(int s, 2).PadLeft(5, '0')

and SymbolTableEntry(symbol:string, value:int) =
    member val Symbol = symbol
    member val Value = value

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
    member st.Lookup(k) = tab.[k] 
    member st.Lookup(ste:SymbolTableEntry) = st.Lookup(ste.Symbol)

    override st.ToString() =
        tab |> Map.toList |> List.fold (fun s (k,v) -> 
            let s' = sprintf "(k,v) ==> (%A, %A)" k (v.ToString("x8"))
            s + s' + "\n") ("")