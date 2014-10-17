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
        | IType(opcode, operands) -> 
            //printfn "IType opcode, operands: (%A, %A)" (opcode.ToString()) (operands.ToString())
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
            printfn "JType opcode, operands ==> %A, %A" opcode operands
            let imm = operands.ToString() //|> revstr
            printfn "JType imm, len ==> %A, %A" imm imm.Length
            let imm = if imm.Length > 26 then imm.Substring(0, 26) else imm
            let sign = imm.Chars(imm.Length - 1)
            printfn "JType sign ==> %A" sign
            sprintf "%s%s" 
                (opcode.ToString())
                (imm.PadLeft(26, sign))

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
    static member TRAP(s:string) = JType(Immediate.Value (Value.UInt (Convert.ToUInt32(s))))

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
                (name.ToString().PadLeft(26, '0'))

and Register =
    | R of string
    | F of string
    | Unused

    static member (+) (b:Base, r:Register) = (b, r) |> function
        | b, R r -> b + int16 r
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
        | Value imm -> imm.ToString().PadLeft(16, '0')
        | Label imm -> imm.ToString().PadLeft(16, '0')
        | Register imm -> imm.ToString().PadLeft(16, '0')
        | BasePlusOffset(b,o) -> 
            let bpo = o |> function
                | Offset.Value o' -> b + o'
                | Offset.Label o' -> o' + b               
                | Offset.Register oreg -> 
                    b + Convert.ToInt16(oreg.ToString(), 2)
                    
            printfn "Imm.BPO.TOString  ===> %A" bpo
            Convert.ToString(bpo, 2).PadLeft(16, '0') //|> revstr
        | Unused -> "0".PadLeft(16, '0')
        
and Base = int16

and Offset =
    | Value of Value
    | Label of Label
    | Register of Register

    member o.RepalceWithAddress(st:SymbolTable) = o |> function
        | Label l -> Label (l.ReplaceWithAddress(st))
        | Value v -> Value v
        | Register r -> Register r

    static member (+) (b:Base, o:Offset) = (b, o) |> function
        | b, Value o' -> b + o'
        | b, Label o' -> o' + b
        | b, Register o' -> b + Convert.ToInt16(o'.ToString(), 2)
            
    override o.ToString() = o |> function
        | Value v -> v.ToString()
        | Label l -> l.ToString()
        | Register r -> r.ToString()

and Value = 
    | Int of int
    | UInt of uint32

    static member (+) (v1:Value, v2:Value) = (v1,v2) |> function
        | Int v1', Int v2' -> v1' + v2'
        | Int v1', UInt v2' -> v1' + (int v2')
        | UInt v1', Int v2' -> (int v1') + v2'
        | UInt v1', UInt v2' -> int (v1' + v2')


    static member (+) (b:Base, v:Value) = v |> function
        | Int x -> b + int16 x
        | UInt x -> b + int16 x

    static member (+) (v:Value, b:Base) = v |> function
        | Int v' -> b + int16 v'
        | UInt v' -> b + int16 v'

    static member (+) (l:Label, v:Value) = (l,v) |> function
        | Label.Reference l', Int v' -> l'.Value + v'
        | Label.Value l', v' -> l' + v' 
        | _ -> failwith "cant add values to inline labels"

    override v.ToString() = v |> function
        | Int x -> Convert.ToString(x, 2).PadLeft(16, '0')
        | UInt x -> Convert.ToString(int x, 2).PadLeft(16, '0')

and Label =
    | Inline of string
    | Reference of SymbolTableEntry
    | Value of Value

    member l.ReplaceWithAddress(st:SymbolTable) = 
        printfn "Replacing: %A" l
        l |> function
        | Inline l -> 
            printfn "Looking up.... %A" l
            Value (st.Lookup(l))
        | _ -> failwith "Can't replace label when not inline type"

    static member (+) (l:Label, b:Base) = (l,b) |> function
        | Reference l', b' -> l'.Value + b'
        | Value l', b' -> l' + b'
        | _ -> failwith "Can't add inline labels"

    static member (+) (l1:Label, l2:Label) = (l1,l2) |> function
        | Reference r1, Reference r2 -> 
            printfn "Adding label label"
            r1.Value + r2.Value
        | _ -> failwith "Can't add inline labels"

    static member (+) (l:Label, v:Value) = (l,v) |> function
        | Reference r, x -> 
            printfn "Adding label value"
            int16 r.Value + x
        | _ -> failwith "Can't add inline labels"

    static member (+) (v:Value, l:Label) = (v,l) |> function
        | Value.Int v', Reference l' -> 
            printfn "Adding value label"
            int16 v' + l'.Value
        | _ -> failwith "Can't add inline labels"

    override l.ToString() = l |> function
        | Inline s -> s
        | Reference r -> r.Symbol.ToString()
        | Value v -> v.ToString()

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

and SymbolTableEntry(symbol:string, value:int16) =
    member val Symbol = symbol
    member val Value = value

    override ste.ToString() =
        sprintf "%s <==> %A" ste.Symbol ste.Value

and SymbolTable() =
    let mutable entries = List.empty<SymbolTableEntry>
    let mutable tab = Map.empty<string, int16>
    member st.UpdateTable() =  
        tab <- 
            entries 
            |> List.map (fun ste -> (ste.Symbol, ste.Value)) 
            |> Map.ofList
    member st.Add(k,v) = tab <- tab.Add(k,v)
    member st.Add(ste:SymbolTableEntry) = tab <- tab.Add(ste.Symbol, ste.Value)
    member st.Lookup(k) = Value.Int (int tab.[k])
    member st.Lookup(ste:SymbolTableEntry) = st.Lookup(ste.Symbol)

    override st.ToString() =
        tab |> Map.toList |> List.fold (fun s (k,v) -> 
            let s' = sprintf "(k,v) ==> (%A, %A)" k (v.ToString("x8"))
            s + s' + "\n") ("")

    member st.Dump() =
        printfn "============  Symbole Table  ============"
        printfn "%s" (st.ToString())