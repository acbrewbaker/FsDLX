[<AutoOpen>]
module FsDLX.Assembler.Interface

open System
open System.IO
open System.Text
open System.Text.RegularExpressions

let inline (++) (a:string) (b:string) = a + " " + b

let matches (r:Regex) s = r.IsMatch(s)


let asmfail errmsg content =
    printfn "%s" errmsg
    printfn "%s" content
    ""

let strAsComment str = "\t#\"" + str + "\""
let asComment str = "\t#" + str
let addLeadingZero (str:string) =
    str |> function
    | _ when str.StartsWith("-.") -> str.Insert(str.IndexOf("-.") + 1, "0")
    | _ when str.StartsWith("+.") -> str.Insert(str.IndexOf("+.") + 1, "0")
    | _ when str.StartsWith(".") -> str.Insert(str.IndexOf("."), "0")
    | _ -> str
let floatingPointAsComment = addLeadingZero >> asComment

let str2hex (str:string) =
    (Encoding.Default.GetBytes(str) |> BitConverter.ToString).Replace("-","").ToLower() + "00"

let revstr (s:string) = s.ToCharArray() |> Array.rev |> Array.fold (fun s c -> s + string c) ("")



type DLXInput =
    | Comment of string
    | Directive of Label option * Directive
    | Instruction of uint32 * Label option * Instruction * string

    override di.ToString() = di |> function
        | Comment s -> s
        | Directive(label, directive) -> directive.ToString()
        | Instruction(pc, _, instruction, comment) ->
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
            printfn "Bout to print"
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
        printfn "Opcode: %A" o
        o |> function
        | IType(op) | JType(op) -> Convert.ToString(op, 2).PadLeft(6, '0')
        | _ -> failwith "Invalid to-string call on opcode"

and Operands =
    | IType of Register * Register * Immediate
    | RType of Register * Register * Register
    | JType of Immediate

    static member NOP() = RType(Register.R "r0", Register.R "r0", Register.R "r0")
    static member TRAP(s:string) = JType(Immediate.Value (Convert.ToUInt32(s)))

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
        | Base.Value b, R r -> b + uint32 r
        //| Base.Label b, R r -> 
        | _ -> failwith "cant be addin that reg to that base"

    override r.ToString() = r |> function
        | R s | F s -> 
            printfn "Register input string: %A" s
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
            let bpo = (b,o) |> function
                | Base.Value bv, Offset.Value ov -> bv + ov
                | Base.Value bv, Offset.Label ol -> bv + ol
                | Base.Label bl, Offset.Value ov -> bl + ov
                | Base.Label bl, Offset.Label ol -> bl + ol
                | Base.Value bv, Offset.Register oreg -> bv + (oreg.ToString() |> uint32)
                | Base.Label bl, Offset.Register oreg -> bl + (oreg.ToString() |> uint32)
            bpo.ToString()
        | Unused -> "0".PadLeft(16, '0')
        
and Base =
    | Value of Value
    | Label of Label

    member b.ReplaceWithAddress(st:SymbolTable) = b |> function
        | Label l -> Label (l.ReplaceWithAddress(st))
        | Value v -> Value v

    static member (+) (b:Base, o:Offset) = (b,o) |> function
        | Value b, Offset.Value o -> b + o
        | Value b, Offset.Label o -> b + o
        | Label b, Offset.Value o -> b + o
        | Label b, Offset.Label o -> b + o

    override b.ToString() = b |> function
        | Value v -> v.ToString()
        | Label l -> l.ToString()

and Offset =
    | Value of Value
    | Label of Label
    | Register of Register

    member o.RepalceWithAddress(st:SymbolTable) = o |> function
        | Label l -> Label (l.ReplaceWithAddress(st))
        | Value v -> Value v
        | Register r -> Register r

    static member (+) (b:Base, o:Offset) = (b, o) |> function
        | Base.Value b, Value o -> b + o
        | Base.Value b, Label o -> b + o
        | Base.Value b, Register o -> b + (o.ToString() |> uint32) 
        | Base.Label b, Value o -> b + o
        | Base.Label b, Label o -> b + o
        //| Base.Label b, Register o -> b + o

    override o.ToString() = o |> function
        | Value v -> v.ToString()
        | Label l -> l.ToString()
        | Register r -> r.ToString()

and Value = uint32

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

    override u.ToString() = u |> function
        | U5 -> "00000"
        | U6 -> "000000"

and Func =
    | F6 of string
    | F5 of string

    override f.ToString() = f |> function
        | F6 s -> Convert.ToString(int s, 2).PadLeft(6, '0')
        | F5 s -> Convert.ToString(int s, 2).PadLeft(5, '0')

and SymbolTableEntry(symbol:string, value:uint32) =
    member val Symbol = symbol
    member val Value = value

and SymbolTable() =
    let mutable entries = List.empty<SymbolTableEntry>
    let mutable tab = Map.empty<string, uint32>
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

module Patterns =
    type InputRegex() =
        member val Comment = Regex(@"(;)(.*)")
        member val Directive = Regex(@"\.(\w+)\s(.*)")
        member val Label = Regex(@"\A(\w+):(.*)")
        member val Instruction = Regex(@"\A(\w+)([^;]*)")

    type DirectiveRegex() =
        member val Text    = Regex(@"(?<=\.(text).*)(\d*)")
        member val Data    = Regex(@"(?<=\.(data).*)(\d+)")
        member val Align   = Regex(@"(?<=\.(align).*)(\d)")
        member val Asciiz  = Regex(@"(?<=\.(asciiz).*)?""([^""]+)""")
        member val Double  = Regex(@"(?<=\.(double).*)([+-]?\d*\.\d+)")
        member val Float   = Regex(@"(?<=\.(float).*)([+-]?\d*\.\d+)")
        member val Word    = Regex(@"(?<=\.(word).*)((-)?(0x)?\d+)")
        member val Space   = Regex(@"(?<=\.(space).*)(\d+)")

    type OpcodeRegex(opinfo:OpcodeInfo) =
        let ipat, rpat, jpat =
            opinfo.ITypes.Pattern,
            opinfo.RTypes.Pattern,
            opinfo.JTypes.Pattern
        member val IType = Regex(opinfo.ITypes.Pattern)
        member val RType = Regex(opinfo.RTypes.Pattern)
        member val JType = Regex(opinfo.JTypes.Pattern)
        member val Any = Regex(ipat ++ rpat ++ jpat)

    type OperandRegex() =
        member val IType = Regex(@"((r\d\d?), (r\d\d?), (\d+))|((r\d\d?), (r\d\d?), (\w+))|((r\d\d?), (\w+))")
        member val RType = Regex(@"([rf]\d\d?), ([rf]\d\d?)$|([rf]\d\d?), ([rf]\d\d?)$")
        member val JType = Regex(@"\A(\w+)$")

    type ImmediateRegex() =
        member val Value            = Regex(@"(\A\d+)$")
        member val Label            = Regex(@"([a-zA-Z]+\d*)$")
        member val Register         = Regex(@"(r\d\d?)$")
        member val BasePlusOffset   = Regex(@"(\w+)\((\w+)\)$")

    let register =
        let r = Regex(@"r\d\d?")
        let f = Regex(@"f\d\d?")

        function
        | reg when reg |> matches r -> Register.R reg
        | reg when reg |> matches f -> Register.F reg
        | _ -> failwith "Failed to create register"

    let baseplusoffset =
        let r = ImmediateRegex()
        function
        | bl,ol when bl |> matches r.Label && ol |> matches r.Label -> 
            Base.Label (Label.Inline bl), Offset.Label (Label.Inline ol)
        | bv,ol when bv |> matches r.Value && ol |> matches r.Value -> 
            Base.Value (Convert.ToUInt32(bv)), Offset.Label (Label.Inline ol)
        | bl,ov when bl |> matches r.Label && ov |> matches r.Value ->
            Base.Label (Label.Inline bl), Offset.Value (Convert.ToUInt32(ov))
        | bv,ov when bv |> matches r.Value && ov |> matches r.Value ->
            Base.Value (Convert.ToUInt32(bv)), Offset.Value (Convert.ToUInt32(ov))
        | bv,oreg when bv |> matches r.Value && oreg |> matches r.Register ->
            Base.Value (Convert.ToUInt32(bv)), Offset.Register (register oreg)
        | _ -> failwith "failed to match base plus offset"

    let immediate =
        let r = ImmediateRegex()
    
        function
        | imm when imm |> matches r.Value       -> Immediate.Value (Convert.ToUInt32(imm))
        | imm when imm |> matches r.Label       -> Immediate.Label (Label.Inline imm)
        | imm when imm |> matches r.Register    -> Immediate.Register (register imm)
        | imm when imm |> matches r.BasePlusOffset -> 
            let b,o = let g = r.BasePlusOffset.Match(imm).Groups in (g.[1].Value, g.[2].Value)
            Immediate.BasePlusOffset (baseplusoffset (b,o))
        | _ -> failwith "Failed to create immediate"

    let (|Instruction|_|) (info:OpcodeInfo) : (string*uint32 ref) -> Instruction option =
        let r = OpcodeRegex(info)
        let i = InputRegex()
        let imm = ImmediateRegex()

        let (|IType|_|) (info:OpcodeInfo) : (string*string) -> (Opcode*Operands) option =
        
//            let (|RRI|RI|R|LF|LR|) (ops:string[]) = ops.Length |> function
            let (|RRI|RI|R|) (ops:string[]) = ops.Length |> function
                | 3 -> RRI ops
                | 2 -> RI ops
//                | 2 -> ops |> function
//                    | _ when ops.[0] |> matches imm.Label -> 
//                        if ops.[1].StartsWith("f")
//                        then LF ops
//                        else LR ops
//                    | _ -> RI ops
                | 1 -> R ops
                | _ -> failwith "no 0 register IType operands"
        
            function
            | (opcode, operands) when opcode |> matches r.IType ->
                //printfn "Matched IType in A.P."
                //printfn "Lookup : %A" (info.Lookup(opcode))
                let opcode, operands =
                    Opcode.IType(info.Lookup(opcode) |> int),
                    operands.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) |> function
                       | RRI ops -> Operands.IType(Register.R ops.[0], Register.R ops.[1], immediate ops.[2])
                       | RI ops -> Operands.IType(Register.R ops.[0], Register.Unused, immediate ops.[1])
                       | R ops -> Operands.IType(Register.R ops.[0], Register.Unused, Immediate.Unused)
//                       | LF ops -> Operands.IType(Register.F ops.[1], Register.Unused, immediate ops.[0])
//                       | LR ops -> Operands.IType(Register.R ops.[1], Register.Unused, immediate ops.[0])
                Some (opcode, operands)
            | _ -> None
    
        let (|RType|_|) (info:OpcodeInfo) : (string*string) -> (Opcode*Operands) option =
          

            let (|RRR|FFF|RR|FF|RF|) (ops:string[]) = 
                let allR, allF, len =
                    ops |> Array.forall (fun reg -> reg.StartsWith("r")),
                    ops |> Array.forall (fun reg -> reg.StartsWith("f")),
                    ops.Length
                (allR, allF, len) |> function
                | true, false, 3 -> RRR ops
                | false, true, 3 -> FFF ops
                | true, false, 2 -> RR ops
                | false, true, 2 -> FF ops
                | false, false,2 -> RF ops
                | _ -> failwith "no 1 register RType operands"
            
            function
            | (opcode, operands) when opcode |> matches r.RType ->
                printfn "Opcode, Operands ===> (%A, %A)" opcode operands
                let op, ops =
                    Opcode.RType(info.Lookup(opcode) |> int),
                    operands.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) |> function
                       | RRR ops -> Operands.RType(Register.R ops.[0], Register.R ops.[1], Register.R ops.[2])
                       | FFF ops -> Operands.RType(Register.F ops.[0], Register.F ops.[1], Register.F ops.[2])
                       | RR ops -> Operands.RType(Register.R ops.[0], Register.Unused, Register.R ops.[1])
                       | FF ops -> Operands.RType(Register.F ops.[0], Register.Unused, Register.F ops.[1])
                       | RF ops -> Operands.RType(Register.R ops.[0], Register.Unused, Register.F ops.[1])
                Some (op, ops)
            | _ -> None

        let (|JType|_|) (info:OpcodeInfo) : (string*string) -> (Opcode*Operands) option =
            
            let (|R|I|) (ops:string[]) = 
                let register = Regex(@"r\d\d?")
                ops.Length |> function
                | 1 -> ops.[0] |> function | o when o |> matches register -> R o | _ -> I ops.[0]
                | _ -> failwith "failed to match JType operands in active pattern"

            function
            | (opcode, operands) when opcode |> matches r.JType ->
                let opcode, operands =
                    Opcode.JType(info.Lookup(opcode) |> int),
                    operands.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) |> function
                       | R ops -> Operands.JType(immediate ops)
                       | I ops -> Operands.JType(immediate ops)
                Some (opcode, operands)
            | _ -> None    
            
        let groups (r:Regex) s = 
            let g = r.Match(s).Groups in (g.[1].Value, g.[2].Value)
             
        function
        | (s, pc) when s |> matches i.Instruction->
            let opcode, operands = (groups i.Instruction s)
            //printfn "Instruction A.P. Opcode, Operands ==> %A, %A" opcode operands
            let rrx (op:string) = 
                printfn "RRX"
                info.GetRRX(op) |> function
                | x when char x = '0' -> RRX.RRalu x
                | x when char x = '1' -> RRX.RRfpu x
                | _ -> failwith "RRX Failed"

            let unused op = rrx op |> function
                | RRX.RRalu _ -> Unused.U5
                | RRX.RRfpu _ -> Unused.U6

            let func op = 
                printfn "FUNC"
                rrx op |> function
                | RRX.RRalu _ -> Func.F6 (info.Lookup(opcode))
                | RRX.RRfpu _ -> Func.F5 (info.Lookup(opcode))

            let instruction = 
                (opcode, operands) |> function
                | "trap", _ ->
                    // trap is IType in the files, but JType in the book.
                    Instruction.JType(Opcode.JType(info.Lookup(opcode) |> int), Operands.TRAP(operands))
                | "nop", _ ->
                    Instruction.RType(rrx opcode, Operands.NOP(), unused opcode, func opcode)
                | IType info (op, ops) -> 
                    //printfn "Making IType"
                    pc := !pc + 4u
                    Instruction.IType(op, ops)
                    
                | RType info (op, ops) -> 
                    //printfn "Making RType"
                    pc := !pc + 4u
                    Instruction.RType(rrx opcode, ops, unused opcode, func opcode)
                | JType info (op, ops) -> 
                    //printfn "Making JType"
                    pc := !pc + 4u
                    Instruction.JType(op, ops)
                | _ -> failwith "failed to make instruction"
            Some instruction
        | _ -> None

    let (|Directive|_|) : (string*uint32 ref) -> Directive list option =
        let r = DirectiveRegex()
    
        let groups (r:Regex) s = [for m in r.Matches(s) -> m.Groups.[2].Value]
        function
        | s, pc when s |> matches r.Text ->
            groups r.Text s |> List.map (fun (str:string) ->
                let pc' = !pc
                let s' = str
                let comment = asComment str
                if s'.Length <= 1 then pc := 0u else pc := UInt32.Parse(s')
                (pc', s', comment)) 
            |> List.map (fun e -> new Directive(e)) 
            |> Some

        | s, pc when s |> matches r.Data ->
            groups r.Data s |> List.map (fun (str:string) ->
                let pc' = !pc
                let s' = str
                let comment = asComment str
                (pc', s', comment)) 
            |> List.map (fun e -> new Directive(e)) 
            |> Some

        | s, pc when s |> matches r.Align ->
            groups r.Align s |> List.map (fun (str:string) ->
                let pc' = !pc
                let s' = str
                let comment = asComment str
                (pc', s', comment)) 
            |> List.map (fun e -> new Directive(e)) 
            |> Some

        | s, pc when s |> matches r.Asciiz ->
            groups r.Asciiz s |> List.map (fun (s:string) ->
                let bytes = Encoding.Default.GetBytes(s)
                let pc' = !pc
                let s' = s |> str2hex
                pc := !pc + uint32 bytes.Length + 1u
                (pc', s', s)) 
            |> List.map (fun e -> new Directive(e)) 
            |> Some
        
        | s, pc when s |> matches r.Double ->
            groups r.Double s |> List.map (fun (str:string) ->
                let pc' = !pc
                let s' = (Double.Parse(str) |> BitConverter.GetBytes |> bytes2hex)
                let comment = floatingPointAsComment str
                pc := !pc + 8u
                (pc', s', comment)) 
            |> List.map (fun e -> new Directive(e)) 
            |> Some

        | s, pc when s |> matches r.Float ->
            groups r.Float s |> List.map (fun (str:string) ->
                let pc' = !pc
                let s' = (Single.Parse(str) |> BitConverter.GetBytes |> bytes2hex)
                let comment = floatingPointAsComment str
                pc := !pc + 4u
                (pc', s', comment)) 
            |> List.map (fun e -> new Directive(e)) 
            |> Some

        | s, pc when s |> matches r.Word ->
            groups r.Word s |> List.map (fun (str:string) ->
                let pc' = !pc
                let str, isNeg = if str.StartsWith("-") then str.Replace("-",""), true else str, false
                let base' = if str.StartsWith("0x") then 16 else 10
                let word = Convert.ToInt32(str, base') * (if isNeg then -1 else 1)
                let s' = (word |> BitConverter.GetBytes |> bytes2hex)
                let comment = asComment (string word)
                (pc', s', comment)) 
            |> List.map (fun e -> new Directive(e)) 
            |> Some

        | s, pc when s |> matches r.Space ->
            groups r.Space s |> List.map (fun (str:string) ->
                let pc' = !pc
                let s' = str
                let comment = asComment str
                (pc', s', comment)) 
            |> List.map (fun e -> new Directive(e)) 
            |> Some

        | _ -> None

    let (|Label|_|) : (string*uint32 ref) -> (SymbolTableEntry*string) option =
        let r = InputRegex()
        let groups (r:Regex) s = let g = r.Match(s).Groups in (g.[1].Value, g.[2].Value)

        function
        | (line, pc) when line |> matches r.Label ->
            let lbl, rest = (groups r.Label line)
            (SymbolTableEntry(lbl, !pc), rest)
            |> Some
        | _ -> None

    let (|Comment|_|) : (string*uint32 ref) -> string option =
        function
        | (line, _) when line.StartsWith(";") -> Some line
        | _ -> None          


let parseInputs (info:OpcodeInfo) (lines:string[]) (pc:uint32 ref) (st:SymbolTable) =
    let r = Patterns.InputRegex()

    let (|InlineComment|_|) : string -> (string*string) option = function
        | s when s.Contains(";") -> 
            let s = s.Split(';') 
            printfn "Inline Comment Tokens ====> %A" s
            (s.[0].Trim(), s.[1].Trim()) |> Some
        | _ -> None

    let groups (r:Regex) s = let g = r.Match(s).Groups in (g.[1].Value, g.[2].Value)
        
    lines |> Seq.fold (fun (i:int, inputs:DLXInput list) line ->
        //printfn "Line: %A" line
        (line.Trim(), pc) |> function
        | _ when line.Length <= 1 ->
            (i, inputs)
        | Patterns.Comment comment ->
            //printfn "Comment match : %A" line
            (i + 1, inputs @ [DLXInput.Comment comment])
        | Patterns.Label (ste, rest) ->
            //printfn "Matched Label: %A, %A" ste rest
            st.Add(ste)
            let lbl = Label.Reference(ste)
            //printfn "Symbol Table : %A" (st)
            let data, comment = rest |> function | InlineComment c -> c | _ -> rest, rest
            let data = data.Trim()
            let rest = rest.Trim()
//            printfn "data, comment = %A, %A" data comment
//            printfn "data, rest = %A, %A" data rest
            (data, pc) |> function
            | Patterns.Instruction info instruction ->
                //printfn "Matched Instruction"
                let instruction = DLXInput.Instruction(!pc, Some lbl, instruction, comment)
                (i + 1, inputs @ [instruction])
            | Patterns.Directive directive ->
                let d = 
                    directive |> List.mapi (fun i d -> 
                        DLXInput.Directive((if i = 0 then Some lbl else None), d))
                (i + 1, inputs @ d)
            | _ when rest.StartsWith(";") ->
                ("nop", pc) |> function
                | Patterns.Instruction info instruction ->
                    printfn "Trying to make NOP"
                    let instruction = DLXInput.Instruction(!pc, Some lbl, instruction, comment)
                    (i + 1, inputs @ [instruction])
                | _ -> failwith "Fail creating nop"
            | _ -> failwith "Failed to match info after matching label"

        | Patterns.Directive directive -> 
            //printfn "Match Directive"
            let d = directive |> List.mapi (fun i d -> DLXInput.Directive(None, d))
            (i + 1, inputs @ d)
        | Patterns.Instruction info instruction ->
            //printfn "Instruction Match (no label):  %A" line
            let instruction = DLXInput.Instruction(!pc, None, instruction, "deeerrrppp")
            (i + 1, inputs @ [instruction])
        | _ -> failwith "Couldn't create dlx input"
        ) (0, List.empty<DLXInput>)
        

type Assembler(dlxfile:string) =
    do dlxfile |> function
    | _ when not (dlxfile.EndsWith(".dlx")) -> failwith "invalid input file"
    | _ -> ()

    static let b2hmap = 
        let hex = 
            ['0'..'9'] @ ['a'..'f']
            |> List.map string
        let bin = [for i in 0..15 -> Convert.ToString(i, 2).PadLeft(4, '0')]
        (bin, hex) ||> List.zip 
        |> Map.ofList
    
    let byte2hex (s:string) = (b2hmap.[s.Substring(0, 4)] + b2hmap.[s.Substring(4, 4)])
    
    let bin2hex (s:string) = 
        //printfn "%A" (s.ToString())
        s |> function
        | _ when s.Length = 32 ->
            let b0 = s.Substring(0,8) |> byte2hex
            let b1 = s.Substring(8,8) |> byte2hex
            let b2 = s.Substring(16,8) |> byte2hex
            let b3 = s.Substring(24,8) |> byte2hex
            //printfn "bytes: %s, %s, %s, %s" b0 b1 b2 b3
            (b0 + b1 + b2 + b3)
        | _ -> failwith "binary string must be length 32"
    

    let hexfile = Path.ChangeExtension(dlxfile, ".hex")

    let removeEmptyLines (lines:seq<string>) =
        lines |> Seq.filter (fun l -> l |> String.IsNullOrEmpty |> not)

    let opcodeInfo = OpcodeInfo()

    let symtab = new SymbolTable()
    
    let assemble() =
        let lines = dlxfile |> File.ReadAllLines 
        let pc = ref 0u
        let _, inputs = parseInputs opcodeInfo lines pc symtab
        printfn "Parsed inputs!"
        //for i in inputs do printfn "%A" i

        let newhex (input:DLXInput) = [input.ToString()]

        inputs
        |> Seq.fold (fun (i:int, hex:string list) input -> 
            input |> function
            | DLXInput.Comment comment ->
                (i + 1, hex @ newhex input)
            | DLXInput.Directive(_) -> 
                printfn "Directive!"
//                printfn "Directive (asm) : %A, %A, %A" 
//                    (label.ToString())
//                    (directive.ToString())
//                    (comment.ToString())
//                let newhex = makeOutputLine input pc
                (i + 1, hex @ newhex input)
            | DLXInput.Instruction(pc, label, instruction, comment) ->
                printfn "Instruction!"
                let instruction = instruction |> function
                    | Instruction.IType(opcode, operands) ->
                        printfn "IType!"
                        let operands = operands |> function
                            | Operands.IType(rs1, rd, imm) ->
                                printfn "IType operands!"
                                imm |> function
                                | Immediate.Label lbl ->
                                    printfn "Label Before: %A" lbl
                                    let lbl = lbl.ReplaceWithAddress(symtab)
                                    //printfn "Label After: %A" lbl
                                    Operands.IType(rs1, rd, Immediate.Label lbl)
                                | Immediate.BasePlusOffset(b,o) -> 
                                    //printfn "BPO ==> (%A, %A)" b o
                                    let b,o = 
                                        b.ReplaceWithAddress(symtab),
                                        o.RepalceWithAddress(symtab)
                                    Operands.IType(rs1, rd, Immediate.BasePlusOffset(b,o))
                                | _ -> operands
                            | _ -> operands
                        printfn "I Made it!"
                        Instruction.IType(opcode, operands)
                    | Instruction.JType(opcode, operands) -> 
                        let operands = operands |> function
                            | Operands.JType(imm) -> imm |> function
                                | Immediate.Label lbl ->
                                    let lbl = lbl.ReplaceWithAddress(symtab)
                                    Operands.JType(Immediate.Label lbl)
                                | Immediate.BasePlusOffset(b,o) ->
                                    let b,o =
                                        b.ReplaceWithAddress(symtab),
                                        o.RepalceWithAddress(symtab)
                                    Operands.JType(Immediate.BasePlusOffset(b,o))
                                | _ -> operands
                            | _ -> operands
                        Instruction.JType(opcode, operands)
                    | Instruction.RType(_) -> instruction
                let newinput = DLXInput.Instruction(pc, label, instruction, comment)
                (i + 1, hex @ newhex newinput)
        ) (0, List.empty<string>)
        |> function | (_, out) -> out

    let createOutput (lines:string list) =
        (lines |> List.fold (fun s l -> s + l + "\n") (""))
        

    member val DlxFile = dlxfile
    member val HexFile = hexfile

    member asm.Run(outpath:string, ?verbose:bool) =
        let verbose = defaultArg verbose false
        //let assemble() = if verbose then assembleVerbose() else assemble()
        
        let outpath = outpath @@ Path.GetFileName(hexfile)
        
        let output = assemble() |> createOutput
//            let lines = assemble()
//            lines |> List.fold (fun (lineNumber, outstring) line -> 
//                (lineNumber = lines.Length - 1) |> function
//                | false -> (lineNumber + 1, outstring + line + "\n") 
//                | true -> (lineNumber + 1, outstring + line)
//                ) (0, "")
//            |> snd
        if File.Exists(outpath) then File.Delete(outpath)
        File.AppendAllText(outpath, output)
    
    member asm.Run() = asm.Run("/u/css/ab67597/5483/Project1/Tests/")

    interface IDisposable with
        member this.Dispose() = ()
        