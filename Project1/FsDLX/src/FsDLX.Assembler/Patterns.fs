module FsDLX.Assembler.Patterns

open System
open System.Text
open System.Text.RegularExpressions

open Grammar

type InputRegex() =
    member val Comment = Regex(@"(;)(.*)")
    member val Directive = Regex(@"\.(\w+)\s(.*)")
    member val Label = Regex(@"\A(\w+):(.*)")
    member val Instruction = Regex(@"\A(\w+)([^;]*)")

type DirectiveRegex() =
    member val Text    = Regex(@"(?<=\.(text))(?: 0x)?(\d*)")
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

//type OperandRegex() =
//    member val IType = Regex(@"((r\d\d?), (r\d\d?), (\d+))|((r\d\d?), (r\d\d?), (\w+))|((r\d\d?), (\w+))|(\w+), ([rf]\d\d?)")
//    member val RType = Regex(@"([rf]\d\d?), ([rf]\d\d?)$|([rf]\d\d?), ([rf]\d\d?)$")
//    member val JType = Regex(@"\A(\w+)$")

type ImmediateRegex() =
    member val Value            = Regex(@"(\A[+-]?\d+)$")
    member val Label            = Regex(@"(\D\w[^\(]\w+[^\)])")
    member val Register         = Regex(@"([rf]\d\d?)$")
    member val BasePlusOffset   = Regex(@"([+-]?\d+)\((\w+)\)")

type RegisterRegex() =
    member val R = Regex(@"r\d\d?")
    member val F = Regex(@"f\d\d?")

let register =
    let r = RegisterRegex()

    function
    | reg when reg |> matches r.R -> Register.R reg
    | reg when reg |> matches r.F -> Register.F reg
    | _ -> failwith "Failed to create register"

let baseplusoffset =
    let imm = ImmediateRegex()
    function
    | bv,ol when bv |> matches imm.Value && ol |> matches imm.Value -> 
        Convert.ToInt32(bv), Offset.Label (Label.Inline ol)
    | bv,ov when bv |> matches imm.Value && ov |> matches imm.Value ->
        Convert.ToInt32(bv), Offset.Value (Convert.ToInt32(ov))
    | bv,oreg when bv |> matches imm.Value && oreg |> matches imm.Register ->
        Convert.ToInt32(bv), Offset.Register (register oreg)
    | bv,oreg -> 
        failwith (sprintf "failed to match base plus offset from: (%A, %A)" bv oreg)

let immediate =
    let r = ImmediateRegex()
    
    function
    | imm when imm |> matches r.Value           -> Immediate.Value (Convert.ToInt32(imm))
    | imm when imm |> matches r.Label           -> Immediate.Label (Label.Inline imm)
    | imm when imm |> matches r.Register        -> Immediate.Register (register imm)
    | imm when imm |> matches r.BasePlusOffset  ->
        let b,o = let g = r.BasePlusOffset.Match(imm).Groups in (g.[1].Value, g.[2].Value)
        Immediate.BasePlusOffset (baseplusoffset (b,o))
    | imm -> failwith (sprintf "Failed to create immediate from: %A" imm)

let (|Instruction|_|) (info:OpcodeInfo) : (string*uint32 ref) -> Instruction option =
    let o = OpcodeRegex(info)
    let imm = ImmediateRegex()
    let reg = RegisterRegex()
    let i = InputRegex()
    
    let storeRegex = Regex(@"(?<offset>-?\d)\((?<rs1>r\d\d?)\), (?<rd>[rf]\d\d?)")

    let (|IType|_|) (info:OpcodeInfo) : (string*string) -> (Opcode*Operands) option =
        let (|RRI|RI|R|IF|IR|S|) (ops:string[]) = ops.Length |> function
            | 3 -> RRI ops
            | 2 -> ops |> function
                | _ when (ops.[0] |> matches imm.Label) || (ops.[0] |> matches imm.BasePlusOffset) -> 
                    if ops.[1].StartsWith("f")
                    then IF ops
                    else IR ops
                | _ -> RI ops
            | 1 -> R ops
            | _ -> failwith "no 0 register IType operands"
        
        function
        | (opcode, operands) when opcode |> matches o.IType ->
            let opcode, operands =
                Opcode.IType(info.Lookup(opcode) |> int),
                operands.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) |> function
                    | RRI ops ->
                        Operands.IType(Register.R ops.[1], Register.R ops.[0], immediate ops.[2])
                    | RI ops -> 
                        Operands.IType(Register.R ops.[1], Register.Unused, immediate ops.[0])
                    | R ops -> 
                        Operands.IType(Register.R ops.[0], Register.Unused, Immediate.Unused)
                    | IF ops -> 
                        printfn "ops1, ops0 ==> %A, %A" (ops.[1]) (ops.[0])
                        //let imm,rs1 = let g = imm.BasePlusOffset.Match(ops.[0]).Groups in (g.[1].Value, g.[2].Value)
                        Operands.IType(Register.R ops.[1], Register.F ops.[1] , immediate ops.[0])
                    | IR ops -> 
                        Operands.IType(Register.R ops.[1], Register.Unused, immediate ops.[0])
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
        | (opcode, operands) when opcode |> matches o.RType ->
            let op, ops =
                Opcode.RType(info.Lookup(opcode) |> int),
                operands.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) |> function
                    | RRR ops -> Operands.RType(Register.R ops.[1], Register.R ops.[2], Register.R ops.[0])
                    | FFF ops -> Operands.RType(Register.F ops.[1], Register.F ops.[2], Register.F ops.[0])
                    | RR ops -> Operands.RType(Register.R ops.[1], Register.Unused, Register.R ops.[0])
                    | FF ops -> Operands.RType(Register.F ops.[1], Register.Unused, Register.F ops.[0])
                    | RF ops -> Operands.RType(Register.R ops.[1], Register.Unused, Register.F ops.[0])
            Some (op, ops)
        | _ -> None

    let (|JType|_|) (info:OpcodeInfo) : (string*string) -> (Opcode*Operands) option =
        let r = RegisterRegex()

        let (|R|I|) (ops:string[]) = 
            ops.Length |> function
            | 1 -> ops.[0] |> function | o when o |> matches reg.R -> R o | _ -> I ops.[0]
            | _ -> failwith "failed to match JType operands in active pattern"

        function
        | (opcode, operands) when opcode |> matches o.JType ->
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
        
        let rrx (op:string) = 
            info.GetRRX(op) |> function
            | x when char x = '0' -> RRX.RRalu x
            | x when char x = '1' -> RRX.RRfpu x
            | _ -> failwith "RRX Failed"

        let unused op = rrx op |> function
            | RRX.RRalu _ -> Unused.U5
            | RRX.RRfpu _ -> Unused.U6

        let func op = 
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
                Instruction.IType(op, ops)    
            | RType info (op, ops) -> 
                Instruction.RType(rrx opcode, ops, unused opcode, func opcode)
            | JType info (op, ops) -> 
                Instruction.JType(op, ops)
            | _ -> failwith (sprintf "failed to make instruction from: %A" (opcode, operands))
        Some instruction
    | _ -> None

let (|Directive|_|) : (string*uint32 ref) -> Directive list option =
    let d = DirectiveRegex()
    
    let groups (r:Regex) s = [for m in r.Matches(s) -> m.Groups.[2].Value]
    function
    | s, pc when s |> matches d.Text ->
        groups d.Text s |> List.map (fun (s:string) ->
            let pc' = !pc
            printfn "Text directive: %A" s
            let comment = asComment s
            if s.Length <= 1 
            then pc := 0u 
            else pc := Convert.ToUInt32(s, 16)
            (!pc, s, "")) 
        |> List.map (fun e -> new Directive(e)) 
        |> Some

    | s, pc when s |> matches d.Data ->
        groups d.Data s |> List.map (fun (s:string) ->
            let pc' = !pc
            let s' = s
            let comment = asComment s'
            (pc', s', comment)) 
        |> List.map (fun e -> new Directive(e)) 
        |> Some

    | s, pc when s |> matches d.Align ->
        groups d.Align s |> List.map (fun (s:string) ->
            let pc' = !pc
            let a = uint32 s
            pc := !pc + 4u
            while !pc % a <> 0u do pc := !pc + 4u
            //pc := pc' + n * 4u
            (pc', s, s)) 
        |> List.map (fun e -> new Directive(e))
        |> List.choose (fun d -> if d.Data.Length > 1 then Some d else None)
        |> Some

    | s, pc when s |> matches d.Asciiz ->
        groups d.Asciiz s |> List.map (fun (s:string) ->
            let bytes = Encoding.Default.GetBytes(s)
            let pc' = !pc
            let s' = s |> str2hex
            let comment = strAsComment s
            pc := !pc + uint32 bytes.Length + 1u
            (pc', s', comment)) 
        |> List.map (fun e -> new Directive(e)) 
        |> Some
        
    | s, pc when s |> matches d.Double ->
        groups d.Double s |> List.map (fun (s:string) ->
            let pc' = !pc
            let s' = (Double.Parse(s) |> BitConverter.GetBytes |> bytes2hex)
            let comment = floatingPointAsComment s
            pc := !pc + 8u
            (pc', s', comment)) 
        |> List.map (fun e -> new Directive(e)) 
        |> Some

    | s, pc when s |> matches d.Float ->
        groups d.Float s |> List.map (fun (s:string) ->
            let pc' = !pc
            let s' = (Single.Parse(s) |> BitConverter.GetBytes |> bytes2hex)
            let comment = floatingPointAsComment s
            pc := !pc + 4u
            (pc', s', comment)) 
        |> List.map (fun e -> new Directive(e)) 
        |> Some

    | s, pc when s |> matches d.Word ->
        groups d.Word s |> List.map (fun (s:string) ->
            let pc' = !pc
            let s, isNeg = if s.StartsWith("-") then s.Replace("-",""), true else s, false
            let base' = if s.StartsWith("0x") then 16 else 10
            let word = Convert.ToInt32(s, base') * (if isNeg then -1 else 1)
            let s' = (word |> BitConverter.GetBytes |> bytes2hex)
            let comment = asComment (string word)
            pc := pc' + 4u
            (pc', s', comment)) 
        |> List.map (fun e -> new Directive(e)) 
        |> Some

    | s, pc when s |> matches d.Space ->
        groups d.Space s |> List.map (fun (s:string) ->
            let pc' = !pc
            let s' = s
            let comment = asComment s
            pc := pc' + (uint32 s)
            (pc', s', comment)) 
        |> List.map (fun e -> new Directive(e))
        |> List.choose (fun d -> if d.Data.Length > 2 then Some d else None)
        |> Some

    | _ -> None

let (|Label|_|) : (string*uint32 ref) -> (SymbolTableEntry*string) option =
    let i = InputRegex()
    let groups (r:Regex) s = let g = r.Match(s).Groups in (g.[1].Value, g.[2].Value)

    function
    | (line, pc) when line |> matches i.Label ->
        let lbl, rest = (groups i.Label line)
        //printfn "Label, PC  ===> %A, %A" lbl pc
        (SymbolTableEntry(lbl, int !pc), rest)
        |> Some
    | _ -> None

let (|Comment|_|) : (string*uint32 ref) -> string option =
    function
    | (line, _) when line.StartsWith(";") -> Some line
    | _ -> None     