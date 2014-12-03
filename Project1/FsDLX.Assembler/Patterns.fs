module FsDLX.Assembler.Patterns

open System
open System.Text
open System.Text.RegularExpressions

open Grammar
//
type InputRegex() =
    member val Comment = Regex(@"(;)(.*)")
    member val Directive = Regex(@"\.(\w+)\s(.*)")
    member val Label = Regex(@"\A(\w+):(.*)")
    member val Instruction = Regex(@"\A(\w+)([^;]*)")

type DirectiveRegex() =
    member val Text    = Regex(@"(?<=\.(text))(?: 0x)?(\d*)")
    member val Data    = Regex(@"(?<=\.(data))(?: 0x)?(\d*)")
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




let (|Instruction|_|) (info:OpcodeInfo) : (string*uint32 ref) -> Instruction option =
    let o = OpcodeRegex(info)
    let imm = ImmediateRegex()
    let i = InputRegex()
    
    let storeRegex = Regex(@"(?<offset>-?\d)\((?<rs1>r\d\d?)\), (?<rd>[rf]\d\d?)")

    let (|IType|_|) (info:OpcodeInfo) : (string*string) -> (Opcode*Operands) option =

        let rri = Regex(@"\A(?<rd>r\d\d?), (?<rs1>r\d\d?), (?<imm>\d+)$")
        let ri = Regex(@"\A(?<rd>r\d\d?),(?<rs1>) (?<imm>\d+)$")
        let rbpo = Regex(@"\A(?<rd>[rf]\d\d?), (?<offset>-?\d+)\((?<rs1>r\d\d?)\)$")
        let rl = Regex(@"\A(?<rd>[rf]\d\d?),(?<rs1>) (?<imm>\w+)$")
        let rrl = Regex(@"\A(?<rd>r\d\d?), (?<rs1>r\d\d?), (?<label>\w+)$")
        let bpor = Regex(@"(?<offset>-?\d+)\((?<rs1>r\d\d?)\), (?<rd>[rf]\d\d?)")
        let lr = Regex(@"(?<label>\w+), (?<rd>[rf]\d\d?)")
        let r = Regex(@"\A(?<rs1>r\d\d?)$")
        

        function
        | (opcode, operands) when opcode |> matches o.IType ->
            Some( 
                
                Opcode.IType(opcode, info.Lookup(opcode) |> int),
                Operands.IType(operands.Trim() |> function
                    | s when s |> matches rri -> 
                        //printfn "s matches rri =======> %A" s
                        let g = rri.Match(s).Groups in (Register.RS1 g.["rs1"].Value, Register.RD g.["rd"].Value, Immediate.Value (g.["imm"].Value |> int))
                    | s when s |> matches ri -> 
                        //printfn "s matches ri =======> %A" s
                        let g = ri.Match(s).Groups in (Register.Unused, Register.RD g.["rd"].Value, Immediate.Value (g.["imm"].Value |> int))
                    | s when s |> matches rbpo ->
                        //printfn "s matches rbpo =====> %A" s
                        let g = rbpo.Match(s).Groups in (Register.RS1 g.["rs1"].Value, Register.RD g.["rd"].Value, Immediate.Value( (g.["offset"].Value |> int)))
                    | s when s |> matches rl ->
                        //printfn "s matches rl =======> %A" s
                        let g = rl.Match(s).Groups in (Register.Unused, Register.RD g.["rd"].Value, Immediate.Label (Label.Inline g.["imm"].Value))
                    | s when s |> matches rrl ->
                        //printfn "s matches rrl =======> %A" s
                        let g = rrl.Match(s).Groups in (Register.RS1 g.["rs1"].Value, Register.RD g.["rd"].Value, Immediate.Label (Label.Inline g.["label"].Value))
                    | s when s |> matches bpor ->
                        //printfn "s matches bpor =======> %A" s
                        let g = bpor.Match(s).Groups in (Register.RS1 g.["rs1"].Value, Register.RD g.["rd"].Value, Immediate.Value((g.["offset"].Value |> int)))
                    | s when s |> matches lr ->
                        //printfn "s matches bpor =======> %A" s
                        let g = lr.Match(s).Groups in (Register.Unused, Register.RD g.["rd"].Value, Immediate.Label (Label.Inline g.["label"].Value))
                    | s when s |> matches r ->
                        let g = r.Match(s).Groups in (Register.RS1 g.["rs1"].Value, Register.Unused, Immediate.Unused)
                    | s -> failwith (sprintf "Unable to match IType operands from %s." s)
                ))
        | _ -> None
    
    let (|RType|_|) (info:OpcodeInfo) : (string*string) -> (Opcode*Operands) option =
        let rrr = Regex(@"(?<rd>r\d\d?), (?<rs1>r\d\d?), (?<rs2>r\d\d?)")
        let fff = Regex(@"(?<rd>f\d\d?), (?<rs1>f\d\d?), (?<rs2>f\d\d?)")
        let ff = Regex(@"(?<rd>[rf]\d\d?), (?<rs1>[rf]\d\d?)")
            
        let regs (r:Regex) s = let g = r.Match(s).Groups in (Register.RS1 g.["rs1"].Value, Register.RS2 g.["rs2"].Value, Register.RD g.["rd"].Value)

        function
        | (opcode, operands) when opcode |> matches o.RType ->
            Some(
                Opcode.RType(opcode, info.Lookup(opcode) |> int),
                Operands.RType(operands.Trim() |> function
                    | s when s |> matches rrr -> s |> regs rrr
                    | s when s |> matches fff -> s |> regs fff
                    | s when s |> matches ff -> 
                        //printfn "s matches ff =====> %A" s
                        let g = ff.Match(s).Groups in (Register.RS1 g.["rs1"].Value, Register.Unused, Register.RD g.["rd"].Value)
                    | _ -> failwith "failed getting rtype operands"
                    ))
        | _ -> None

    let (|JType|_|) (info:OpcodeInfo) : (string*string) -> (Opcode*Operands) option =
        let l = Regex(@"(?<label>\w+)")
        let name (r:Regex) s = let g = r.Match(s).Groups in Immediate.Label(Label.Inline g.["label"].Value)

        function
        | (opcode, operands) when opcode |> matches o.JType ->
            Some(
                Opcode.JType(opcode, info.Lookup(opcode) |> int),
                Operands.JType(operands.Trim() |> function
                    | s when s |> matches l -> s |> name l
                    | s -> failwith (sprintf "Failed to create JType operands from %s" s)
                    ))
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
            | "trap", _ ->              Instruction.JType(Opcode.JType(opcode, info.Lookup(opcode) |> int), Operands.TRAP(operands))
            | "nop", _ ->               Instruction.RType(rrx opcode, Operands.NOP(), unused opcode, func opcode)
            | IType info (op, ops) ->   Instruction.IType(op, ops)    
            | RType info (op, ops) ->   Instruction.RType(rrx opcode, ops, unused opcode, func opcode)
            | JType info (op, ops) ->   Instruction.JType(op, ops)
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
            //printfn "Text directive: %A" s
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
            if s.Length > 1 then pc := Convert.ToUInt32(s, 16)
            (pc', s, "")) 
        |> List.map (fun e -> new Directive(e))
        |> List.choose (fun d -> if d.Data.Length > 1 then Some d else None)
        |> Some

    | s, pc when s |> matches d.Align ->
        groups d.Align s |> List.map (fun (s:string) ->
            let pc' = !pc
            let a = int s
            pc := !pc <<< a
            //while pc' % a <> 0u do pc := !pc * 2u
//            pc := if !pc <> 0u then !pc - 1u else !pc
            //pc := if int (pc' - 1u) > 0 then pc' - 1u else pc'
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