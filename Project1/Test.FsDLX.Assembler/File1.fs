module Test.FsDLX.Assembler.File1

open System
open System.IO
open System.Text

open FsDLX.Assembler

open NUnit.Framework
open FsUnit


let op2bin (info:OpcodeInfo) (opcode:string) = let enc = info.Lookup(opcode) in Convert.ToString(int enc, 2).PadLeft(6,'0')
let reg2bin (s:string) = Convert.ToString(s.Substring(1) |> int, 2).PadLeft(5, '0')
let imm2bin (s:string) = Convert.ToString(int16 s, 2).PadLeft(16, '0')

let op2rrx2bin (info:OpcodeInfo) (opcode:string) = let rrx = info.GetRRX(opcode) in Convert.ToString(int rrx, 2).PadLeft(6, '0')
let op2func2bin info opcode = 
    let op = op2bin info opcode
    if info.GetRRX(opcode) |> int = 1 then op.Substring(1) else op.Substring(2)
let op2unused2bin (info:OpcodeInfo) opcode = if info.GetRRX(opcode) |> int = 1 then "0000" else "00000"

let name2bin (name:string) = name

let itype info opcode rs1 rd immediate = 
    op2bin info opcode +
    reg2bin rd +
    reg2bin rs1 +
    imm2bin immediate
    |> bin2hex

let rtype info opcode rs1 rs2 rd =
    op2rrx2bin info opcode +
    reg2bin rs1 +
    reg2bin rs2 +
    reg2bin rd +
    op2unused2bin info opcode +
    op2func2bin info opcode
    |> bin2hex

let jtype info opcode name _ _ =
    op2bin info opcode +
    name2bin name
    |> bin2hex
   

let filterComments (dlx:string seq) = 
    dlx 
    |> Seq.fold (fun state (line:string) -> let line = line.Trim() in line |> function
        | _ when line.StartsWith(";") -> state
        | _ when line.Contains(";") -> state @ [line.Split(';').[0].Trim()]
        | _ -> state @ [line])
        (List.empty<string>)



let updateSymbolTable (st:Map<string,int>) = function
    | (pc:int, s:string) when s.Contains(":") -> let label = s.Split(':').[0] in st.Add(label, pc)
    | _ -> st

type pc = int
type st = Map<string,int>
type state = pc * st * string list

let pass1 (dlx:string list) =
    let update (state:state) : string -> state = 
        let pc, st, dlx = state
        function
        | line when line.Contains(":") -> let label = line.Split(':').[0] in pc + 4, st.Add(label, pc), dlx @ [line]
        | line -> pc + 4, st, dlx @ [line]
        | _ -> failwith "" 

    dlx
    |> Seq.fold (fun (state:state) line -> let line = line.Trim() in line |> function
        | _ when line.StartsWith(";") -> state
        | _ when line.Contains(";") -> line.Split(';').[0].Trim() |> update state
        | _ -> line |> update state)
        (0, Map.empty<string, int>, List.empty<string>)


let instruction (info:OpcodeInfo) : string -> string -> string -> string -> string -> string =
    let o = Patterns.OpcodeRegex(info)
    function
    | s when s |> matches o.IType -> itype info
    | s when s |> matches o.RType -> rtype info
    | s when s |> matches o.JType -> jtype info
    | s -> failwith s

[<Test>]
let ``new stuff test`` () =
//    let itype = "opcode,rd,rs1,immediate"

    let info = new OpcodeInfo(srcdir)
    let itype = itype info
    printfn "%A" (info.Lookup("addi"))
//    let op2bin = op2bin info
    let dlx = File.ReadAllLines(inputdir @@ "arithImmed.dlx")
    
    let x = filterComments dlx
    printfn "%A" x
    let i1 = dlx.[0]
    let label, ins = let t = i1.Split(':') in t.[0], t.[1]
    let opcode, operands = let t = ins.Split([|' '|], 2) in t.[0].Trim(), t.[1].Trim()
    //printfn "opcode %A" (info.Lookup(opcode.Trim()))
    let rd, rs1, immediate = let t = operands.Replace(" ", "").Split(',') in t.[0], t.[1], t.[2]
    let instruction = itype opcode rs1 rd immediate
    printfn "%A" instruction
    //let opcode (s:string) = itype.Replace("opcode", )
    ()