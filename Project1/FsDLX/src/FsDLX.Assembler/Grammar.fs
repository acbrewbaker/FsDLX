module FsDLX.Grammar

open System
open System.Text
open System.Text.RegularExpressions
open Patterns


//
//type DLXInput =
//    | Comment of Comment
//    | Instruction of Instruction * Comment option
//    | Directive of Directive * Comment option
//
//    static member Parse = function 
//        | Patterns.Input.Comment s -> Comment.Parse s
//        | _ -> failwith ""
////        | Patterns.Input.Instruction s -> Instruction s
////        | Patterns.Input.Directive s -> Directive.Parse s
//            
//
//and Comment =
//    | CommentOnly of string
//    | Inline of string
//
//    static member Parse (input:string) : Comment option =
//        input.Split(';') |> function
//        | tokens when tokens.Length > 1 -> input |> Inline |> Some
//        | tokens when tokens.Length = 1 -> input |> CommentOnly |> Some
//        | _ -> None
//
//and Instruction =
//    | IType of Opcode * Operands
//    | RType of Opcode * Operands
//    | JType of Opcode * Operands
//
////    static member Parse : (string*string -> DLXInput) = function 
////        | Patterns.Instruction.IType (op,ops) -> 
////            let rs1, rd, imm = ops.[0], ops.[1], ops.[2]
////            (Opcode.IType op, Operands.Encode ops)
////        | Patterns.Instruction.RType (op,ops) -> Opcode.RType op
////        | Patterns.Instruction.JType (op,ops) -> Opcode.IType op
////        | _ -> failwith ""
//
//and Directive =
//    | Text of int
//    | Data of int
//    | Asciiz of string[]
//    | Double of string[]
//    | Float of string[]
//    | Word of int
//    | Space of int
//
//    static member Parse : (string -> DLXInput) = function
//        | Patterns.Directive.Text s ->
//            DLXInput.Directive (Text (int s), s |> Comment.Parse)
//        | Patterns.Directive.Asciiz s -> 
//            DLXInput.Directive (s.Split(' ') |> Asciiz, Comment.Parse s)        
//
//and Opcode =
//    | IType of string
//    | RType of string
//    | JType of string
//
//    static member Encode(opcodes:Opcodes) : Opcode -> Opcode = 
//        let convert (op:string) = Convert.ToString(opcodes.Lookup(op) |> int, 2)
//        function
//        | IType opcode ->  convert opcode |> IType
//        | RType opcode -> convert opcode |> RType
//        | JType opcode -> convert opcode |> JType
//
//    //static member Encode = Opcode.Encode(Opcodes())
//
//and Operands =
//    | IType of Register * Register * Immediate
//    | RType of RRX * Register * Register * Register
//    | JType of Immediate
//
//    static member Encode (tokens:string[]) : Opcode -> Operands = 
//        let reg i = tokens.[i] |> Register.Encode
//        let imm i = tokens.[i] |> Immediate.Encode
//        function
//        | Opcode.IType s -> IType(reg 1, reg 2, imm 3)
//        | Opcode.JType s -> JType(imm 1)
//        | op -> RType(RRX.Encode op, reg 1, reg 2, reg 3)
//        
//        
//
//
//and Register =
//    | R of string
//    | F of string
//
//    static member Encode : string -> Register = function
//        | Patterns.Register.R s -> R s
//        | Patterns.Register.F s -> F s
//
//and RRX =
//    | RRAlu of string
//    | RRFpu of string
//
//    static member Encode(opcodes:Opcodes) : Opcode -> RRX = function
//        | Opcode.RType opcode -> 
//            opcodes.RTypes.[opcode] |> fst |> function
//            | s when int s = 0 -> s.PadLeft(5,'0') |> RRAlu
//            | s when int s = 1 -> s.PadLeft(5, '0') |> RRFpu
//            | _ -> failwith ""
//        | _ -> failwith "" 
//
//    static member Encode op = RRX.Encode(Opcodes()) op
//
//and Immediate =
//    | Value of Value
//    | Label of Label
//    | Register of Register
//    | BasePlusOffset of BasePlusOffset
//        
//    static member Encode : string -> Immediate = function
//        | Patterns.Immediate.Value s -> Value (uint32 s)
//        | Patterns.Immediate.Label s -> Label (Inline s)
//        | Patterns.Immediate.Register s -> s |> Register.Encode |> Register
//        | _ -> failwith ""
//        //| Patterns.Immediate.BasePlusOffset s -> BasePlusOffset s
//
//and Label =
//    | Reference of string
//    | Inline of string
//
//and BasePlusOffset =
//    | ValPlusLabel of Value * Label
//    | ValPlusReg of Value * Register
//
//    static member Encode : string * string -> BasePlusOffset = function
//        | v, l -> ValPlusLabel (v |> uint32, Inline l)
//        //| _ -> ValPlusReg(0u, Register.Encode "r0")
//        
//
//and Value = uint32


//type Derp =
//    | Value of v:int
//
//
//
////type ProgramCounter =
////    | Value of uint32
////    | Hex of string
////
////    member pc.ForHexOutput = pc |> function
////        | Value v -> pc.ToString() + ": "
////        | Hex h -> h + ": "
////    
////    member pc.ToBinary() = pc |> function
////        | _ -> Convert.ToString(pc.ToUint32() |> int, 2)
////
////    member pc.ToUint32() = pc |> function
////        | Value v -> v
////        | Hex h -> Convert.ToUInt32(h, 16)
////
////    member pc.GetNewPC(oldpc:uint32) = pc |> function
////        | Value v -> ProgramCounter.Value(v + oldpc)
////        | _ -> ProgramCounter.Value(pc.ToUint32() + oldpc)
////
////    member pc.GetNewPC() = pc |> function
////        | Value v -> ProgramCounter.Value(v + 4u)
////        | _ -> ProgramCounter.Value(pc.ToUint32() + 4u)
////
////    override pc.ToString() = pc |> function
////        | Value v -> pc2hex v
////        | Hex h -> h
////
////type Label = string
////
////type Address =
////    | String of string
////    | Value of uint32
//type DLXLine =
//    | CommentOnly
//
//type DLXObject =
//    | Opcode of Opcode
//    | Register of Register
//
//    member dlx.ToBinary() = dlx |> function
//        | Opcode o -> o.ToBinary()
//        | Register r -> r.ToBinary()
//    
//and Opcode = { Name : string; Encoding : int }
//    with member o.ToBinary() = Convert.ToString(o.Encoding, 2).PadLeft(6, '0')
//
//and Register = { Name : string; Encoding : int}
//    with member o.ToBinary() = Convert.ToString(o.Encoding, 2).PadLeft(5, '0')
//
//
////type  =
////    |
////
////
////
//
//type MatchFunction = Regex -> string -> GroupCollection
//type ConversionFunction = AssemblerState -> string -> AssemblerState
//
//type Opcode =
//    | IType of string
//    | RType of string
//    | JType of string
//    
//    member o.Encode(?opcodes:Support.Opcodes) = 
//        let opcodes = defaultArg opcodes (Support.Opcodes())
//        let encoding = o |> function
//            | IType op -> opcodes.Lookup(op)
//            | RType op -> opcodes.Lookup(op)
//            | JType op -> opcodes.Lookup(op)
//        Convert.ToString(int encoding, 2).PadLeft(6, '0')
//
////    member o.ToBinary(?opcodes:Support.Opcodes) = 
////        let opcodes = defaultArg opcodes (Support.Opcodes())
////        o.Encode(opcodes)
////
////    member o.ToHex() = 
////        let opcodes = defaultArg opcodes (Support.Opcodes())
////        Convert.ToUInt32()
//
//    static member Init (groups:GroupCollection) =
//        printfn "%A" groups
//        (   groups.["itype"].Value |> str2option,
//            groups.["rtype"].Value |> str2option,
//            groups.["jtype"].Value |> str2option) |> function
//        | Some opcode, None, None -> IType(opcode)
//        | None, Some opcode, None -> RType(opcode)
//        | None, None, Some opcode -> JType(opcode)
//        | _ -> failwith "couldn't create valid opcode DU"
//
////type RRU =
////    | Integer = 0
////    | FloatingPoint = 1
//
//type Register =
//    | String of string
//    | Value of string
//
//    member reg.Encode() = reg |> function
//        | String r -> (reg2bin r).PadLeft(5, '0')
//        | _ -> "Value type of Register DU not used yet"
//
//type Immediate =
//    | Value of string
//    | Label of Label
//    | Register of Register
//    | BasePlusOffset of string * string
//
////    member imm.Convert(st:SymbolTable, lineNumber:int) = imm |> function
////        | Label lbl -> st.Lookup(x, lineNumber) |> function | Some address -> address | None -> failwith "fail"
////        | Value x -> x.PadLeft(16, '0')
////        | _ -> ""
//
//    member imm.Encode() = 
//        let encoding = imm |> function
//            | Value value -> Convert.ToString((int value), 2)
//            | Label label -> label
//            | Register register -> register.Encode()
//            | BasePlusOffset(b, offset) -> Convert.ToString((int b) + (int offset), 2)
//        encoding.PadLeft(16, '0')
//            
//    member imm.LabelToAddress(st:SymbolTable) = 
//        let update = function | Some (pc:ProgramCounter) -> pc.ToBinary() | None -> failwith ""
//        imm |> function
//        | Label label -> label |> st.Lookup |> update |> Label            
//        | BasePlusOffset(b, offset) -> BasePlusOffset(b, offset |> st.Lookup |> update)
//        | _ -> imm
//        
//    static member InitBasePlusOffset (groups:GroupCollection) =
//        (   groups.["base"] |> group2option,
//            groups.["offset"] |> group2option) |> function
//        | Some b, Some offset -> BasePlusOffset(b, offset)
//        | _ -> failwith "missing base and/or offset"
//
//    static member Init (groups:GroupCollection) = 
//        (   groups.["label"] |> group2option,
//            groups.["register"] |> group2option,
//            groups.["val"] |> group2option) |> function
//        | Some label, None, None -> Label label
//        | None, Some register, None -> register |> Register.String |> Register
//        | None, None, Some value -> Value value
//        | None, None, None -> Immediate.InitBasePlusOffset groups
//        | _ -> failwith "couldn't construct a valid Immediate DU"
//        
//    static member Parse (regex:Regex) (input:string) = ()
//
//type Operands =
//    | IType of Register * Register * Immediate
//    | RType of Register * Register * Register
//    | JType of Immediate
//
//    member ops.Encode() = ops |> function
//        | IType(rs1, rd, imm) -> 
//            rs1.Encode() +
//            rd.Encode() +
//            imm.Encode()
//        | RType(rs1, rs2, rd) -> ""
//        | JType(imm) -> ""
//
//    member ops.LabelToAddress(st:SymbolTable) = ops |> function
//        | IType(rs1, rd, imm) -> IType(rs1, rd, imm.LabelToAddress st)
//        | JType(imm) -> JType(imm.LabelToAddress st)
//        | RType(_) -> ops
//
//    static member Init(groups:GroupCollection) =
//        (   groups.["rs1"].Value |> str2option, 
//            groups.["rs2"].Value |> str2option, 
//            groups.["rd"].Value |> str2option, 
//            groups.["imm"] |> group2option) |> function
//        | Some rs1, None, Some rd, Some imm -> 
//            IType(Register.String rd, Register.String rs1, Immediate.Init groups)
//        | Some rs1, Some rs2, Some rd, _ -> 
//            RType(Register.String rs1, Register.String rs2, Register.String rd)
//        | None, None, None, Some imm -> JType(Immediate.Init groups)
//        | _ -> failwith "couldn't create valid instruction DU"
//
//type Instruction =
//    | Raw of string 
//    | Parsed of Opcode * Operands
//    
//    member i.Parse regex (f:Regex -> string -> GroupCollection) = i |> function
//        | Raw instruction -> 
//            let groups = f regex instruction
//            Parsed(Opcode.Init groups, Operands.Init groups)
//        | Parsed(opcode, operands) -> i
//    
//    member private i.Encode() = i |> function
//        | Parsed(opcode, operands) -> 
//            opcode.Encode() +
//            operands.Encode()
//        | Raw str -> failwith "cant encode raw function!"
//
//    member i.ToHex() = Convert.ToUInt32(i.Encode(), 2).ToString("x8")
//    member i.ToBinary() = i.Encode()
//
//    member i.LabelToAddress(st:SymbolTable) = i |> function 
//        | Parsed(opcode, operands) -> Parsed(opcode, operands.LabelToAddress st)
//        | _ -> i
//
//    static member Init (groups:GroupCollection) =
//        Parsed(Opcode.Init groups, Operands.Init groups)
//
//    static member Match (regex:Regex) (input:string) =
//        Instruction.Init([for m in regex.Matches(input) -> m.Groups].Head)
//        
//
//
//type AssemblerInput =
//    | Comment of string
//    | Directive of ProgramCounter * Label option * string
//    | Instruction of ProgramCounter * Label option * Instruction
//
//    static member Parse (regex:Regex) (line:string) (pc:ProgramCounter) =
//        let matches = regex.Matches(line)
//        let groups = [for m in matches -> m.Groups].Head
//        let label = [for m in matches -> m.Groups.["newlabel"].Value].Head.Replace(":","")
//        printfn "new label: %A" label
//        label |> str2option |> function
//        | Some label -> Instruction(pc, Some label, Instruction.Init groups)
//        | input -> Instruction(pc, None, Instruction.Init groups)