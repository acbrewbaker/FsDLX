namespace FsDLX.Common

open System.IO
open System.Text.RegularExpressions
 
        
module OpcodeUtil =

    module Match = 
        let (|IType|_|) op = op |> function
            | "beqz" -> Some op
            | "bnez" -> Some op
            | "addi" -> Some op
            | "addui" -> Some op
            | "subi" -> Some op
            | "subui" -> Some op
            | "andi" -> Some op
            | "ori" -> Some op
            | "xori" -> Some op
            | "lhi" -> Some op
            | "trap" -> Some op
            | "jr" -> Some op
            | "jalr" -> Some op
            | "slli" -> Some op
            | "srli" -> Some op
            | "srai" -> Some op
            | "seqi" -> Some op
            | "snei" -> Some op
            | "slti" -> Some op
            | "sgti" -> Some op
            | "slei" -> Some op
            | "sgei" -> Some op
            | "lb" -> Some op
            | "lh" -> Some op
            | "lw" -> Some op
            | "lbu" -> Some op
            | "lhu" -> Some op
            | "lf" -> Some op
            | "ld" -> Some op
            | "sb" -> Some op
            | "sh" -> Some op
            | "sw" -> Some op
            | "sf" -> Some op
            | "sd" -> Some op
            | _ -> None

        let (|RType|_|) op = op |> function
            | "nop" -> Some op
            | "sll" -> Some op
            | "srl" -> Some op
            | "sra" -> Some op
            | "add" -> Some op
            | "addu" -> Some op
            | "sub" -> Some op
            | "subu" -> Some op
            | "and" -> Some op
            | "or" -> Some op
            | "xor" -> Some op
            | "seq" -> Some op
            | "sne" -> Some op
            | "slt" -> Some op
            | "sgt" -> Some op
            | "sle" -> Some op
            | "sge" -> Some op
            | "movf" -> Some op
            | "movd" -> Some op
            | "movfp2i" -> Some op
            | "movi2fp" -> Some op
            | "addf" -> Some op
            | "subf" -> Some op
            | "multf" -> Some op
            | "divf" -> Some op
            | "addd" -> Some op
            | "subd" -> Some op
            | "multd" -> Some op
            | "divd" -> Some op
            | "cvtf2d" -> Some op
            | "cvtf2i" -> Some op
            | "cvtd2f" -> Some op
            | "cvtd2i" -> Some op
            | "cvti2f" -> Some op
            | "cvti2d" -> Some op
            | "mult" -> Some op
            | "div" -> Some op
            | "multu" -> Some op
            | "divu" -> Some op
            | _ -> None

        let (|JType|_|) op = op |> function
            | "j" -> Some op
            | "jal" -> Some op
            | _ -> None

    module Lookup =
        let byName = 
            [   "beqz", 4
                "bnez", 5
                "addi", 8
                "addui", 9
                "subi", 10
                "subui", 11
                "andi", 12
                "ori", 13
                "xori", 14
                "lhi", 15
                "trap", 17
                "jr", 18
                "jalr", 19
                "slli", 20
                "srli", 22
                "srai", 23
                "seqi", 24
                "snei", 25
                "slti", 26
                "sgti", 27
                "slei", 28
                "sgei", 29
                "lb", 32
                "lh", 33
                "lw", 35
                "lbu", 36
                "lhu", 37
                "lf", 38
                "ld", 39
                "sb", 40
                "sh", 41
                "sw", 43
                "sf", 46
                "sd", 47
                "nop", 0
                "sll", 4
                "srl", 6
                "sra", 7
                "add", 32
                "addu", 33
                "sub", 34
                "subu", 35
                "and", 36
                "or", 37
                "xor", 38
                "seq", 40
                "sne", 41
                "slt", 42
                "sgt", 43
                "sle", 44
                "sge", 45
                "movf", 50
                "movd", 51
                "movfp2i", 52
                "movi2fp", 53
                "addf", 0
                "subf", 1
                "multf", 2
                "divf", 3
                "addd", 4
                "subd", 5
                "multd", 6
                "divd", 7
                "cvtf2d", 8
                "cvtf2i", 9
                "cvtd2f", 10
                "cvtd2i", 11
                "cvti2f", 12
                "cvti2d", 13
                "mult", 14
                "div", 15
                "multu", 22
                "divu", 23
                "j", 2
                "jal", 3 ] |> Map.ofList

        let itypeByEnc= 
            [   4, "beqz"
                5, "bnez"
                8, "addi"
                9, "addui"
                10, "subi"
                11, "subui"
                12, "andi"
                13, "ori"
                14, "xori"
                15, "lhi"
                17, "trap"
                18, "jr"
                19, "jalr"
                20, "slli"
                22, "srli"
                23, "srai"
                24, "seqi"
                25, "snei"
                26, "slti"
                27, "sgti"
                28, "slei"
                29, "sgei"
                32, "lb"
                33, "lh"
                35, "lw"
                36, "lbu"
                37, "lhu"
                38, "lf"
                39, "ld"
                40, "sb"
                41, "sh"
                43, "sw"
                46, "sf"
                47, "sd" ] |> Map.ofList
        
        let rtypeByEnc =
            [|
                [   0, "nop"
                    4, "sll"
                    6, "srl"
                    7, "sra"
                    32, "add"
                    33, "addu"
                    34, "sub"
                    35, "subu"
                    36, "and"
                    37, "or"
                    38, "xor"
                    40, "seq"
                    41, "sne"
                    42, "slt"
                    43, "sgt"
                    44, "sle"
                    45, "sge"
                    50, "movf"
                    51, "movd"
                    52, "movfp2i"
                    53, "movi2fp" ]
                    |> Map.ofList;
               [    0, "addf"
                    1, "subf"
                    2, "multf"
                    3, "divf"
                    4, "addd"
                    5, "subd"
                    6, "multd"
                    7, "divd"
                    8, "cvtf2d"
                    9, "cvtf2i"
                    10, "cvtd2f"
                    11, "cvtd2i"
                    12, "cvti2f"
                    13, "cvti2d"
                    14, "mult"
                    15, "div"
                    22, "multu"
                    23, "divu" ] |> Map.ofList 
            |]

        let jtypeByEnc =
            [   2, "j"
                3, "jal" ] |> Map.ofList
    


type Opcode(op:string, enc:int) =

    static let isRType (hex:string) =
        Convert.hex2bits2int hex 0 (Constants.nOpcodeBits - 1) |> function
            | rru when rru = 0 -> true, rru
            | rru when rru = 1 -> true, rru 
            | _ -> false, 2

    
    static let getOpcodeBits (hex:string) =
        let bin = Convert.hex2bin hex
        let isRType, rru = isRType hex
        if      not(isRType)
        then    bin.[0..Constants.nOpcodeBits - 1], rru
        else    bin.[26..31], rru

//    static let getTrapFunc (hex:string) =
//        Convert.hex2bits2int hex  27 31 |> function
//        | 0 -> "halt"
//        | 1 -> "dumpgpr"
//        | 2 -> "dumpfpr"
//        | 3 -> "dumpstr"
//        | _ -> failwith "invalid trap function"

    member val Name = op with get, set
    member val asInt = enc with get 
    member val asHex = Convert.int2hex enc with get
    member val asBin = Convert.int2bin enc


    override o.ToString() = o.Name

    static member ofName name = Opcode(name, OpcodeUtil.Lookup.byName.[name])
    
    static member ofInstructionHex hex = 
        let bits, rru = getOpcodeBits hex
        let enc = Convert.bin2int(bits)
        if rru <> 2 then
            Opcode.ofName(OpcodeUtil.Lookup.rtypeByEnc.[rru].[enc])
        elif OpcodeUtil.Lookup.itypeByEnc.ContainsKey(enc) then
            Opcode.ofName(OpcodeUtil.Lookup.itypeByEnc.[enc])
        elif OpcodeUtil.Lookup.jtypeByEnc.ContainsKey(enc) then
            Opcode.ofName(OpcodeUtil.Lookup.jtypeByEnc.[enc])
        else
            failwith "opcode lookup failure"
    
    static member ofInstructionInt i = Opcode.ofInstructionHex(Convert.int2hex i)

    static member Opt2String (o:Opcode option) = o |> function
        | Some o -> sprintf "%s" o.Name
        | None -> sprintf "%O" o