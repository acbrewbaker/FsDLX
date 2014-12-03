[<AutoOpen>]
module Test.FsDLX.Assembler.Common

open System
open System.IO
open FsDLX.Assembler
open NCrunch.Framework
open NUnit.Framework
open FsUnit

let srcdir = 
    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
        Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
    else 
        Environment.CurrentDirectory

let inputdir = srcdir @@ "../Inputs"

let testdir    = srcdir @@ "../Tests"

let printi op rs1 rd imm = printfn "op,rs1,rd,imm ==> %s,%s,%s,%s" op rs1 rd imm


let tobin (s:string) = Convert.ToString(s |> int, 2)

let carray2string (c:char[]) = c |> Array.fold (fun s c -> string c + s) ("")

let revstr (s:string) = s.ToCharArray() |> Array.rev |> Array.fold(fun s c -> s + string c) ("")

let str2bytes (s:string) =
    [   s.[0..7]
        s.[8..15]
        s.[16..23]
        s.[24..31] ]

let hex = ['0'..'9'] @ ['a'..'f']
let bin = 
    [
        "0000"
        "0001"
        "0010"
        "0011"
        "0100"
        "0101"
        "0110"
        "0111"
        "1000"
        "1001"
        "1010"
        "1011"
        "1100"
        "1101"
        "1110"
        "1111"
    ]

let b2hmap = 
    let hex = 
        ['0'..'9'] @ ['a'..'f']
        |> List.map string
    let bin = [for i in 0..15 -> Convert.ToString(i, 2).PadLeft(4, '0')]
    (bin, hex) ||> List.zip 
    |> Map.ofList

let byte2hex (s:string) = (b2hmap.[s.Substring(0, 4)] + b2hmap.[s.Substring(4, 4)])
    
//let bin2hex (s:string) = 
//    s |> function
//    | _ when s.Length = 32 ->
//        let b0 = s.Substring(0,8) |> byte2hex
//        let b1 = s.Substring(8,8) |> byte2hex
//        let b2 = s.Substring(16,8) |> byte2hex
//        let b3 = s.Substring(24,8) |> byte2hex
//        (b0 + b1 + b2 + b3)
//    | _ -> failwith "binary string must be length 32"

let bin2hex (s:string) = 
    s |> function
    | _ when s.Length % 4 <> 0 -> failwith "binary string length needs to be multiple of 4"
    | _ ->
        let nibbles = s.Length / 4
        [0..nibbles-1] 
        |> List.map (fun nib -> s.Substring(nib * 4, 4)) 
        |> List.map nibble2hex
        |> List.reduce (+)
//    s |> function
//    | _ when s.Length = 32 ->
//        let b0 = s.Substring(0,8) |> byte2hex
//        let b1 = s.Substring(8,8) |> byte2hex
//        let b2 = s.Substring(16,8) |> byte2hex
//        let b3 = s.Substring(24,8) |> byte2hex
//        (b0 + b1 + b2 + b3)
    | _ -> failwith (sprintf "failed binary to hex conversion of: %A, of length %d\nbinary string must be length 32!" s s.Length)

let [<Test>] `` dir test `` () =
    printfn "srcdir ===> %A" srcdir
    printfn "inputdir ===> %A" inputdir

let [<Test>] `` asm test `` () =
    let op = "8"
    let rs1 = "2"
    let rd = "1"
    let imm = "3"
    
    printi op rs1 rd imm
    let op' = (op |> tobin).PadLeft(6,'0')
    let rs1' = (rs1 |> tobin).PadLeft(5,'0')
    let rd' = (rd |> tobin).PadLeft(5,'0')
    let imm' = (imm |> tobin).PadLeft(16,'0')
    printfn "op, rs1, rd, imm = %s, %s, %s, %s" op rs1 rd imm
    printfn "op', rs1', rd', imm' = %s, %s, %s, %s" op' rs1' rd' imm'
    let s = op' + rs1' + rd' + imm'
    
    let itypeIn = op, rd, rs1, imm
    
    printfn "S: %A" s
    let b0 = s.Substring(0,8)
    let b1 = s.Substring(8,8)
    let b2 = s.Substring(16,8)
    let b3 = s.Substring(24,8)
    printfn "b0 ==> %A" b0
    printfn "b1 ==> %A" b1
    printfn "b2 ==> %A" b2
    printfn "b3 ==> %A" b3
    let b0' = byte2hex b0
    let b1' = byte2hex b1
    let b2' = byte2hex b2
    let b3' = byte2hex b3
    printfn "b0' ==> %A" b0'
    printfn "b1' ==> %A" b1'
    printfn "b2' ==> %A" b2'
    printfn "b3' ==> %A" b3'
    
    let actual = bin2hex s
    printi op' rs1' rd' imm'
    let expected = "20410003"
    expected |> should equal actual

[<Test>]
let ``asdfasdf`` () = 
    let opcode = "sd"
    let rs1 = "-8", "3"
    let rd = "10"

    let info = new OpcodeInfo(srcdir)

    let enc = Convert.ToString(info.Lookup(opcode) |> int, 2)

//    let rs1 = 
//        Convert.ToString(((rs1 |> fst |> int) + (rs1 |> snd |> int)), 2)


    printfn "enc, rs1, rd ===> %A, %A, %A" enc rs1 rd

    let o, b = rs1

    printfn "basdfffffffffffffff %A" (uint32 (int o + int b))
    let y = (uint32 (int o + int b))

    printfn "y is ====> %A" (Convert.ToString(int y, 2))

    let op = enc
    let rs1 = Convert.ToString(b |> int, 2).PadLeft(5, '0')
    let rd = Convert.ToString(rd |> int, 2).PadLeft(5, '0')
    let imm = 
        Convert.ToString(o |> int16,2)

    let s = op + rs1 + rd + imm

    printfn "op %A" op
    printfn "rs1 %A" rs1
    printfn "rd %A" rd
    printfn "imm %A" imm

    printfn "op len %A" (op.Length)
    printfn "rs1 len %A" (rs1.Length)
    printfn "rd len %A" (rd.Length)
    printfn "imm len %A" (imm.Length)

    printfn "result s: %A" s
    printfn "s.length %A" (s.Length)

    printfn "bin2hex: %A" (s |> bin2hex)
    
    printfn "asdfas:%A" (imm |> bin2hex)

    let actual = s |> bin2hex
    let expected = "bc6afff8"

    actual |> should equal expected


[<Test>]
let ``func gen`` () =
    let info = new OpcodeInfo(srcdir)

    let itypes, rtypes, jtypes = 
        info.ITypes.Info,
        info.RTypes.Info,
        info.JTypes.Info

    let itypestr name = 
        sprintf "
let %s opcode rs1 rd imm = ()" name

    let rtypestr name = 
        sprintf "
let %s rrx rs1 rs2 rd = ()" name

    let jtypestr name = 
        sprintf "
let %s opcode imm = ()" name


    let genFunc (name:string) (f:string -> string) = f name

    let ifun = itypes |> List.map (fun (op,_,_) -> genFunc op itypestr)
    let rfun = rtypes |> List.map (fun (op,_,_) -> genFunc op rtypestr)
    let jfun = jtypes |> List.map (fun (op,_,_) -> genFunc op jtypestr)


    ifun |> List.iter (printfn "%s")
    rfun |> List.iter (printfn "%s")
    jfun |> List.iter (printfn "%s")


[<Test>]
let ``regex gen`` () =
    let info = new OpcodeInfo(srcdir)

    let opOnly (info:(string*string*string) list) =
        info |> List.map (fun (op,_,_) -> op)

    let itypes, rtypes, jtypes = 
        info.ITypes.Info,
        info.RTypes.Info,
        info.JTypes.Info



    let genRegex (name:string) (f:string -> string) = f name

    let genActivePattern (name:string) =
        sprintf "
let (|%s|_|) = 
    function
    | s when s |> matches r.%s -> Some (groups r.%s s)
    | _ -> None" (name.ToUpper()) (name.ToUpper()) (name.ToUpper())

//    let itypeMembers = 
//        itypes |> opOnly 
//        |> List.fold (fun state op ->
//            state @ [(sprintf "member val %s = Regex(@\"%s\")" (op.ToUpper()) op) + "\n"]
//            ) ([""])

    let itypeMembers = 
        itypes |> opOnly 
        |> List.fold (fun state op ->
            state @ [genActivePattern op]
            ) ([""])

    for itm in itypeMembers do printfn "%s" itm