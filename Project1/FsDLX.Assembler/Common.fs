[<AutoOpen>]
module FsDLX.Assembler.Common

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
//open NCrunch.Framework

let inline (@@) (a:string) (b:string) = Path.Combine(a, b)
let inline (++) (a:string) (b:string) = a + " " + b
let inline (+|+) (a:string) (b:string) = a + "|" + b

let matches (r:Regex) s = r.IsMatch(s)

let asComment str = "    \t# " + str

let addLeadingZero (str:string) =
    str |> function
    | _ when str.StartsWith("-.") -> str.Insert(str.IndexOf("-.") + 1, "0")
    | _ when str.StartsWith(".") -> str.Insert(str.IndexOf("."), "0")
    | _ -> str

let floatingPointAsComment = addLeadingZero >> asComment

let strAsComment str = "\t#\"" + str + "\""

let bytes2hex (b:byte[]) =
    (b |> Array.rev |> BitConverter.ToString)
        .Replace("-","").ToLower()

let str2hex (str:string) =
    (Encoding.Default.GetBytes(str) |> BitConverter.ToString).Replace("-","").ToLower() + "00"

let revstr (s:string) = s.ToCharArray() |> Array.rev |> Array.fold(fun s c -> s + string c) ("")

let concatLines lines = lines |> List.fold (fun s l -> s + l + "\n") ("")

let b2hmap = 
    let hex = 
        ['0'..'9'] @ ['a'..'f']
        |> List.map string
    let bin = [for i in 0..15 -> Convert.ToString(i, 2).PadLeft(4, '0')]
    (bin, hex) ||> List.zip 
    |> Map.ofList

let h2bmap = 
    let hex = 
        ['0'..'9'] @ ['a'..'f']
    let bin = [for i in 0..15 -> Convert.ToString(i, 2).PadLeft(4, '0')]
    (hex, bin) ||> List.zip 
    |> Map.ofList

let nibble2hex (s:string) = 
    //printfn "trying to change %A from %A to hex" (s.Substring(0,4)) s
    b2hmap.[s.Substring(0,4)]

let byte2hex (s:string) = (b2hmap.[s.Substring(0, 4)] + b2hmap.[s.Substring(4, 4)])
    
let hex2bin (s:string) = s.ToCharArray() |> Array.fold (fun s c -> s + string h2bmap.[c]) ("")

let bin2hex (s:string) = 
    //printfn "bin2hex ===> %A" s
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

let parseTypeFile filepath =
    let pattern = @"(?<opcode>[^\s]+)\s+(?<rrid>\d\s+)*\s*(?<encoding>\d+)"
    let regex = new Regex(pattern, RegexOptions.Multiline)
    let matches = File.ReadAllText(filepath) |> regex.Matches
    [for m in matches -> 
        m.Groups.["opcode"].Value.Trim(), 
        m.Groups.["rrid"].Value.Trim(), 
        m.Groups.["encoding"].Value.Trim()]

let getInfo = parseTypeFile
    
let getOpEncOnly =
    getInfo
    >> List.map (fun (op, _, enc) -> (op, enc))

let getPattern =
    getInfo
    >> List.map (fun (op, _, _) -> op)
    >> List.fold (+|+) ("")

let getLookupByOp =
    getOpEncOnly
    >> Map.ofList

let getLookupByEnc =
    getOpEncOnly
    >> List.map (fun (o,e) -> (int e, o))
    >> Map.ofList


type ParsedOpcode = 
    { 
        Info : List<string*string*string>; 
        Pattern : string; 
        LookupByOpcode : Map<string, string>
        LookupByEncoding : Map<int, string>
    }

    static member create fp =
        {   Info            = fp |> parseTypeFile
            Pattern         = (fp |> getPattern).Substring(1) // remove extra or bar |
            LookupByOpcode  = fp |> getLookupByOp  
            LookupByEncoding= fp |> getLookupByEnc}

type OpcodeInfo(srcdir:string, itypesfile:string, rtypesfile:string, jtypesfile:string) = 
    let itypes = itypesfile |> ParsedOpcode.create
    let rtypes = rtypesfile |> ParsedOpcode.create
    let jtypes = jtypesfile |> ParsedOpcode.create
    
    let rrxOpEnc = 
        rtypes.Info
        |> List.map (fun (op, rrx, enc) -> (op, rrx))
        |> Map.ofList

    let rrxEncOp =
        rtypes.Info
        |> List.map (fun (op, rrx, enc) -> (int enc, rrx))
        |> Map.ofList

    let allOpEncPairs = 
        (itypesfile |> getOpEncOnly) @
        (rtypesfile |> getOpEncOnly) @
        (jtypesfile |> getOpEncOnly)
        
    let lookupByOp = allOpEncPairs |> Map.ofList
    
    let lookupByEnc = 
        allOpEncPairs
        |> List.map (fun (o,e) -> (int e,o))

    member val SrcDir = srcdir

    member val ITypes = itypes
    member val RTypes = rtypes
    member val JTypes = jtypes

//    member oi.GetIType(op:string) = itypes.LookupByOpcode.[op]
//    member oi.GetIType(enc:int) = itypes.LookupByEncoding.[enc]
//
//    member oi.GetRType(op:string) = rtypes.LookupByOpcode.[op]
//    member oi.GetRType(enc:int) = rtypes.LookupByEncoding.[enc]
//    
//    member oi.GetJType(op:string) = jtypes.LookupByOpcode.[op]
//    member oi.GetJType(enc:int) = jtypes.LookupByEncoding.[enc]

    member oi.GetRRX(op:string) = rrxOpEnc.[op]
//    member oi.GetRRX(enc:int) = rrxEncOp.[enc]

    member oi.Lookup(op:string) = lookupByOp.[op]
//    member oi.Lookup(enc:int) = lookupByEnc.[enc]


    new() =
        let srcdir = Environment.CurrentDirectory
//            if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
//                Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
//            else 
//                Environment.CurrentDirectory
            
        let itypesfile, rtypesfile, jtypesfile = 
            srcdir @@ @"../../../Itypes",
            srcdir @@ @"../../../Rtypes",
            srcdir @@ @"../../../Jtypes"
        
        new OpcodeInfo(srcdir, itypesfile, rtypesfile, jtypesfile)