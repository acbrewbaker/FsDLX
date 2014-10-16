[<AutoOpen>]
module FsDLX.Assembler.Common

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open NCrunch.Framework

let inline (@@) (a:string) (b:string) = Path.Combine(a, b)
let inline (++) (a:string) (b:string) = a + " " + b
let inline (+|+) (a:string) (b:string) = a + "|" + b

let matches (r:Regex) s = r.IsMatch(s)

let asComment str = "\t#" + str

let addLeadingZero (str:string) =
    str |> function
    | _ when str.StartsWith("-.") -> str.Insert(str.IndexOf("-.") + 1, "0")
    | _ when str.StartsWith(".") -> str.Insert(str.IndexOf("."), "0")
    | _ -> str

let floatingPointAsComment = addLeadingZero >> asComment

let bytes2hex (b:byte[]) =
    (b |> Array.rev |> BitConverter.ToString)
        .Replace("-","").ToLower()

let str2hex (str:string) =
    (Encoding.Default.GetBytes(str) |> BitConverter.ToString).Replace("-","").ToLower() + "00"

let concatLines lines = lines |> List.fold (fun s l -> s + l + "\n") ("")

let b2hmap = 
    let hex = 
        ['0'..'9'] @ ['a'..'f']
        |> List.map string
    let bin = [for i in 0..15 -> Convert.ToString(i, 2).PadLeft(4, '0')]
    (bin, hex) ||> List.zip 
    |> Map.ofList

module Support =
    let srcdir = 
        if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
            Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
        else 
            Environment.CurrentDirectory

    let inputdir = srcdir @@ @"../../../Inputs"

    let itypesfile, rtypesfile, jtypesfile = 
        srcdir @@ @"../../../Itypes",
        srcdir @@ @"../../../Rtypes",
        srcdir @@ @"../../../Jtypes"

    type Defaults = 
        { Directories : Directories; Files : Files}
        static member Init =
            { Directories = Directories.Default; Files = Files.Default }

    and Directories =
        { Source : string; Input : string }
        static member Default =
            { Source = srcdir; Input = inputdir}

    and Files =
        { IType : string; RType : string; JType : string }
        static member Default =
            { IType = itypesfile; RType = rtypesfile; JType = jtypesfile }



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
        {   Info            = fp |> Support.parseTypeFile
            Pattern         = (fp |> Support.getPattern).Substring(1) // remove extra or bar |
            LookupByOpcode  = fp |> Support.getLookupByOp  
            LookupByEncoding= fp |> Support.getLookupByEnc}

and OpcodeInfo(itypesfile:string, rtypesfile:string, jtypesfile:string) = 
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
        (itypesfile |> Support.getOpEncOnly) @
        (rtypesfile |> Support.getOpEncOnly) @
        (jtypesfile |> Support.getOpEncOnly)
        
    let lookupByOp = allOpEncPairs |> Map.ofList
    
    let lookupByEnc = 
        allOpEncPairs
        |> List.map (fun (o,e) -> (int e,o))

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
       