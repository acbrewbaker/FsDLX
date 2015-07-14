namespace FsDLX.Common

open System
open System.IO

//
//type AddressDataComment(inputLine:string) =
//    let split = 
//
//type DLXHexFile(filePath:string) =
//    let lines = File.ReadAllLines filePath
//    let a,d,c =
//        lines |> Array.map splitLine |> Array.map ()

type ParsedOpcode = 
    { 
        Info : List<string*string*string>; 
        Pattern : string; 
        LookupByOpcode : Map<string, string>
        LookupByEncoding : Map<int, string>
    }

    static member create fp =
        {   Info            = fp |> Info.parseTypeFile
            Pattern         = (fp |> Info.getPattern).Substring(1) // remove extra or bar |
            LookupByOpcode  = fp |> Info.getLookupByOp  
            LookupByEncoding= fp |> Info.getLookupByEnc}

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
        (itypesfile |> Info.getOpEncOnly) @
        (rtypesfile |> Info.getOpEncOnly) @
        (jtypesfile |> Info.getOpEncOnly)
        
    let lookupByOp = allOpEncPairs |> Map.ofList
    
    let lookupByEnc = 
        allOpEncPairs
        |> List.map (fun (o,e) -> (int e,o))

    member val SrcDir = srcdir

    member val ITypes = itypes
    member val RTypes = rtypes
    member val JTypes = jtypes

    member oi.GetRRX(op:string) = rrxOpEnc.[op]

    member oi.Lookup(op:string) = lookupByOp.[op]

    new(srcdir) =
//        let srcdir = //Environment.CurrentDirectory
//            if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
//                Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
//            else 
//                Environment.CurrentDirectory
            
        let itypesfile, rtypesfile, jtypesfile = 
            srcdir @@ @"../Itypes",
            srcdir @@ @"../Rtypes",
            srcdir @@ @"../Jtypes"
        
        new OpcodeInfo(srcdir, itypesfile, rtypesfile, jtypesfile)

