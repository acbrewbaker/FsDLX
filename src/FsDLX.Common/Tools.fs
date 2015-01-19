namespace FsDLX.Common

open System
open System.Linq
open System.IO
open System.Text
open System.Text.RegularExpressions


[<AutoOpen>]
module Tools =
    let inline (@@) (a:string) (b:string) = Path.Combine(a, b)
    let inline (++) (a:string) (b:string) = a + " " + b
    let inline (+|+) (a:string) (b:string) = a + "|" + b

    let revstr (s:string) = s.ToCharArray() |> Array.rev |> Array.fold(fun s c -> s + string c) ("")

    let concatLines lines = lines |> List.fold (fun s l -> s + l + "\n") ("")

    let splitForHex (s:string) = s.Split([|':'; '#'|]).[1].Trim()

    module Info =
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


module Convert =
    let bytes2hex (b:byte[]) =
        (b |> Array.rev |> BitConverter.ToString)
            .Replace("-","").ToLower()

    let str2hex (str:string) =
        (Encoding.Default.GetBytes(str) |> BitConverter.ToString).Replace("-","").ToLower() + "00"
            
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

    let nibble2hex (s:string) = b2hmap.[s.Substring(0,4)]

    let byte2hex (s:string) = (b2hmap.[s.Substring(0, 4)] + b2hmap.[s.Substring(4, 4)])
    
    let hex2bin (s:string) = s.ToCharArray() |> Array.fold (fun s c -> s + string h2bmap.[c]) ("")

    let bin2int (s:string) = Convert.ToInt32(s, 2)

    let int2bin (i:int) = Convert.ToString(i, 2).PadLeft(32, '0')

    let bin2hex (s:string) = 
        s |> function
        | _ when s.Length % 4 <> 0 -> failwith "binary string length needs to be multiple of 4"
        | _ ->
            let nibbles = s.Length / 4
            [0..nibbles-1] 
            |> List.map (fun nib -> s.Substring(nib * 4, 4)) 
            |> List.map nibble2hex
            |> List.reduce (+)
        | _ -> failwith (sprintf "failed binary to hex conversion of: %A, of length %d\nbinary string must be length 32!" s s.Length)


    let hex2bytes (hex:string) = hex.Length |> function
        | 8 -> 
            Enumerable.Range(0, hex.Length)
                      .Where(fun x -> x % 2 = 0)
                      .Select(fun x -> Convert.ToByte(hex.Substring(x,2), 16))
                      .ToArray()
        | _ -> failwith "Instruction length greater than 8 bytes"

    let hex2int hex = Convert.ToInt32(hex, 16)

    let hex2bits hex a b = (hex2bin hex).[a..b]

    let hex2bits2int hex a b = bin2int(hex2bits hex a b)

    let int2hex (i:int) = i.ToString("x8")

    let int2nibble (i:int) = (int2hex i).[4..7]

    let int2bits i a b = (int2bin i).[a..b]

    let int2bits2int i a b = bin2int(int2bits i a b)

    let int2bits2reg i startBit = int2bits2int i startBit (startBit + 4)

    let strOption2str (o:string option) = o |> function
        | Some o -> sprintf "%s" o
        | None -> sprintf "%O" o

    let intOption2str (o:int option) = o |> function
        | Some o -> int2hex o
        | None -> sprintf "%s" (int2hex 0)



