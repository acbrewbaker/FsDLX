module Test.FsDLX.Assembler.PcRelativeOffset

open System
open System.IO
open System.Text

open FsDLX.Assembler

open NUnit.Framework
open FsUnit


type DLX =
    {
        DLXInputs : string seq
    }

    member dlx.GetPairs() = dlx.DLXInputs |> Seq.map (fun s -> 
        let r = s.Split(':')
        r.[0].Trim(), r.[1].Trim())

    static member Default = 
        let dlx = 
            [
                "label1: j label3"
                "label2: jal label2"
                "label3: j label1"
                "label4: j label4"
                "label5: jal label6"
                "label6: jal label1"
            ]
        { DLXInputs = dlx }


type HEX =
    {
        HEXAnswer : string seq
    }

    member hex.GetPairs() = hex.HEXAnswer |> Seq.map (fun s ->
        let r = s.Split(':')
        r.[0].Trim(), r.[1].Trim())

    member hex.GetAddresses() = hex.GetPairs() |> Seq.map (fun (pc,instruction) ->
        let bin = instruction |> hex2bin
        let name = bin.Substring(6, 26).PadLeft(32, bin.[6])
        
        //printfn "binary: %A" bin
//        let s = bin.Substring(6, 26)
//        printfn "name only: %A, len ==> %A" s (s.Length)
//        printfn "s after : %A" (Convert.ToInt32(s.PadLeft(32, s.[0]) |> bin2hex, 16))
        Convert.ToInt32(name, 2))

    static member Default =
        let hex = 
            [
                "00000000: 08000004"
                "00000004: 0ffffffc"
                "00000008: 0bfffff4"
                "0000000c: 0bfffffc"
                "00000010: 0c000000"
                "00000014: 0fffffe8"
            ]
        { HEXAnswer = hex}




type Entry = string * int
type SymTab = Map<string, int>

type PCOffset =
    {
        Label : string
        mutable Offset : int
    }

    member pco.Update (st:SymTab) = st.ContainsKey(pco.Label) |> function
        | true -> ()
        | false -> pco.Offset <- pco.Offset + 4 

let opcodes = 
    [
        "j", 2
        "jal", 3
    ] |> Map.ofList

let op2bin (s:string) = Convert.ToString(opcodes.[s], 2).PadLeft(6, '0')
let ops2bin (st:SymTab) (s:string) = 
    let addr = st.[s]
//    printfn "ops2bin -> addr: %A" addr
    let s = Convert.ToString(st.[s], 2)
//    printfn "binAddr: %A" s
    //let s = Convert.ToString(st.[s], 2).PadLeft(26, '0')
//    printfn "s.length > 26? ==> %A" (s.Length > 26)
    let s = 
        (s.Length > 26) |> function
        | true -> s.Substring(s.Length - 26, 26)
//            let s = s.Substring(s.Length - 26, 26)
//            printfn "s.substring: %A" s
//            printfn "s.len: %A" s.Length
//            s
        | false -> s.PadLeft(26, '0')
    
    printfn "%A, ops length: %A" s (s.Length)
    s

let dlx2hex (pc:int) (op:string) (ops:string) = 
    //printfn "op.len, ops.len: %A, %A" (op.Length) (ops.Length)
    //printfn "both.len: %A" ((op + ops).Length)
    //printfn "32/4 == %A" ((op + ops).Length / 4)
    sprintf "%s: %s" 
        (pc.ToString("x8"))
        ((op + ops) |> bin2hex)


let displayDLXandHEX (dlx:string seq) (hex:string seq) =
    printfn "=== DLX ==="
    for d in dlx do printfn "%s" d

    printfn "\n=== HEX ==="
    for h in hex do printfn "%s" h

[<Test>]
let ``pc relative offset`` () = 
    
//    let pairs = dlx |> Seq.map (fun s -> 
//        let r = s.Split(':')
//        r.[0].Trim(), r.[1].Trim())
    let hex = HEX.Default.HEXAnswer
    let dlx = DLX.Default.DLXInputs
    let pairs = DLX.Default.GetPairs()

    let addr = HEX.Default.GetAddresses()
    printfn "=== Addresses ==="
    addr |> Seq.iteri (fun i a -> printfn "line %d:\t %A" (i + 1) a)


    let pc = ref 0
    let st = ref Map.empty<string, int>

    pairs |> Seq.iter (fun pair -> 
        let l, i = pair
        st := (!st).Add(l, !pc - 4)
        pc := !pc + 4)

//    printfn "result1"
//    for r in result1 do printfn "%A" r    
    printfn "SymTab:\n%A" (!st)

    let hex = 
        let pc = ref 0
        pairs |> Seq.map (fun pair ->
        let l, i = pair
        let op, ops = i.Split(' ').[0], i.Split(' ').[1].Trim()
//        printfn "op, ops ==> %A" (op, ops)
//        printfn "op.len, ops.len ==> %A, %A" (op.Length) (ops.Length)
        let op = op2bin op
//        printfn "binary op: %A, len = %A" op op.Length
        let ops = ops2bin !st ops
//        printfn "binary ops: %A, len = %A" ops ops.Length

        let r = dlx2hex !pc op ops
        pc := !pc + 4
        r)
    
    displayDLXandHEX dlx hex    


[<Test>]
let ``pc relative offset 2`` () = 
    
//    let pairs = dlx |> Seq.map (fun s -> 
//        let r = s.Split(':')
//        r.[0].Trim(), r.[1].Trim())
    let hex = HEX.Default.HEXAnswer
    let dlx = DLX.Default.DLXInputs
    let pairs = DLX.Default.GetPairs()

//    printfn "pairs: %A" pairs

    let st = ref Map.empty<string, int>
    let pc = ref 0

    let updateSymTab (s:string) (pc:int) = st := (!st).Add(s, pc)
    
    
    let labels = pairs |> Seq.map (fun p -> (snd p).Split(' ').[1])

//    printfn "Labels: %A" labels

    let addLabelsToSymTab() = pairs |> Seq.iter (fun p ->
        let label, _ = p
        updateSymTab label !pc
        pc := !pc + 4)

    addLabelsToSymTab()
//    !st |> Map.iter (fun k v -> printfn "%A -- %A" k v)


    let pc = ref 0
    let x() = pairs |> Seq.mapi (fun i p ->
        let label, inlineLabel = fst p, (snd p).Split(' ').[1]
  //      printfn "label, inlineLabel ==> %A, %A" label inlineLabel
        let lblPC = (!st).[inlineLabel]
        
        let jmp = if (!pc + 4) < lblPC then !pc + 4 else lblPC - (!pc + 4)
        pc := !pc + 4
        printfn "pc, lblPC, jmp ==> %A, %A, %A" !pc lblPC jmp
        
        (label, inlineLabel, jmp))

    let x = x()
    for y in x do printfn "%A" y
    