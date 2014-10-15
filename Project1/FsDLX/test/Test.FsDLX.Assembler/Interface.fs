module Test.FsDLX.Assembler.Interface

open System
open System.IO

open FsDLX.Assembler

open NUnit.Framework
open FsUnit

let verbose = true

let displayFiles (expected:string list) (actual:string list) =
    printfn "size (expected, actual) = (%d, %d)" (expected.Length) (actual.Length)
    printfn "============ HEX (expected) ============== "
    expected |> List.iter (printfn "%A")
    printfn "============ HEX (actual) ============== "
    actual |> List.iter (printfn "%A")

let compare (filename:string) =
    let expected =
        inputdir @@ (filename + ".hex")
        |> File.ReadAllLines |> List.ofArray
    
    let actual = 
        testdir @@ (filename + ".hex")
        |> File.ReadAllLines |> List.ofArray

    (expected, actual)
    ||> displayFiles

    (expected, actual)
    ||> List.iter2 (fun e a -> Assert.AreEqual(e, a))
    let result =
        (expected, actual)
        ||> Seq.zip
        |> Seq.choose (fun (e,a) -> e = a |> function 
            | true -> None 
            | _ -> Some (e,a))
    result |> Seq.isEmpty |> function
    | true -> None
    | _ -> result |> Seq.head |> Some


let teststr dlxfile hexfile = 
    let dlx = dlxfile |> Path.GetFileName
    let hex = hexfile |> Path.GetFileName
    sprintf "
[<Test>]
let ``%s to %s`` () =
    let dlxfile = Path.Combine(inputdir, \"%s\")
    let hexfile = Path.Combine(inputdir, \"%s\")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile).Replace(\"\\r\",\"\")
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected" dlx hex dlxfile hexfile


[<Test>]
let ``get file names`` () =
    let dlxfiles = Directory.GetFiles(inputdir, "*.dlx")
    for f in dlxfiles do 
        let testfile = f |> Path.GetFileNameWithoutExtension
        let dlxfile = testfile + ".dlx"
        let hexfile = testfile + ".hex"
        printfn "%s" (teststr dlxfile hexfile)

let printContent dlx expected actual =
    printfn "DLX:\n%s" dlx
    printfn "HEX - Expected:\n%s" expected
    printfn "HEX - Actual:\n%s" actual
   

[<Test>]
let ``simple assembler test`` () =
    let dlx = inputdir @@ "asciiz1.dlx"
    use assembler = new Assembler(dlx)
    assembler.Run(testdir, verbose)
    compare "asciiz1.dlx"

[<Test>]
let ``arithImmed.dlx`` () =
    let dlx = inputdir @@ "arithImmed.dlx"
    use assembler = new Assembler(dlx)
    assembler.Run(testdir, verbose)
    compare "arithImmed"

[<Test>]
let ``directives1.dlx`` () =
    let dlx = inputdir @@ "directives1.dlx"
    use assembler = new Assembler(dlx)
    assembler.Run(testdir, verbose)
    compare "directives1"

[<Test>]
let ``all files`` () =
    let dlx = Directory.GetFiles(inputdir, "*.dlx")
    //printfn "Input files: %A" dlx 
    let assemble f =
        use assembler = new Assembler(f)
        assembler.Run(testdir, verbose)
    dlx |> Seq.iter assemble

let assemble (file:string) = 
    use assembler = new Assembler(file)
    assembler.Run(testdir, verbose)
    let hex = testdir @@ Path.ChangeExtension(file, ".hex")
    (0,0, hex |> File.ReadAllText)

[<Test>]
let ``align.dlx to align.hex`` () =
    let dlxfile = Path.Combine(inputdir, "align.dlx")
    let hexfile = Path.Combine(inputdir, "align.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``arithImmed.dlx to arithImmed.hex`` () =
    let dlxfile = Path.Combine(inputdir, "arithImmed.dlx")
    let hexfile = Path.Combine(inputdir, "arithImmed.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``asciiz1.dlx to asciiz1.hex`` () =
    let dlxfile = Path.Combine(inputdir, "asciiz1.dlx")
    let hexfile = Path.Combine(inputdir, "asciiz1.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``asciiz2.dlx to asciiz2.hex`` () =
    let dlxfile = Path.Combine(inputdir, "asciiz2.dlx")
    let hexfile = Path.Combine(inputdir, "asciiz2.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile) //
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``branches.dlx to branches.hex`` () =
    let dlxfile = Path.Combine(inputdir, "branches.dlx")
    let hexfile = Path.Combine(inputdir, "branches.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``convert.dlx to convert.hex`` () =
    let dlxfile = Path.Combine(inputdir, "convert.dlx")
    let hexfile = Path.Combine(inputdir, "convert.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``data.dlx to data.hex`` () =
    let dlxfile = Path.Combine(inputdir, "data.dlx")
    let hexfile = Path.Combine(inputdir, "data.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``directives1.dlx to directives1.hex`` () =
    let dlxfile = Path.Combine(inputdir, "directives1.dlx")
    let hexfile = Path.Combine(inputdir, "directives1.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``directives2.dlx to directives2.hex`` () =
    let dlxfile = Path.Combine(inputdir, "directives2.dlx")
    let hexfile = Path.Combine(inputdir, "directives2.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``double1.dlx to double1.hex`` () =
    let dlxfile = Path.Combine(inputdir, "double1.dlx")
    let hexfile = Path.Combine(inputdir, "double1.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``double2.dlx to double2.hex`` () =
    let dlxfile = Path.Combine(inputdir, "double2.dlx")
    let hexfile = Path.Combine(inputdir, "double2.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``float1.dlx to float1.hex`` () =
    let dlxfile = Path.Combine(inputdir, "float1.dlx")
    let hexfile = Path.Combine(inputdir, "float1.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``float2.dlx to float2.hex`` () =
    let dlxfile = Path.Combine(inputdir, "float2.dlx")
    let hexfile = Path.Combine(inputdir, "float2.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``fpArith.dlx to fpArith.hex`` () =
    let dlxfile = Path.Combine(inputdir, "fpArith.dlx")
    let hexfile = Path.Combine(inputdir, "fpArith.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``intArith.dlx to intArith.hex`` () =
    let dlxfile = Path.Combine(inputdir, "intArith.dlx")
    let hexfile = Path.Combine(inputdir, "intArith.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``intLogical.dlx to intLogical.hex`` () =
    let dlxfile = Path.Combine(inputdir, "intLogical.dlx")
    let hexfile = Path.Combine(inputdir, "intLogical.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``intSets.dlx to intSets.hex`` () =
    let dlxfile = Path.Combine(inputdir, "intSets.dlx")
    let hexfile = Path.Combine(inputdir, "intSets.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``intShift.dlx to intShift.hex`` () =
    let dlxfile = Path.Combine(inputdir, "intShift.dlx")
    let hexfile = Path.Combine(inputdir, "intShift.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``jump.dlx to jump.hex`` () =
    let dlxfile = Path.Combine(inputdir, "jump.dlx")
    let hexfile = Path.Combine(inputdir, "jump.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``jumpR.dlx to jumpR.hex`` () =
    let dlxfile = Path.Combine(inputdir, "jumpR.dlx")
    let hexfile = Path.Combine(inputdir, "jumpR.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``loadImmed.dlx to loadImmed.hex`` () =
    let dlxfile = Path.Combine(inputdir, "loadImmed.dlx")
    let hexfile = Path.Combine(inputdir, "loadImmed.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``loads.dlx to loads.hex`` () =
    let dlxfile = Path.Combine(inputdir, "loads.dlx")
    let hexfile = Path.Combine(inputdir, "loads.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``logicalImmed.dlx to logicalImmed.hex`` () =
    let dlxfile = Path.Combine(inputdir, "logicalImmed.dlx")
    let hexfile = Path.Combine(inputdir, "logicalImmed.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``move.dlx to move.hex`` () =
    let dlxfile = Path.Combine(inputdir, "move.dlx")
    let hexfile = Path.Combine(inputdir, "move.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``nop.dlx to nop.hex`` () =
    let dlxfile = Path.Combine(inputdir, "nop.dlx")
    let hexfile = Path.Combine(inputdir, "nop.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``setImmed.dlx to setImmed.hex`` () =
    let dlxfile = Path.Combine(inputdir, "setImmed.dlx")
    let hexfile = Path.Combine(inputdir, "setImmed.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``shiftImmed.dlx to shiftImmed.hex`` () =
    let dlxfile = Path.Combine(inputdir, "shiftImmed.dlx")
    let hexfile = Path.Combine(inputdir, "shiftImmed.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``space.dlx to space.hex`` () =
    let dlxfile = Path.Combine(inputdir, "space.dlx")
    let hexfile = Path.Combine(inputdir, "space.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``stores.dlx to stores.hex`` () =
    let dlxfile = Path.Combine(inputdir, "stores.dlx")
    let hexfile = Path.Combine(inputdir, "stores.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``text1.dlx to text1.hex`` () =
    let dlxfile = Path.Combine(inputdir, "text1.dlx")
    let hexfile = Path.Combine(inputdir, "text1.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``text2.dlx to text2.hex`` () =
    let dlxfile = Path.Combine(inputdir, "text2.dlx")
    let hexfile = Path.Combine(inputdir, "text2.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``trap.dlx to trap.hex`` () =
    let dlxfile = Path.Combine(inputdir, "trap.dlx")
    let hexfile = Path.Combine(inputdir, "trap.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``word1.dlx to word1.hex`` () =
    let dlxfile = Path.Combine(inputdir, "word1.dlx")
    let hexfile = Path.Combine(inputdir, "word1.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected

[<Test>]
let ``word2.dlx to word2.hex`` () =
    let dlxfile = Path.Combine(inputdir, "word2.dlx")
    let hexfile = Path.Combine(inputdir, "word2.hex")
    let dlx = File.ReadAllText(dlxfile)
    let expected = File.ReadAllText(hexfile)
    let symbols, pc, hex = dlxfile |> assemble
    printContent dlx expected hex
    hex |> should equal expected
