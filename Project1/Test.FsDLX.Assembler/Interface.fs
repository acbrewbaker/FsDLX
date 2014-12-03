module Test.FsDLX.Assembler.Interface

open System
open System.IO
open System.Text

open FsDLX.Assembler

open NUnit.Framework
open FsUnit

let verbose = true

let displayFiles (dlx:string[]) (expected:string[]) (actual:string[]) =
    printfn "================ DLX ====================="
    dlx |> Array.iter (printfn "%A")
    printfn "size (expected, actual) = (%d, %d)" (expected.Length) (actual.Length)
    printfn "============ HEX (expected) ============== "
    expected |> Array.iter (printfn "%A")
    printfn "============ HEX (actual) ============== "
    actual |> Array.iter (printfn "%A")

let removeComments (lines:string[]) =
    lines |> Array.map (fun l -> l.Split('#').[0])

let compare (dlx:string[]) (expected:string[]) (actual:string[]) =
//    let expected, actual =
//        expected    |> removeComments,
//        actual      |> removeComments
    
    (dlx, expected, actual)
    |||> displayFiles

    (expected, actual)
    ||> Array.iter2 (fun e a -> Assert.AreEqual(e, a))

let assemble (dlxfile:string) = 
    use assembler = new Assembler(dlxfile, new OpcodeInfo(srcdir))
    assembler.Run(testdir, verbose)
    let resultfile = testdir @@ (Path.GetFileName(Path.ChangeExtension(dlxfile, ".hex")))
    printfn "Finised assembling %A ===> %A" dlxfile resultfile
    resultfile
    |> File.ReadAllLines

let teststr dlxfile hexfile = 
    let dlx = dlxfile |> Path.GetFileName
    let hex = hexfile |> Path.GetFileName
    sprintf "
[<Test>]
let ``%s to %s`` () =
    let dlxfile = inputdir @@ \"%s\"
    let hexfile = inputdir @@ \"%s\"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual" dlx hex dlxfile hexfile


[<Test>]
let ``gen all files tests`` () =
    let dlxfiles = Directory.GetFiles(inputdir, "*.dlx")
    for f in dlxfiles do 
        let testfile = f |> Path.GetFileNameWithoutExtension
        let dlxfile = testfile + ".dlx"
        let hexfile = testfile + ".hex"
        printfn "%s" (teststr dlxfile hexfile)

[<Test>]
let ``show paths in test context`` () =
    printfn "SrcDir: %A" srcdir
    printfn "InputDir: %A" inputdir
    printfn "TestDir: %A" testdir
  


    

///////////////////////////////////////////////////////////////////////////////////////////////

[<Test>]
let ``align.dlx to align.hex`` () =
    let dlxfile = inputdir @@ "align.dlx"
    let hexfile = inputdir @@ "align.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``arithImmed.dlx to arithImmed.hex`` () =
    let dlxfile = inputdir @@ "arithImmed.dlx"
    let hexfile = inputdir @@ "arithImmed.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``asciiz1.dlx to asciiz1.hex`` () =
    let dlxfile = inputdir @@ "asciiz1.dlx"
    let hexfile = inputdir @@ "asciiz1.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``asciiz2.dlx to asciiz2.hex`` () =
    let dlxfile = inputdir @@ "asciiz2.dlx"
    let hexfile = inputdir @@ "asciiz2.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``branches.dlx to branches.hex`` () =
    let dlxfile = inputdir @@ "branches.dlx"
    let hexfile = inputdir @@ "branches.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``convert.dlx to convert.hex`` () =
    let dlxfile = inputdir @@ "convert.dlx"
    let hexfile = inputdir @@ "convert.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``data.dlx to data.hex`` () =
    let dlxfile = inputdir @@ "data.dlx"
    let hexfile = inputdir @@ "data.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``directives1.dlx to directives1.hex`` () =
    let dlxfile = inputdir @@ "directives1.dlx"
    let hexfile = inputdir @@ "directives1.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``directives2.dlx to directives2.hex`` () =
    let dlxfile = inputdir @@ "directives2.dlx"
    let hexfile = inputdir @@ "directives2.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``double1.dlx to double1.hex`` () =
    let dlxfile = inputdir @@ "double1.dlx"
    let hexfile = inputdir @@ "double1.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``double2.dlx to double2.hex`` () =
    let dlxfile = inputdir @@ "double2.dlx"
    let hexfile = inputdir @@ "double2.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``float1.dlx to float1.hex`` () =
    let dlxfile = inputdir @@ "float1.dlx"
    let hexfile = inputdir @@ "float1.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``float2.dlx to float2.hex`` () =
    let dlxfile = inputdir @@ "float2.dlx"
    let hexfile = inputdir @@ "float2.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``fpArith.dlx to fpArith.hex`` () =
    let dlxfile = inputdir @@ "fpArith.dlx"
    let hexfile = inputdir @@ "fpArith.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``intArith.dlx to intArith.hex`` () =
    let dlxfile = inputdir @@ "intArith.dlx"
    let hexfile = inputdir @@ "intArith.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``intLogical.dlx to intLogical.hex`` () =
    let dlxfile = inputdir @@ "intLogical.dlx"
    let hexfile = inputdir @@ "intLogical.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``intSets.dlx to intSets.hex`` () =
    let dlxfile = inputdir @@ "intSets.dlx"
    let hexfile = inputdir @@ "intSets.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``intShift.dlx to intShift.hex`` () =
    let dlxfile = inputdir @@ "intShift.dlx"
    let hexfile = inputdir @@ "intShift.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``jump.dlx to jump.hex`` () =
    let dlxfile = inputdir @@ "jump.dlx"
    let hexfile = inputdir @@ "jump.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``jumpR.dlx to jumpR.hex`` () =
    let dlxfile = inputdir @@ "jumpR.dlx"
    let hexfile = inputdir @@ "jumpR.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``loadImmed.dlx to loadImmed.hex`` () =
    let dlxfile = inputdir @@ "loadImmed.dlx"
    let hexfile = inputdir @@ "loadImmed.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``loads.dlx to loads.hex`` () =
    let dlxfile = inputdir @@ "loads.dlx"
    let hexfile = inputdir @@ "loads.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``logicalImmed.dlx to logicalImmed.hex`` () =
    let dlxfile = inputdir @@ "logicalImmed.dlx"
    let hexfile = inputdir @@ "logicalImmed.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``move.dlx to move.hex`` () =
    let dlxfile = inputdir @@ "move.dlx"
    let hexfile = inputdir @@ "move.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``nop.dlx to nop.hex`` () =
    let dlxfile = inputdir @@ "nop.dlx"
    let hexfile = inputdir @@ "nop.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``setImmed.dlx to setImmed.hex`` () =
    let dlxfile = inputdir @@ "setImmed.dlx"
    let hexfile = inputdir @@ "setImmed.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``shiftImmed.dlx to shiftImmed.hex`` () =
    let dlxfile = inputdir @@ "shiftImmed.dlx"
    let hexfile = inputdir @@ "shiftImmed.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``space.dlx to space.hex`` () =
    let dlxfile = inputdir @@ "space.dlx"
    let hexfile = inputdir @@ "space.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``stores.dlx to stores.hex`` () =
    let dlxfile = inputdir @@ "stores.dlx"
    let hexfile = inputdir @@ "stores.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``text1.dlx to text1.hex`` () =
    let dlxfile = inputdir @@ "text1.dlx"
    let hexfile = inputdir @@ "text1.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``text2.dlx to text2.hex`` () =
    let dlxfile = inputdir @@ "text2.dlx"
    let hexfile = inputdir @@ "text2.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``trap.dlx to trap.hex`` () =
    let dlxfile = inputdir @@ "trap.dlx"
    let hexfile = inputdir @@ "trap.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``word1.dlx to word1.hex`` () =
    let dlxfile = inputdir @@ "word1.dlx"
    let hexfile = inputdir @@ "word1.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

[<Test>]
let ``word2.dlx to word2.hex`` () =
    let dlxfile = inputdir @@ "word2.dlx"
    let hexfile = inputdir @@ "word2.hex"
    let dlx = dlxfile |> File.ReadAllLines
    let expected = hexfile |> File.ReadAllLines
    let actual = dlxfile |> assemble
    compare dlx expected actual

