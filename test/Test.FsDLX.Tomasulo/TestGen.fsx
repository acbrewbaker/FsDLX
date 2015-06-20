open System
open System.IO

let (@@) a b = Path.Combine(a,b)

let srcdir = __SOURCE_DIRECTORY__
let inputdir = srcdir @@ "..\..\Project2\Inputs"

let inputs = Directory.GetFiles(inputdir)


module IntUnit =
    let inputs = inputs |> Array.filter (fun f -> f.Contains("intUnit") && f.Contains(".hex"))
    let testfile = srcdir @@ "Interface.IntUnit.fs"
    let starttag = @"///<IntUnitTests>"
    let endtag = @"///</IntUnitTests>"

    let teststr (verbose:bool) path =
        let dotHexFile = Path.GetFileName(path)
        let dotOutFile = dotHexFile.Replace(".hex", ".out")
        sprintf "let [<Test>] ``%s`` () =\n" dotHexFile +
        sprintf "    let simulator = Simulator(inputdir @@ @%A,%A)\n" dotHexFile verbose +
        sprintf "    use sw = new StringWriter() in Console.SetOut(sw)\n" +
        sprintf "    simulator.Run()\n" +
        sprintf "    let expected, actual =\n" +
        sprintf "        File.ReadAllText(inputdir @@ @%A).Trim(),\n" dotOutFile +
        sprintf "        sw.ToString()\n" +
        sprintf "    StringAssert.Contains(expected, actual)\n"
        //sprintf "    Console.Clear()\n"

    let gen verbose _ = 
        let teststr = teststr verbose
        let testfileLines = File.ReadAllLines(testfile)
        let upToStartTag, afterEndTag = 
            match testfileLines |> Seq.tryFindIndex (fun line -> line.Contains(starttag)) with
            | Some st -> 
                match testfileLines |> Seq.tryFindIndex (fun line -> line.Contains(endtag)) with
                | Some et -> testfileLines.[0..st], testfileLines.[et..]
                | None -> Array.empty, Array.empty
            | None -> Array.empty, Array.empty

        
        [| upToStartTag; inputs |> Array.map teststr; afterEndTag |]
        |> Array.concat

let gen (outfile:string) (f:unit -> string[]) =
    File.WriteAllLines(outfile, f())
    

printfn "-----------------------------------------------"
(IntUnit.testfile, IntUnit.gen false)
||> gen