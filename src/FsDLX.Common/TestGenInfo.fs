[<AutoOpen>]
module TestGenInfo

open System
open System.IO
open System.Text.RegularExpressions
open FsDLX.Common

let oneLine = "\r\n"
let twoLines = "\r\n\r\n"

let findSolutionDir (startDir:string) =
    let filesToCheck = [ "FsDLX.sln" ]

    let isSolutionDir (dir:string) =
        filesToCheck |> List.forall (fun file -> File.Exists(dir @@ file))

    let rec find (dir:string) =
        if isSolutionDir dir then dir
        else find (Directory.GetParent(dir).FullName)

    find startDir

let replace file tag (newText:string) =
    let replace =
        let newText = oneLine + newText + oneLine
        let regex = Regex(sprintf """(?<=((//|')<%s>)).*?(?=((//|')</%s>))""" tag tag, RegexOptions.Singleline ||| RegexOptions.Compiled)
        fun source -> regex.Replace(source, newText)

    let contents = File.ReadAllText file
    let newContents = replace contents
    if contents <> newContents then
        File.WriteAllText(file, newContents)

let getHexFiles inputDir inputSet =
    Directory.GetFiles(inputDir)
    |> Array.filter (fun f -> f.Contains(inputSet) && f.Contains(".hex"))

module TomasuloTests =
    let generate inputSet testFile tag testProjectDir inputDir (verbose:bool) =
        use stream = new MemoryStream()
        use file = new StreamWriter(stream)
        let gen path =            
            let dotHexFile = Path.GetFileName(path)
            let dotOutFile = dotHexFile.Replace(".hex", ".out")
            fprintfn file "[<NCrunch.Framework.Isolated>]"
            fprintfn file "let [<Test>] ``%s`` () =" dotHexFile
            fprintfn file "    let simulator = Simulator(@%A @@ @%A,%A)" inputDir dotHexFile verbose
            fprintfn file "    use sw = new StringWriter() in Console.SetOut(sw)"
            fprintfn file "    simulator.Run()"
            fprintfn file "    let expected, actual ="
            fprintfn file "        File.ReadAllText(@%A @@ @%A).Replace(\"\\r\",\"\")," inputDir dotOutFile
            fprintfn file "        sw.ToString()"
            fprintfn file "    StringAssert.Contains(expected, actual)"
            fprintfn file ""
        getHexFiles inputDir inputSet |> Array.iter gen
        file.Flush()
        let code = stream.ToArray() |> System.Text.Encoding.UTF8.GetString
        replace (testProjectDir @@ testFile) tag code

    
    module IntUnit =
        let generate = generate "intUnit" "Interface.IntUnit.fs" "IntUnitTests"



