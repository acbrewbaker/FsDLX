let out s = printfn "%s" s 

let capChars =
    (['a'..'z'], ['A'..'Z'])
    ||> List.zip
    |> Map.ofList

let tocap (s:string) = 
    string capChars.[s.[0]] + 
    s.Substring(1)

let directiveToCap (s:string) = s.Replace(".", "") |> tocap


let directives = 
    [
        ".text"
        ".data"
        ".align"
        ".asciiz"
        ".double"
        ".float"
        ".word"
        ".space"
    ]


let types =
    [
        "R"
        "P"
        "Comment"
        "Directive"
        "Instruction"
        "Opcode"
        "Operand"
        "Immediate"
        "Label"
        "BasePlusOffset"
        "Register"
    ]

let patterns = types |> List.map (fun s -> s + "Patterns")
let regexes = types |> List.map (fun s -> s + "Regexs")

//let regexes =
//    [
//        ["CommentOnly"]
//        (directives |> List.map directiveToCap)
//        ["IType"; "RType"; "JType"; "Any"]
//        ["IType"; "RType"; "JType"; "Any"]
//        ["IType"; "RType"; "JType"; "Any"]
//        ["Value"; "Label"; "Register"; "BasePlusOffset"; "Any"]
//        ["New"; "Inline"]
//        ["ValPlusReg"; "ValPlusLabel"; "RegPlusVal"; "RegPlusLabel"; "Any"]
//        ["R"; "F"]
//    ]

//let prmap =
//    (pats2regexes, regexes) ||> List.zip |> Map.ofList
//
//let regexTest p r s =
//    sprintf "
//let [<Test>] ``%s - %s`` () =
//    let s = %s
//    let r = R.%s.%s
//    r.IsMatch(s) |> should equal true"
//        p r
//        s
//        p r
//
//(patterns, regexes)
//||> List.iter2 (fun p rs -> 
//    rs |> List.iter (fun r -> regexTest p r "\"\"" |> out))

//let tile s =
//    sprintf
//"[<Test>]
//let ``%s`` () ="
//
//let patternTestGen() =
//    let str =
//        sprintf
//"[<Test>]
//let ``%s`` () =
//    let str = %s
//    let regex"