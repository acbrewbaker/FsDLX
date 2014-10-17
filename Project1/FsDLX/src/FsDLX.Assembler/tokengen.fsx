﻿//open System //open System.IO //open System.Text.RegularExpressions // //let inline (@@) (a:string) (b:string) = Path.Combine(a,b) // //let srcdir = __SOURCE_DIRECTORY__ //let inputdir = srcdir @@ "Inputs" //let supportdir = srcdir @@ "../" //let itypesfile, rtypesfile, jtypesfile = //    srcdir @@ "../../../Itypes", //    srcdir @@ "../../../Rtypes", //    srcdir @@ "../../../Jtypes" // //let capfirst : string -> string = //    let rmp (s:string) = s.Replace(".", "") //    let charmap =  //        (['a'..'z'], ['A'..'Z']) //        ||> List.zip //        |> Map.ofList //    let cap (s:string) = string charmap.[s.[0]] + s.Substring(1) //    function  //    | s when s.StartsWith(".") -> s |> rmp |> cap //    | s -> s |> cap //     //let directives = [ ".text"; ".data"; ".align"; ".asciiz"; ".double"; ".float"; ".word"; ".space"] // //type TokenInfo = (string * string list) list // //let tokenInfo : TokenInfo =  //    [ //        "Comment", ["CommentOnly"] //        "Directive", (directives |> List.map capfirst) //        "Register", ["R"; "F"] //        "BasePlusOffset", ["ValPlusReg"; "ValPlusLabel"; "RegPlusVal"; "RegPlusLabel"] //        "Label", ["New"; "Inline"] //        "Immediate", ["Value"; "Label"; "Register"; "BasePlusOffset"] //        "Operand", ["RS1"; "RS2"; "RD"; "Immediate"] //        "Opcode", ["IType"; "RType"; "JType"] //    ] // //let tokenCategories     = tokenInfo |> List.map fst //let tokenSubCategories  = tokenInfo |> List.map snd //let infoMap             = tokenInfo |> Map.ofList // //let getTokenType (name:string,tokens:string list) =  //    sprintf "type %s = %s"  //        name //        (tokens |> List.map (fun s -> s.ToUpper() |> sprintf "\n    | %s") |> List.reduce (+)) // //let parseTypeFile filepath = //    let pattern = @"(?<opcode>[^\s]+)\s+(?<rrid>\d\s+)*\s*(?<encoding>\d+)" //    let regex = new Regex(pattern, RegexOptions.Multiline) //    let matches = File.ReadAllText(filepath) |> regex.Matches //    [for m in matches ->  //        m.Groups.["opcode"].Value,  //        m.Groups.["rrid"].Value,  //        m.Groups.["encoding"].Value] // // //let itypes, rtypes, jtypes =  //    parseTypeFile itypesfile, //    parseTypeFile rtypesfile, //    parseTypeFile jtypesfile // //let opOnly (xtypes:(string*string*string) list) = xtypes |> List.map (fun (op, _, _) -> op) // //let addTab (s:string) = //    s.Split('\n')  //    |> Array.map (fun s -> "    " + s + "\n") //    |> List.ofArray //    |> List.reduce (+) // //let getOpcodeTokenType : string -> string = //    let opOnly = parseTypeFile >> opOnly //    let getTokenType fp name = (name, opOnly fp) |> getTokenType //    function //    | name when name = "IType" -> getTokenType itypesfile name //    | name when name = "RType" -> getTokenType rtypesfile name //    | name when name = "JType" -> getTokenType jtypesfile name //    | _ -> failwith "" // // // //let genTokenModules (info:TokenInfo) = //    let moduleName = sprintf "\nmodule %s = %s" //     //    let noModule cat subcat =  //        ("T", subcat) //        |> getTokenType //        |> addTab //        |> sprintf "\nmodule %s =\n%s" cat  //         //    info |> List.map (function //        | cat, subcat when cat = "Comment" -> noModule cat subcat //        | cat, subcat when cat = "Directive" -> noModule cat subcat //        | cat, subcat when cat = "Opcode" ->  //            moduleName cat  //                ( subcat |> List.map getOpcodeTokenType //                |> List.map ((+) "\n" >> addTab) //                |> List.reduce (+)) //         //        | cat, subcat when cat = "Operand" -> noModule cat subcat //        | cat, subcat when cat = "Immediate" -> noModule cat subcat //        | cat, subcat when cat = "Label" -> noModule cat subcat     //        | cat, subcat when cat = "BasePlusOffset" -> noModule cat subcat         //        | cat, subcat when cat = "Register" -> noModule cat subcat   //        | _ -> failwith "" ) // // //let tokengen (info:TokenInfo) = //    let out = srcdir @@ "Tokens.fs" //    if File.Exists(out) then File.Delete(out) //    let s =  //        "module FsDLX.Assembler.Tokens\n" +  //        (info |> genTokenModules |> List.reduce (+)) //    File.AppendAllText(srcdir @@ "Tokens.fs", s) // // //tokengen tokenInfo