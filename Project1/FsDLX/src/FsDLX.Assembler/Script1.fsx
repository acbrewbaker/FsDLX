open System
open System.Text
open System.Text.RegularExpressions


let (|Let|) value input = (value, input)

let flag, num = true, 3

(flag, num) |> function
| true, (Let "one" (str, 1) | Let "two" (str, 2) | Let "three" (str, 3)) -> printfn "%s" str
| _ -> printfn "Somthing Else"


let matchToken pattern s =
    Regex.Match(s, pattern |> sprintf "\A(%s)((?s).*)",
        RegexOptions.Multiline)
    |> fun mtch ->
        if mtch.Success then
            (mtch.Groups.[1].Value, mtch.Groups.[2].Value) |> Some
        else
            None

let (|WS|_|) = matchToken "[ |\t|\n|\n\r]+" 
let (|COMMENT|_|) = matchToken "#.*[\n|\r\n]"
let (|WHITESPACE|_|) s = match s with | WS rest -> rest |> Some | COMMENT rest -> rest |> Some | _ -> None 
let rec (|Star|_|) f acc s = match f s with | Some (res, rest) -> (|Star|_|) f (res :: acc) rest | None -> (acc |> List.rev , s) |> Some
let (|WhiteSpace|_|) s = (|Star|_|) (|WHITESPACE|_|) [] s
let rec MatchTokenNoWS s pattern = match (|WhiteSpace|_|) s with | Some (_, rest) -> rest |> matchToken pattern | None -> s |> matchToken pattern
let MatchToken s f pattern = pattern |> MatchTokenNoWS s |> Option.bind f
let MatchSymbol s pattern = pattern |> MatchToken s (fun (_, rest) -> rest |> Some)
let (|ITYPE|_|) s = 
    "[beqz|bnez|addi|addui|subi|subui|andi|ori|xori|lhi|trap|jr|jalr|slli|srli|srai|seqi|snei|slti|sgti|slei|sgei|lb|lh|lw|lbu|lhu|lf|ld|sb|sh|sw|sf|sd]" 
    |> MatchToken s (fun res -> res |> Some)
let (|REG|_|) s = "r\d\d?" |> MatchToken s (fun res -> res |> Some)
let str = @"addi r1, r2, label4"

str |> function
| ITYPE s -> s
| _ -> "derp", "derp"
|> printfn "%A"

let rec (|DERP|_|) = function
    | ITYPE (n, rest) -> (n, rest) |> Some
    | REG (n, rest) -> (n, rest) |> Some
    | _ -> None

let _ = 
    str |> function
    | DERP (e, r) -> e |> printfn "%A"
    | _ -> printfn "shit"

