open System
open System.IO
open System.Text.RegularExpressions

let rtypePattern = @"(?<rru>[01]{6})(?<rs1>[01]{5})(?<rs2>[01]{5})(?<rd>[01]{5})[01]{5,6}(?<func>[01]{5})"
let cvtf2dBinary = @"00000100101000000001000000001000"
let cvtf2dHex = "x04a01008"

let matches = Regex(rtypePattern).Matches(cvtf2dBinary)
printfn "%A" matches
for m in matches do
    printfn "RRU:\t%A" m.Groups.["rru"]
    printfn "RS1:\t%A" m.Groups.["rs1"]
    printfn "RS2:\t%A" m.Groups.["rs2"]
    printfn "RD:\t%A" m.Groups.["rd"]
    printfn "FUNC:\t%A" m.Groups.["func"]


printfn "%A" (Convert.ToUInt32(cvtf2dBinary, 2))

printfn "%A" (Convert.ToUInt32(cvtf2dBinary,2).ToString("x8"))


