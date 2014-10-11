open FsDLX

[<EntryPoint>]
let main argv = 
    let assembler = Assembler.T()
    assembler.Message()
    printfn "%A" argv
    0 // return an integer exit code

