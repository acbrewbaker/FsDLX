module Test.FsDLX.Tomasulo.Registers

open NUnit.Framework
open FsUnit
open FsDLX.Tomasulo


[<Test>]
let ``gpr set``() =
    let gpr = GPR.GetInstance
    let qi, x = Some "IntUnit0", 100
    let idx = 2
    printfn "Before Set:\n%O" gpr
    
    printfn "gpr(idx) before: %O" (gpr.[idx])
    printfn "gpr(idx).Qi: %O" (gpr.[idx].Qi)
    printfn "gpr(idx).Contents: %O" (gpr.[idx].Contents)
    gpr.[idx].Qi <- qi
    gpr.[idx].Contents <- x
    

    
    printfn "\nAfter Set:\n%O" gpr
    printfn "gpr(idx).Qi: %O" (gpr.[idx].Qi)
    printfn "gpr(idx).Contents: %O" (gpr.[idx].Contents)


    printfn "gpr.R0toR7():  %O" (gpr.R0toR7())


[<Test>]
let ``gpr update`` () =
    let cdb = CDB.GetInstance
    let gpr = GPR.GetInstance
    cdb.Result <- 3
    cdb.Src <- "IntUnit0"
    printfn "GPR before update:\n%O" gpr
    gpr.Update()
    printfn "GPR after update:\n%O" gpr