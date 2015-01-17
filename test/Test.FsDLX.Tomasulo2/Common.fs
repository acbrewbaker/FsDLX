module Test.FsDLX.Tomasulo2.Common

open System
open System.IO
open NUnit.Framework

open FsDLX.Common

let inline (@@) (a:string) (b:string) = Path.Combine(a,b)

let srcdir = 
    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then 
        Directory.GetParent(NCrunch.Framework.NCrunchEnvironment.GetOriginalProjectPath()).FullName
    else 
        Environment.CurrentDirectory

let inputdir = 
    if NCrunch.Framework.NCrunchEnvironment.NCrunchIsResident() then
        srcdir @@ "../../Project2/Inputs"
    else
        srcdir @@ "../../../../Project2/Inputs"


//
//[<Test>]
//let ``test`` () =
//    let intUnit : FunctionalUnit =
//        let x : ReservationStation = 
//            false, None, 0,0, None, None, 0, false, false, false
//        let rs : RS = [x]
//        let f : FUnit = 0, 0, false, (ref rs)
//        let funit : FUnit[] = [| f |]
//        FunctionalUnit.IU funit
//    
//    let funits = [| intUnit |]
//    let cdb : CDB = None
//    let i = Instruction.Int 0
//    let istate = InstructionState.Write(None, i)
//    let initState : SimulatorState = 0, 0, cdb, funits, istate
//    let log : Log = [initState]
//    let memory : Memory = 
//        let m = Array.zeroCreate<int> 1000
//        (inputdir @@ "add.hex")
//        |> File.ReadAllLines
//        |> Array.map (splitForHex >> Convert.hex2int)
//        |> Array.iteri (fun i e -> m.[i] <- e )
//        m
//
//    let halt : Halt = false
//    let stall : Stall = false
//
//    let finished() = false
//
//    let simulate() =
//        memory 
//        |> Seq.fold (fun (state:SimulatorState) (instruction:int) ->
//               let _, _, _, _, istate = state
//               ((not(halt) &&  not(finished())), istate) |> function
//               | false, Write write -> state
//                    
//               | false, Execute execute -> state
//               | false, Issue issue -> 
//                    funits |> Array.tryFindIndex (fun u -> busy) |> function
//                    | Some u -> state
//                    | None -> state
//               | true, _ -> state
//            ) (initState)
//    for l in log do printfn "%A" l