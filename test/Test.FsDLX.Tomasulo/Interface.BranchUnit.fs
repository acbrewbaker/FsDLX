module Test.FsDLX.Tomasulo.Interface.BranchUnit

open System
open System.IO
open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

//<BranchUnitTests>
[<NCrunch.Framework.Isolated>]
let [<Test>] ``brUnit1.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit1.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit1.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``brUnit2.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit2.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit2.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``brUnit3.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit3.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit3.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``brUnit4.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit4.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit4.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``brUnit5.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit5.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit5.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``brUnit6.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit6.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"brUnit6.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)


//</BranchUnitTests>