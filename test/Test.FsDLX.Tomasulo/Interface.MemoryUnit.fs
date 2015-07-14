module Test.FsDLX.Tomasulo.Interface.MemoryUnit

open System
open System.IO
open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

//<MemoryUnitTests>
[<NCrunch.Framework.Isolated>]
let [<Test>] ``memUnit1.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit1.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit1.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``memUnit2.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit2.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit2.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``memUnit3.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit3.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit3.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``memUnit4.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit4.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit4.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``memUnit5.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit5.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit5.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``memUnit6.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit6.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"memUnit6.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)


//</MemoryUnitTests>
