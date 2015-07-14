module Test.FsDLX.Tomasulo.Interface.FloatingPointUnit

open System
open System.IO
open NUnit.Framework
open FsDLX.Common
open FsDLX.Tomasulo

//<FloatingPointUnitTests>
[<NCrunch.Framework.Isolated>]
let [<Test>] ``fpUnit1.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit1.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit1.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``fpUnit2.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit2.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit2.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``fpUnit3.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit3.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit3.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``fpUnit4.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit4.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit4.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``fpUnit5.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit5.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit5.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``fpUnit6.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit6.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit6.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``fpUnit7.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit7.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit7.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``fpUnit8.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit8.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"fpUnit8.out").Replace("\r",""),
        sw.ToString()
    StringAssert.Contains(expected, actual)


//</FloatingPointUnitTests>