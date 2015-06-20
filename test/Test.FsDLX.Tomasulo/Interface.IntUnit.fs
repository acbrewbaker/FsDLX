module Test.FsDLX.Tomasulo.Interface

open System
open System.IO
open NUnit.Framework
open FsDLX.Tomasulo

//<IntUnitTests>
[<NCrunch.Framework.Isolated>]
let [<Test>] ``intUnit1.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit1.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit1.out").Trim(),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``intUnit2.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit2.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit2.out").Trim(),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``intUnit3.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit3.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit3.out").Trim(),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``intUnit4.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit4.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit4.out").Trim(),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``intUnit5.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit5.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit5.out").Trim(),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``intUnit6.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit6.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit6.out").Trim(),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``intUnit7.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit7.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit7.out").Trim(),
        sw.ToString()
    StringAssert.Contains(expected, actual)

[<NCrunch.Framework.Isolated>]
let [<Test>] ``intUnit8.hex`` () =
    let simulator = Simulator(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit8.hex",false)
    use sw = new StringWriter() in Console.SetOut(sw)
    simulator.Run()
    let expected, actual =
        File.ReadAllText(@"H:\FsDLX\Project2\Inputs" @@ @"intUnit8.out").Trim(),
        sw.ToString()
    StringAssert.Contains(expected, actual)


//</IntUnitTests>
