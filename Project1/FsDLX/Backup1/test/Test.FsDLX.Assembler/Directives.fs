module Test.FsDLX.Directive

open FsDLX.Assembler
open System
open System.IO
open System.Text.RegularExpressions
open NUnit.Framework
open FsUnit

open FsDLX

let directive = @"
.text 13
.data 6
.align 2
.asciiz ""Hello there!"", ""ends here""
.double -3333.367
.float 22.5
.word 32, 127, 1023, -1
.space 16"

//
//[<Test>]
//let ``text directive`` () =
//    let pattern = Patterns.Directive.text
//    let matches = Regex(pattern).Matches(directive)
//    let expected = ".text 13"
//    matches.[0].Value |> should equal expected
//
//[<Test>]
//let ``data directive`` () =
//    let pattern = Patterns.Directive.data
//    let matches = Regex(pattern).Matches(directive)
//    let expected = ".data 6"
//    matches.[0].Value |> should equal expected
//
//[<Test>]
//let ``align directive`` () =
//    let pattern = Patterns.Directive.align
//    let matches = Regex(pattern).Matches(directive)
//    let expected = ".align 2"
//    matches.[0].Value |> should equal expected
//
//[<Test>]
//let ``asciiz directive`` () =
//    let pattern = Patterns.Directive.asciiz
//    let matches = Regex(pattern).Matches(directive)
//    let expected = @".asciiz ""Hello there!"", ""ends here"""
//    matches.[0].Value |> should equal expected
//
//[<Test>]
//let ``double directive`` () =
//    let pattern = Patterns.Directive.double
//    let matches = Regex(pattern).Matches(directive)
//    let expected = ".double -3333.367"
//    matches.[0].Value |> should equal expected
//
//[<Test>]
//let ``float directive`` () =
//    let pattern = Patterns.Directive.float
//    let matches = Regex(pattern).Matches(directive)
//    let expected = ".float 22.5"
//    matches.[0].Value |> should equal expected
//
//[<Test>]
//let ``word directive`` () =
//    let pattern = Patterns.Directive.word
//    let matches = Regex(pattern).Matches(directive)
//    let expected = ".word 32, 127, 1023, -1"
//    matches.[0].Value |> should equal expected
//
//[<Test>]
//let ``space directive`` () =
//    let pattern = Patterns.Directive.space
//    let matches = Regex(pattern).Matches(directive)
//    let expected = ".space 16"
//    matches.[0].Value |> should equal expected

//
//[<Test>]
//let ``text directive - dlx to hex`` () =
//    let f = Conversions.DLX2HEX.text
//    ()
//
//[<Test>]
//let ``data directive - dlx to hex`` () =
//    let f = Conversions.DLX2HEX.data
//    ()
//
//[<Test>]
//let ``align directive - dlx to hex`` () =
//    let str = "2"
//    let pc = 0x0000000c |> uint32
//    let expected = "00000020"
//    let convert = Conversions.DLX2HEX.align
//    pc |> convert str |> should equal expected
//
////[<Test>]
////let ``asciiz directive - dlx to hex`` () =
////    let pc = 0u
////    let line = "hello there"
////    let expected = @"00000000: 68656c6c6f20746865726500 #""hello there"""
////    let convert = Conversions.DLX2HEX.asciiz
////    (pc, line) ||> convert |> snd |> should equal expected
//
//[<Test>]
//let ``double directive - dlx to hex`` () =
//    let str = "-3333.367"
//    let expected = "c0aa0abbe76c8b44"
//    let convert = Conversions.DLX2HEX.double
//    str |> convert |> should equal expected
//
//[<Test>]
//let ``float directive - dlx to hex`` () =
//    let f = Conversions.DLX2HEX.float
//    ()
//
//[<Test>]
//let ``word directive - dlx to hex`` () =
//    let f = Conversions.DLX2HEX.word
//    ()
//
//[<Test>]
//let ``space directive - dlx to hex`` () =
//    let f = Conversions.DLX2HEX.space
//    ()

//type Instruction =
//    | Directive of string * string list
//    | Itype of string * string * string
//    | Rtype of string * string * string * string * string * string
//    | Jtype of string * string
//
//    member this.Parse line =
//        let matchDirective = Regex(Patterns.Directive.directive).Match(line)
//        

//[<Test>]
//let ``Directive 1`` () =
//    let dlxfile = @"C:\Users\User\OneDrive\CS5483\Project1\Inputs\directive1.dlx"
//    let hexfile = @"C:\Users\User\OneDrive\CS5483\Project1\Inputs\directive1.hex"
//    
//    let dlx = dlxfile |> File.ReadAllLines
//    let hex = hexfile |> File.ReadAllLines
//
//    let pc = 0u
//
//    let matches = Regex(Patterns.Directive.directive).Match(dlxfile |> File.ReadAllText)
//    
//    let matchDirective line = Regex(Patterns.Directive.directive).Match(line)
//
//    printfn "%A" matches.Success
    