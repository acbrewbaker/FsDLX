#r @"..\..\src\FsDLX.Common\bin\Release\FsDLX.Common.dll"
#load "..\..\src\FsDLX.Common\TestGenInfo.fs"

open FsDLX.Common

let testProjectDir = __SOURCE_DIRECTORY__
let solutionDir = findSolutionDir(testProjectDir)
let inputDir = solutionDir @@ "Project2\Inputs"

let verbose = false

do
    TomasuloTests.IntUnit.generate testProjectDir inputDir verbose


