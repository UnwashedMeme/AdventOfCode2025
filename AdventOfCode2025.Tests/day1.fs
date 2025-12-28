module AdventOfCode2025.Tests

open NUnit.Framework
open FsUnit
open AdventOfCode2025.Day1


type TestCase = {
    Input: string
    Expected: Instruction
}

[<Test>]
let ``parsing input works correctly`` () = 
    let testCases = [
        { Input = "R1"; Expected = { Steps = 1 } }
        { Input = "L2"; Expected = { Steps = -2 } }
        { Input = "L240"; Expected = { Steps = -240 } }
        { Input = "R130"; Expected = { Steps = 130 } }
    ]

    testCases
    |> List.iter (fun testCase ->
        let actual = Instruction.Parse testCase.Input
        actual |> should equal testCase.Expected
    )

[<Test>]
let ``spinning the dial works correctly`` () =
    let testCases = [
        { Position = 0 }, 10, { Position = 10 }
        { Position = 90 }, 15, { Position = 5 }
        { Position = 5 }, -10, { Position = 95 }
        { Position = 0 }, -1, { Position = 99 }
        { Position = 3 }, -105, { Position = 98 }
        { Position = 2 }, -307, { Position = 95 }
        { Position = 10 }, 204, { Position = 14 }
    ]

    testCases
    |> List.iter (fun (initialDial, steps, expectedDial) ->
        let actualDial = Spin initialDial steps
        actualDial |> should equal expectedDial
    )