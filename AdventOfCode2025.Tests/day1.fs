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
        { Input = "R1"; Expected =  1}
        { Input = "L2"; Expected =  -2}
        { Input = "L240"; Expected =  -240}
        { Input = "R130"; Expected =  130}
    ]

    testCases
    |> List.iter (fun testCase ->
        let actual = Parse testCase.Input
        actual |> should equal testCase.Expected
    )

[<Test>]
let ``spinning the dial works correctly`` () =
    let testCases = [
        Dial 0, 10, Dial 10
        Dial 90, 15, Dial 5
        Dial 5, -10, Dial 95
        Dial 0, -1, Dial 99
        Dial 3, -105, Dial 98
        Dial 2, -307, Dial 95
        Dial 10, 204, Dial 14
    ]

    testCases
    |> List.iter (fun (initialDial, steps, expectedDial) ->
        let actualDial = Spin initialDial steps
        actualDial |> should equal expectedDial
    )