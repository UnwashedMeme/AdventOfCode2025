module Day1

open Expecto
open NUnit.Framework
open FsUnit
open AdventOfCode2025.Day1


type TestCase =
    { Input: string; Expected: Instruction }

[<Tests>]
let ``parsing tests`` =
    testList
        "parsing"
        [ let testCases =
              [ { Input = "R1"; Expected = 1 }
                { Input = "L2"; Expected = -2 }
                { Input = "L240"; Expected = -240 }
                { Input = "R130"; Expected = 130 } ]

          testTheory "parsing input works correctly" testCases (fun testCase ->
              let actual = Parse testCase.Input
              Expect.equal actual testCase.Expected) ]

[<Tests>]
let ``spinning tests`` =
    testList
        "spinning"
        [ testTheory
              "spinning the dial works correctly"
              [ Dial 0, 10, Dial 10
                Dial 90, 15, Dial 5
                Dial 5, -10, Dial 95
                Dial 0, -1, Dial 99
                Dial 3, -105, Dial 98
                Dial 2, -307, Dial 95
                Dial 10, 204, Dial 14 ]
          <| fun (initialDial, steps, expectedDial) ->
              let actualDial = Spin initialDial steps
              Expect.equal actualDial expectedDial "Spinning the dial should yield the expected result"


          testTheory
              "spinning the dial with zero counts works correctly"
              [ Dial 0, 10, (Dial 10, 0)
                Dial 0, -1, (Dial 99, 0)
                Dial 0, 100, (Dial 0, 1)
                Dial 0, -100, (Dial 0, 1)
                Dial 90, 15, (Dial 5, 1)
                Dial 5, -10, (Dial 95, 1)
                Dial 3, -105, (Dial 98, 2)
                Dial 2, -307, (Dial 95, 4)
                Dial 10, 204, (Dial 14, 2) ]
          <| fun (initialDial, steps, (expectedDial, expectedZeroCount)) ->
              let actualDial, actualZeroCount = SpinCountZero initialDial steps
              Expect.equal actualDial expectedDial "wrong dial value"
              Expect.equal actualZeroCount expectedZeroCount "wrong zero count" ]
