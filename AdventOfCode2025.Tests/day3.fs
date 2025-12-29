module day3

open Expecto
open NUnit.Framework
open FsUnit
open AdventOfCode2025.Day3


type TestCase = { Input: string; Expected: int64 }

[<Tests>]
let ``part1`` =
    let testCases =
        [ { Input = "987654321111111"
            Expected = 98 }
          { Input = "811111111111119"
            Expected = 89 }
          { Input = "234234234234278"
            Expected = 78 }
          { Input = "818181911112111"
            Expected = 92 } ]

    testTheory "maxOfBank" testCases (fun testCase ->
        let actual = int64 (max2OfBank testCase.Input)
        Expect.equal actual testCase.Expected "mismatch")


[<Tests>]
let ``part2`` =
    let testCases =
        [ { Input = "987654321111111"
            Expected = 987654321111L }
          { Input = "811111111111119"
            Expected = 811111111119L }
          { Input = "234234234234278"
            Expected = 434234234278L }
          { Input = "818181911112111"
            Expected = 888911112111L} ]

    testTheory "max12ofBank" testCases (fun testCase ->
        let actual = max12ofBank 12 testCase.Input
        Expect.equal actual testCase.Expected "mismatch")
