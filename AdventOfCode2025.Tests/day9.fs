module Day9Tests

open Expecto
open NUnit.Framework
open FsUnit
open AdventOfCode2025.Day9


[<Tests>]
let ``exampleRectangles`` =
    let points = readPoints exampleFilename |> Array.ofSeq

    testTheory 
        "overlap works correctly" 
        [ 
            ((7L, 3L), (11L, 1L)), Some 15L;
            ((9L, 7L), (9L, 5L)), Some 3L;
         ] 
        (fun (pair, expected) ->
            let result = if checkPairInt points pair then Some(area pair) else None
            Expect.equal result expected "didn't match")

