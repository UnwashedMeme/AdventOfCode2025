module Day5Tests

open Expecto
open NUnit.Framework
open FsUnit
open AdventOfCode2025.Day5b


[<Tests>]
let ``overlap tests`` =
    testTheory
              "overlap works correctly"
              [ 
                (1L, 5L), (4L, 8L), true
                (1L, 5L), (5L, 10L), true
                (10L, 20L), (15L, 25L), true
                (15L, 25L), (10L, 20L), true
                (1L, 10L), (2L, 3L), true
                (2L, 3L), (1L, 10L), true 

                (1L, 5L), (6L, 10L), false
                (6L, 10L), (1L, 5L), false
                (1L, 2L), (3L, 4L), false
                (3L, 4L), (1L, 2L), false ]
          <| fun (range1, range2, expected) ->
              let actualL = overlap range1 range2
              let actualR = overlap range2 range1
              Expect.equal actualL expected "Overlap result L should match expected"
              Expect.equal actualR expected "Overlap result R should match expected"


[<Tests>]
let ``lmerge tests`` =
    testTheory
              "lmerge works correctly"
              [ 
                (1L, 5L), (4L, 8L), (1L, 8L)
                (1L, 5L), (5L, 10L), (1L, 10L)
                (10L, 20L), (15L, 25L), (10L, 25L)
                (1L, 10L), (2L, 3L), (1L, 10L)
                (1L, 5L), (5L, 5L), (1L, 5L)
                ]
          <| fun (range1, range2, expected) ->
              let actualL = lmerge range1 range2
              let actualR = lmerge range2 range1
              Expect.equal actualL expected "Merged range L should match expected"
              Expect.equal actualR expected "Merged range R should match expected"

[<Tests>]
let ``condenseRanges tests`` =
    testTheory
              "condenseRanges works correctly"
              [ 
                [ (1L, 5L); (4L, 8L); (10L, 15L) ], [ (1L, 8L); (10L, 15L) ]
                [ (1L, 5L); (6L, 10L); (11L, 15L) ], [ (1L, 5L); (6L, 10L); (11L, 15L) ]
                [ (1L, 10L); (2L, 3L); (4L, 5L); (2L, 3L)], [ (1L, 10L) ]
                [ (10L, 20L); (5L, 15L); (10L, 20L); (20L, 25L) ], [ (5L, 25L) ] ]
          <| fun (inputRanges, expected) ->
              let actual = condenseRanges inputRanges
              Expect.equal actual expected "Condensed ranges should match expected"