module AdventOfCode2025.Day5b

open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day5-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day5-input.txt")


let readFreshRanges fn =
    File.ReadLines fn
    |> Seq.takeWhile (fun line -> line <> "")
    |> Seq.map (fun line ->
        let parts = line.Split('-')
        let left,right = int64 parts[0], int64 parts[1]
        if left < right then left, right else right, left)
    |> Seq.toList

let overlap x y = 
    let s1, e1 = x
    let s2, e2 = y
    not (e1 < s2 || e2 < s1)

let lmerge x y =
    let s1, e1 = x
    let s2, e2 = y
    let mr = (min s1 s2, max e1 e2)
    do printfn "merging %A and %A to %A" x y mr
    mr 

let rec condenseRanges (inputRanges: List<(int64 * int64)>) : List<(int64 * int64)> =
    let tryMerge x y = 
        if overlap x y then
            lmerge x y
        else
            y

    match inputRanges with
    | [] -> []
    | [x] -> [x]
    | x :: tail ->
        if Seq.exists (overlap x) tail then
            do printfn "Found overlap for %A" x
            tail |> List.map (tryMerge x) |> condenseRanges
        else
            x :: condenseRanges tail


let icount r = 
    let s, e = r
    1L + e - s

let report (lst: List<int64*int64>) =
    let c = lst |> List.map icount |> List.sum
    let len = lst |> List.length
    printfn "Condensed to %d ranges, total ingredients: %d" len c

// answer is 360341832208407
let part2 = 
    
    let ranges = readFreshRanges inputFilename
    ranges |> condenseRanges |> report