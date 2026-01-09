module AdventOfCode2025.Day6

open System
open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day6-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day6-input.txt")



let readWorksheet fn =
    let lines =
        File.ReadLines fn |> Seq.filter (fun line -> line.Length > 0) |> Seq.toArray

    let operandLine (line: string) =
        line.Split(' ') |> Seq.filter (fun p -> p.Length > 0) |> Seq.toArray


    let lastIndex = lines.Length - 1
    let operands = lines[0 .. lastIndex - 1] |> Seq.map operandLine |> array2D

    let operators =
        lines.[lastIndex].Split(' ')
        |> Seq.filter (fun p -> p.Length > 0)
        |> Seq.toArray

    operands, operators

let reduceWith operator =
    match operator with
    | "+" -> Seq.fold (+) 0L
    | "*" -> Seq.fold (*) 1L
    | _ -> failwithf "Unknown operator: %s" operator


let part1Operands (column: string array) = column |> Seq.map int64


let applyOperators (operands: string array2d) (operators: string array) =
    let colCoun = Array2D.length2 operands

    seq { 0 .. colCoun - 1 }
    |> Seq.map (fun c ->
        let op = operators[c]
        let columnValues = operands.[*, c] |> part1Operands
        let columnTotal = columnValues |> reduceWith op
        do printfn "Column %d with operator %s reduces to %d" c op columnTotal
        columnTotal)
    |> Seq.sum


let readWorksheet2 fn =
    let lines =
        File.ReadLines fn |> Seq.filter (fun line -> line.Length > 0) |> Seq.toArray

    let operators = lines.[lines.Length - 1]
    let operands = lines[0 .. lines.Length - 2] |> array2D
    operators, operands

let part2Operands (operands: char array2d) =
    let width = Array2D.length2 operands
    let extractColumn i =
        operands.[*, i]
        |> Seq.filter (fun c -> c <> ' ')
        |> Seq.toArray
        |> String.Concat
        |> int64
    seq { 0 .. width - 1 } |> Seq.map extractColumn


let opWidth (operators: string) =
    let opfn = reduceWith (string operators[0])
    let nextIndex = operators[1..] |> Seq.tryFindIndex (fun c -> c <> ' ')
    match nextIndex with
    | None -> opfn, operators.Length
    | Some i -> opfn, i

let rec opSeq (operators: string) (operands: char array2d)=
    if operators.Length = 0 then
        seq []
    else
        //do printfn "Operators remaining: |%s|" operators
        let opfn, w = opWidth operators
        let opands = part2Operands operands.[*, 0..w - 1]
        seq {
            yield opfn opands
            yield! opSeq operators.[w + 1 ..] operands.[*, w + 1 ..]
        }

let part2 operators operands = 
    opSeq operators operands |> Seq.sum
    