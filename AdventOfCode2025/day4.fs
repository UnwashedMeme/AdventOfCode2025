module AdventOfCode2025.Day4

open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day4-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day4-input.txt")


let readFile fn =
    File.ReadLines fn
    |> Seq.filter (fun line -> line.Length > 0)
    |> Seq.toArray
    |> array2D

let subMat mat (x, y) : char array2d =
    let xmin = max 0 (x - 1)
    let ymin = max 0 (y - 1)
    let xmax = min (Array2D.length1 mat - 1) (x + 1)
    let ymax = min (Array2D.length2 mat - 1) (y + 1)
    mat.[ymin..ymax, xmin..xmax]

let isRoll = function
    | '@' -> true
    | _ -> false

let countRolls (mat: char array2d) =
    let countRow y =
        mat[y, *] |> Seq.filter isRoll |> Seq.length

    let rows = Array2D.length1 mat
    [ 0 .. rows - 1 ] |> Seq.map countRow |> Seq.sum

let countRollsAt mat (x, y) = 
    let xmin = max 0 (x - 1)
    let ymin = max 0 (y - 1)
    let xmax = min (Array2D.length1 mat - 1) (x + 1)
    let ymax = min (Array2D.length2 mat - 1) (y + 1)
    let countRow y =
        mat[y, xmin..xmax] |> Seq.filter isRoll |> Seq.length
    [ymin .. ymax] |> Seq.map countRow |> Seq.sum

let check (mat: char array2d) (x, y) =
    //let sub = subMat mat (x, y)
    let at = mat[y, x]

    match at with
    | '.' -> '.'
    | '@' when countRollsAt mat (x, y) < 5 -> 'x'
    | '@' -> '@'
    | 'x' -> 'x'
    | _ -> failwith "invalid character"


let removeRolls (mat: char array2d) =
    mat |> Array2D.mapi (fun y x _ -> check mat (x, y))

let printMat (mat: char array2d) =
    let rows = Array2D.length1 mat
    let cols = Array2D.length2 mat
    do printfn ""

    for y in 0 .. rows - 1 do
        for x in 0 .. cols - 1 do
            do printf "%c" mat[y, x]
        do printfn ""
    mat

let countRemoved (mat: char array2d) =
    let rows = Array2D.length1 mat
    let countRow y =
        mat[y, *] |> Seq.filter (fun c -> c = 'x') |> Seq.length

    [ 0 .. rows - 1 ] |> Seq.map countRow |> Seq.sum

// part1's answer is 1489
let part1 (args: string[]) =
    let fn = if args.Length > 0 then args.[0] else inputFilename
    let mat = readFile fn
    let total = mat |> removeRolls |> countRemoved
    do printfn "\nTotal: %d" total
    0

let rec stabilize mat = 
    let newmat = removeRolls mat //|> printMat
    if newmat = mat then
        mat
    else
        stabilize newmat

// part2's answer is 8890
// with submat Real: 00:00:00.168, CPU: 00:00:00.211, GC gen0: 11, gen1: 0, gen2: 0
let part2 (args: string[]) =
    let fn = if args.Length > 0 then args.[0] else inputFilename
    let mat = readFile fn
    mat |> stabilize |> countRemoved |> printfn "Total: %d"
    0
