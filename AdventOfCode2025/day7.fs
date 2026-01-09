module AdventOfCode2025.Day7

open System
open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day7-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day7-input.txt")

let readDiagram fn =
    let lines =
        File.ReadLines fn |> Seq.filter (fun line -> line.Length > 0) |> Seq.toArray
    lines |> array2D


let findStart row = 
    let index = Seq.findIndex (fun c -> c = 'S') row
    index

let p1SplitSet (beams) (row: char array) : int*(int array)=
    let folder (sc, nb) b = 
        match row[b] with
        | '.' -> sc, nb |> Set.add b
        | '^' -> sc+1, nb |> Set.add (b - 1) |> Set.add (b + 1)
        | _ -> failwith "wtfb"
    let sc, nb = 
        beams |> Seq.fold folder (0, Set.empty<int>)
    sc, Set.toArray nb


// answer: 1630
let part1TachyonSplitCount (diagram: char array2d) : int =
    let initialBeam = [| findStart diagram[0, *] |]
    let rowCount = (Array2D.length1 diagram) - 1

    let initialState = (0, initialBeam)
    let folder (sc, beams) row =
        let c, nb = p1SplitSet beams row
        sc + c, nb
    seq { 1 .. rowCount - 1} 
        |> Seq.map (fun r -> diagram[r, *])
        |> Seq.fold folder initialState
        |> fst

let p2SplitAll (beams) (row: char array) =
    
    beams |> Seq.collect (fun b ->
        match row[b] with
        | '.' -> [ b ]
        | '^' -> [ b - 1; b+1 ]
        | _ -> failwith "wtfb"
    )

// too slow, @ row 80/140 it is 6s for len 63863743
let part2TachyonSplitCount (diagram: char array2d) : int =
    let initialBeam = [| findStart diagram[0, *] |]
    let rowCount = (Array2D.length1 diagram) - 1

    seq { 1 .. rowCount - 1} 
        |> Seq.filter (fun i -> i % 2 = 0)
        |> Seq.map (fun r -> diagram[r, *])
        |> Seq.fold p2SplitAll initialBeam
        |> Seq.length
    

let pathCount diagram  =
    let rec looper pos = 
        let x,y = pos
        if y >= Array2D.length1 diagram then
            1L
        else
            match diagram[y,x] with
            | '.' -> looper  (x, y+2)
            | '^' -> (looper  (x-1, y+2)) + (looper  (x+1, y+2))
            | _ as e -> failwith $"wtfb {pos} got {e}"
    let start = findStart diagram[0, *]
    do printfn "starting at (%A, 2)" start
    looper (start, 2)

// answer: 47857642990160L
let pathCountByRow (diagram: char array2d) =
    let rowcount = (Array2D.length1 diagram)
    let width = Array2D.length2 diagram
    let initials = Array.init width (fun i -> 1L)
    let spotCount (state: int64 array) (row: char array) x : int64 =
        let get x : int64 = 
            if x < 0 then 0L
            elif x > state.Length - 1 then 0L
            else state[x]
        match row[x] with
        | '.' -> get x
        | '^' -> get (x - 1)  + get (x + 1)
        | _ as e -> failwith $"wtfb {x} got {e}"
    let rower (state: int64 array) row = 
        state |> Array.mapi (fun i  _ -> spotCount state row i)
    seq{ 2..2..(rowcount-1) } 
    |> Seq.rev 
    |> Seq.map (fun i -> 
        do printfn $"{i}"
        diagram[i, *])
    |> Seq.fold rower initials
