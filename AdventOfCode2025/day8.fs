module AdventOfCode2025.Day8

open System
open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day8-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day8-input.txt")


type Point = int64 * int64 * int64


type Circuit = Point Set

let read fn =
    File.ReadLines fn
    |> Seq.filter (fun l -> l.Length > 0)
    |> Seq.map (fun l ->
        let parts = l.Split ','
        int64 parts[0], int64 parts[1], int64 parts[2])

let sortedPoints (points: Point seq) : Point seq = Seq.sort points

type Pair = Point * Point

let rec pairup (points: Point list) : Pair list =
    match points with
    | a :: tail -> List.append (List.map (fun b -> (a, b)) tail) (pairup tail)
    | [] -> []


let sortedPairs points =
    let pairs = points |> Seq.toList |> pairup

    let distance (p: Pair) =
        let a, b = p
        let x1, y1, z1 = a
        let x2, y2, z2 = b
        let xd, yd, zd = x1 - x2, y1 - y2, z1 - z2
        xd * xd + yd * yd + zd * zd

    pairs |> Seq.sortBy distance


let buildCircuits n sortedPairs =
    let initialState = [], n
    let hasWires (_, n) = n >= 0

    let merge (circuits: Circuit list) (p: Pair) : Circuit list option =
        let a, b = p
        let ain = circuits |> Seq.tryFind (Set.contains a)
        let bin = circuits |> Seq.tryFind (Set.contains b)

        let addItemToCircuit item found =
            circuits |> List.map (fun c -> if c = found then Set.add item found else c)

        match ain, bin with
        | Some ac, Some bc ->
            if ac = bc then
                None
            else
                let remainder = circuits |> List.except [| ac; bc |]
                let newCircuit = Set.union ac bc
                Some(newCircuit :: remainder)

        | Some ac, None -> Some(addItemToCircuit b ac)
        | None, Some bc -> Some(addItemToCircuit a bc)
        | None, None -> Some(([ a; b ] |> Set.ofList) :: circuits)

    let update state pair =
        let circuits, wiresRemaining = state

        match merge circuits pair with
        | Some circuits -> circuits, wiresRemaining - 1
        | None ->
            do printfn "pair %A already in same circuit" pair
            circuits, wiresRemaining - 1

    let result, _ =
        sortedPairs
        |> Seq.scan update initialState
        |> Seq.takeWhile hasWires
        |> Seq.last

    result

// answer 75582
let part1 fn n =
    read fn
    |> sortedPairs
    |> buildCircuits n
    |> Seq.map Set.count
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (fun s i -> s * i) 1


let p2Build points =
    let sp = sortedPairs points
    let initialCircuits = points |> Seq.toList |> List.map Set.singleton

    let merge circuits pair =
        let a, b = pair
        let eitherIn c = Set.contains a c || Set.contains b c
        let included, excluded = circuits |> List.partition eitherIn
        Set.unionMany included :: excluded

    let initialState = initialCircuits, None

    let scanner (circuits, _) pair = merge circuits pair, Some pair  
    
    sp
    |> Seq.scan scanner initialState
    |> Seq.find (fun (circuits, pairopt) -> circuits.Length = 1)
    |> snd

// answer is 59039696L
let p2 fn = 
    match read fn |> p2Build with
    | Some (a, b) ->
        let (x1,_,_), (x2,_,_) = a,b
        x1 * x2
    | None -> failwith "whoops"
