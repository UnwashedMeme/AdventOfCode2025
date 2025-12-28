module AdventOfCode2025.Day1

open System.IO


// __SOURCE_DIRECTORY__ gives the path to the current file's directory
let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day1-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day1-input.txt")


type Instruction = int

let Parse (line: string) =
    let dir = line.[0]
    let mag = line.[1..]

    match dir with
    | 'L' -> -int mag
    | 'R' -> int mag
    | _ -> failwith ("Invalid direction:" + line)


type Dial = Dial of int

let Spin (dial: Dial) (steps: Instruction) =
    match dial with
    | Dial dial ->
        let mutable newpos = dial + steps

        while newpos < 0 do
            newpos <- newpos + 100

        Dial(newpos % 100)

let SpinCountZero dial steps =
    match dial with
    | Dial pos ->
        // let rec looper newpos zerocount =
        //     match newpos with
        //     | 0 -> newpos, zerocount + 1
        //     | p when p > 100 ->
        //         looper (p - 100) (zerocount + 1)
        //     | p when p < 0 ->
        //         looper (p + 100) (zerocount + 1)
        //     | p when p < 0 ->
        //         looper (p + 100) (zerocount + 1)
        //     | _ -> newpos, zerocount
        // let newpos, zc = looper (pos + steps) 0
        // Dial newpos, zc

        let rec loop pos steps zerocount =
            match steps with
            | 0 -> Dial pos, zerocount
            | n when n >= 100 -> loop pos (steps - 100) (zerocount + 1)
            | n when n <= -100 -> loop pos (steps + 100) (zerocount + 1)
            | n when pos + n >= 100 -> Dial((pos + n) % 100), zerocount + 1
            | n when pos + n < 0 && pos = 0 -> Dial(pos + n + 100), zerocount
            | n when pos + n < 0 && pos > 0 -> Dial(pos + n + 100), zerocount + 1
            | n when pos + n = 0 -> Dial 0, zerocount + 1
            | n when pos + n = 100 -> Dial 0, zerocount + 1
            | n -> Dial(pos + n), zerocount

        loop pos steps 0

type Process = Dial * int

let ApplyInstruction (dial, zeroCount) inst =
    let newdial = Spin dial inst
    do printfn "Orig: %A \tInst: %A \tNew: %A" dial inst newdial

    if newdial = Dial 0 then
        (newdial, zeroCount + 1)
    else
        (newdial, zeroCount)

let Apply2 (dial, zeroCount) inst =
    let newdial, zc = SpinCountZero dial inst
    do printfn "Orig: %A \tInst: %A \tNew: %A" dial inst newdial
    newdial, zeroCount + zc

[<EntryPoint>]
let part1 args =
    let fn = if args.Length > 0 then args.[0] else inputFilename
    do printfn "Reading input file: %s" fn

    let instructions =
        File.ReadLines fn |> Seq.filter (fun line -> line.Length > 0) |> Seq.map Parse

    let start = Dial 50, 0

    let (d, finalcount) = Seq.fold Apply2 start instructions
    printfn "ending dial: %A, zerocount: %A" d finalcount
    0
