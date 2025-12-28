module AdventOfCode2025.Day1

open System.IO


// __SOURCE_DIRECTORY__ gives the path to the current file's directory
let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day1-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day1-input.txt")


type Dial = Dial of int
type Instruction = int

let Spin (dial: Dial) (steps: int) =
    match dial with
    | Dial dial ->
        let mutable newpos = dial + steps
        
        while newpos < 0 do
            newpos <- newpos + 100

        Dial (newpos % 100)

let Parse(line: string) =
        let dir = line.[0]
        let mag = line.[1..]

        match dir with
        | 'L' -> -int mag
        | 'R' -> int mag
        | _ -> failwith ("Invalid direction:" + line)

type Process = Dial * int

let ApplyInstruction (dial, zeroCount) inst =
        let newdial = Spin dial inst
        do printfn "Orig: %A \tInst: %A \tNew: %A" dial inst newdial
        if newdial = Dial 0 then
            (newdial, zeroCount + 1)
        else
            (newdial, zeroCount)

[<EntryPoint>]
let part1 args =
    let fn = 
        if args.Length > 0 then
            args.[0]
        else
            inputFilename
    do printfn "Reading input file: %s" fn
    let instructions =
        File.ReadLines fn
        |> Seq.filter (fun line -> line.Length > 0)
        |> Seq.map Parse

    let start = Dial 50, 0

    let (d, finalcount) = Seq.fold ApplyInstruction start instructions
    printfn "Example ending dial: %A, zerocount: %A" d finalcount
    0
