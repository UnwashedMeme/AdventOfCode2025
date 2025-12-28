module AdventOfCode2025.Day1

open System.IO


// __SOURCE_DIRECTORY__ gives the path to the current file's directory
let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day1-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day1-input.txt")


type Dial =
    { Position: int }

    member this.Zero() = this.Position = 0


let Spin (dial: Dial) (steps: int) =
    let mutable newpos = dial.Position + steps

    while newpos < 0 do
        newpos <- newpos + 100

    { dial with Position = newpos % 100 }

type Instruction =
    { Steps: int }

    static member Parse(line: string) =
        let dir = line.[0]
        let mag = line.[1..]

        match dir with
        | 'L' -> { Steps = -int mag }
        | 'R' -> { Steps = int mag }
        | _ -> failwith ("Invalid direction:" + line)

type Process =
    { dial: Dial
      zeroCount: int }

let ApplyInstruction proc inst  =
        let newdial = Spin proc.dial inst.Steps
        do printfn "Orig: %A \tInst: %A \tNew: %A" proc.dial.Position inst.Steps newdial.Position
        if newdial.Position = 0 then
            { proc with
                dial = newdial
                zeroCount = proc.zeroCount + 1 }
        else
            { proc with dial = newdial }

[<EntryPoint>]
let part1 args =
    let fn = 
        if args.Length > 0 then
            args.[0]
        else
            inputFilename
    let instructions =
        File.ReadLines fn
        |> Seq.filter (fun line -> line.Length > 0)
        |> Seq.map Instruction.Parse


    let start =
        { dial = { Position = 50 }
          zeroCount = 0 }

    let ending = Seq.fold ApplyInstruction start instructions
    printfn "Example ending dial: %A, zerocount: %A" ending.dial.Position ending.zeroCount
    0
