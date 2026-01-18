module AdventOfCode2025.Day10

open System
open System.IO
open System.Numerics
open System.Runtime.InteropServices


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day10-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day10-input.txt")


type Button = Button of int array
type JoltageLevel = JL of int array

type Machine =
    { diagram: string
      buttonWiringSchematics: Button seq
      targetJoltage: JoltageLevel }

    member this.enabled state = state = this.diagram

let trimFL (s: string) = s[1 .. s.Length - 2]


let parseMachine (line: string) =
    let parts = line.Split ' '
    let partLen = parts.Length
    let diagram = parts[0] |> trimFL
    let schematicWidth = diagram.Length

    let parseButton (bs: string) : Button =
        let newarr = Array.create schematicWidth 0
        let coords = (trimFL bs).Split ',' |> Array.map int

        for c in coords do
            Array.set newarr c 1

        Button newarr

    let parseJoltage js =
        (trimFL js).Split ',' |> Array.map int |> JL

    { diagram = diagram
      buttonWiringSchematics = parts[1 .. partLen - 2] |> Seq.map parseButton
      targetJoltage = parts[partLen - 1] |> parseJoltage }

let readMachines fn =
    fn
    |> File.ReadLines
    |> Seq.filter (fun l -> l.Length > 0)
    |> Seq.map parseMachine
    |> Array.ofSeq

let alterState (state: string) (Button buttons) =
    let ch i c =
        if buttons[i] = 1 then
            match c with
            | '#' -> '.'
            | '.' -> '#'
            | _ -> failwith "wtfbbq"
        else
            c

    state |> String.mapi ch

let pressButtons m =
    let len = m.diagram.Length
    let initialState = String.replicate len "."

    let rec finder count states =
        if Set.contains m.diagram states then
            count
        else
            let newstateset =
                seq {
                    for s in states do
                        for b in m.buttonWiringSchematics do
                            let bs = alterState s b

                            if not (Set.contains bs states) then
                                yield bs

                }
                |> Seq.fold (fun fs s -> Set.add s fs) states

            finder (count + 1) newstateset

    finder 0 (Set.singleton initialState)


// part1 answer 558
let part1 fn =
    fn |> readMachines |> Seq.map pressButtons |> Seq.sum

let addPowerBasic (JL jl) (Button b) = Array.map2 (+) b jl |> JL

let addPowerVectorized (Button b) (JL jl) =
    let len = b.Length
    let result = Array.zeroCreate<int> len

    let bSpan = b.AsSpan()
    let jlSpan = jl.AsSpan()
    let resSpan = result.AsSpan()

    let bVec = MemoryMarshal.Cast<int, Vector<int>>(bSpan)
    let jlVec = MemoryMarshal.Cast<int, Vector<int>>(jlSpan)
    let resVec = MemoryMarshal.Cast<int, Vector<int>>(resSpan)

    for i in 0 .. bVec.Length - 1 do
        resVec.[i] <- bVec.[i] + jlVec.[i]

    for i in (bVec.Length * Vector<int>.Count) .. len - 1 do
        resSpan.[i] <- bSpan.[i] + jlSpan.[i]

    JL result

let targetPowerLevel (JL current) (JL target) = Array.forall2 (=) current target
let safePowerLevel (JL current) (JL target) = Array.forall2 (<=) current target

let adjustJoltage m =
    let addPower = addPowerBasic

    let initialJoltage = JL(Array.create (m.diagram.Length) 0)

    let rec finder count qjls =
        let qjls = Seq.cache qjls

        seq {
            yield!
                qjls
                |> Seq.choose (fun jl ->
                    if targetPowerLevel jl m.targetJoltage then
                        Some count
                    else
                        None)

            let nextLevel =
                seq {
                    for jl in qjls do
                        for b in m.buttonWiringSchematics do
                            let njl = addPower jl b
                            if safePowerLevel njl m.targetJoltage then
                                yield njl
                 }

            yield! finder (count + 1) nextLevel
        }

    finder 0 (List.singleton initialJoltage) |> Seq.head

let adjustJoltage2 m =
    let addPower = addPowerBasic

    let initialJoltage = JL(Array.create (m.diagram.Length) 0)

    let mutable allStates = Set.empty
    let rec finder count qjls =
        let count = count + 1
        do printfn "examining level %A with %A tracked states" count (Set.count allStates)
        seq {
            let mutable next = List.empty
            for jl in qjls do
                for b in m.buttonWiringSchematics do
                    let njl = addPower jl b
                    if targetPowerLevel njl m.targetJoltage then
                        yield count
                    elif safePowerLevel njl m.targetJoltage && not (Set.contains njl allStates) then
                        allStates <- Set.add njl allStates
                        next <- njl :: next
            
            yield! finder count next
        }

    finder 0 (List.singleton initialJoltage) |> Seq.head



let part2 (aj:Machine->int) fn =
    let ms = readMachines fn
    ms 
    |> Seq.mapi (fun i m ->
        let buttoncount = aj m
        do printfn "Machine %A took %A" i buttoncount
        buttoncount)
    |> Seq.sum