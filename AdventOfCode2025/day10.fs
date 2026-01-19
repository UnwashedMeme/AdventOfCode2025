module AdventOfCode2025.Day10

open System
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Collections.Generic


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day10-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day10-input.txt")

let midFilename = Path.Combine(baseDir, "data/day10-midsize.txt")

type Button = Button of int array


type JoltageLevel =
    | JL of int array

    static member (+)(JL a, JL b) = Array.map2 (+) a b |> JL
    static member (+)(JL a, Button b) = Array.map2 (+) a b |> JL

    static member (-)(JL a, JL b) = Array.map2 (-) a b |> JL


type Machine =
    { diagram: string
      buttonWiringSchematics: Button array
      targetJoltage: JoltageLevel
      heuristic: float }

    member this.enabled state = state = this.diagram


let trimFL (s: string) = s[1 .. s.Length - 2]

let parseMachine (line: string) =
    let parts = line.Split ' '
    let partLen = parts.Length
    let diagram = parts[0] |> trimFL
    let schematicWidth = diagram.Length

    let parseButton (bs: string) : Button =
        let newarr = Array.zeroCreate schematicWidth
        let coords = (trimFL bs).Split ',' |> Array.map int

        for c in coords do
            Array.set newarr c 1

        Button newarr

    let buttons = parts[1 .. partLen - 2] |> Array.map parseButton
    let maxh = buttons |> Seq.map (fun (Button b) -> b |> Array.sum) |> Seq.max


    let parseJoltage js =
        (trimFL js).Split ',' |> Array.map int |> JL

    { diagram = diagram
      buttonWiringSchematics = buttons
      heuristic = float maxh
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




let targetPowerLevel (m: Machine) (JL current) = 
    let (JL target) = m.targetJoltage
    Array.forall2 (=) current target
let safePowerLevel (m: Machine) (JL current) = 
    let (JL target) = m.targetJoltage
    Array.forall2 (<=) current target

let adjustJoltageBF m =
    let initialJoltage = JL(Array.create (m.diagram.Length) 0)
    let rec finder count qjls =
        let qjls = Seq.cache qjls
        seq {
            yield!
                qjls
                |> Seq.choose (fun jl ->
                    if targetPowerLevel m jl then
                        Some count
                    else
                        None)

            let nextLevel =
                seq {
                    for jl in qjls do
                        for b in m.buttonWiringSchematics do
                            let njl = jl + b

                            if safePowerLevel m njl then
                                yield njl
                }
            yield! finder (count + 1) nextLevel
        }
    finder 0 (List.singleton initialJoltage) |> Seq.head


let adjustJoltageBF2 m =
    let initialJoltage = JL(Array.zeroCreate (m.diagram.Length))
    
    let allStates = HashSet<JoltageLevel>()
    //let mutable allStates = Set.empty
    let mutable visited = 0
    let rec finder count  candidates = 
        let count = count + 1
        let mutable next = List.empty
        seq {
            for jl in candidates do
                visited <- visited + 1
                for b in m.buttonWiringSchematics do
                    let njl = jl + b
                    if targetPowerLevel m njl then
                        do printfn "Finish %A \tw/ count=%A\t visited=%A\t remaining=%A" njl count visited (List.length candidates)
                        yield count
                    elif safePowerLevel m njl && allStates.Add(njl) then
                        next <- njl :: next
            yield! finder count next
        }
    finder 0 (List.singleton initialJoltage) |> Seq.head


let adjustJoltage2 m =
    let initialJoltage = JL(Array.create (m.diagram.Length) 0)
    let mutable allStates = Set.empty
    let mutable visited = 0

    let rec finder count qjls =
        let count = count + 1
        //do printfn "examining level %A with %A tracked states" count (Set.count allStates)
        seq {
            let mutable next = List.empty

            for jl in qjls do
                visited <- visited + 1
                for b in m.buttonWiringSchematics do
                    let njl = jl + b
                    if targetPowerLevel m njl then
                        do printfn "Finish %A \tw/ count=%A\t visited=%A\t remaining=%A" njl count visited (List.length qjls)
                        yield count
                    elif safePowerLevel m njl && not (Set.contains njl allStates) then
                        allStates <- Set.add njl allStates
                        next <- njl :: next

            yield! finder count next
        }

    finder 0 (List.singleton initialJoltage) |> Seq.head


type JLP = int * JoltageLevel

let hdTotalMinSteps (m: Machine) current =
    let (JL diff) = m.targetJoltage - current
    (float (diff |> Array.sum)) / m.heuristic

let hdTotalMinSteps2 (m: Machine) current =
    let (JL tj) = m.targetJoltage
    let (JL cj) = current
    let diff = Array.fold2 (fun s i j -> s + (i - j)) 0 tj cj 
    (float diff) / m.heuristic

let hdFurthestMinSteps (m: Machine) current =
    let (JL diff) = m.targetJoltage - current
    float (Array.max diff)

let hdFurthestMinSteps2 (m: Machine) current =
    let (JL tj) = m.targetJoltage
    let (JL cj) = current
    let max = (Array.fold2 (fun s a b -> 
        max s (a - b)
    ) 0 tj cj
    )
    float max

let hdMax (m: Machine) current =
    max (hdTotalMinSteps m current) (hdFurthestMinSteps m current)

// A* search
let adjustJoltage (hd:Machine->JoltageLevel->float) m =
    let queue = PriorityQueue<JLP, float>()
    let initialJoltage = JL(Array.zeroCreate m.diagram.Length)
    do queue.Enqueue((0, initialJoltage), (hd m initialJoltage))
    let allStates = HashSet<JoltageLevel>()
    //let mutable allStates = Set.empty
    let mutable visited = 0
    seq {
        while queue.Count > 0 do
            let (count, jl) = queue.Dequeue()

            if not (allStates.Contains(jl)) then
                do allStates.Add(jl)

                if targetPowerLevel m jl then
                    do printfn "Finish %A \tw/ count=%A\t visited=%A\t remaining=%A" jl count visited queue.Count
                    yield count
                else
                    do visited <- visited + 1
                    let ohd = hd m jl
                    //do printfn "Checking count %A on %A; remaining %A" count jl queue.Count
                    let count = count + 1

                    for b in m.buttonWiringSchematics do
                        let njl = jl + b
                        if safePowerLevel m njl then // && not (allStates.Contains(njl)) then
                            let nhd = hd m njl
                            if ohd > 1.00001 + nhd then
                                failwith (sprintf "non-consistent %A > %A" ohd (1.0 + nhd))
                            else
                                //do printfn "Priority: %A = %A + %A" (float count + hd) count hd
                                queue.Enqueue((count, njl), float count + nhd)
                    //else do printfn "Discarding count %A" count
    }
    |> Seq.head

let part2 (aj: Machine -> int) fn =
    let ms = readMachines fn

    ms
    |> Seq.mapi (fun i m ->
        let buttoncount = aj m
        //do printfn "Machine %A took %A" i buttoncount
        buttoncount)
    |> Seq.sum

// let ms = readMachines exampleFilename;;
// let ims = readMachines inputFilename;;
// let inital = JL (Array.create 5 0);;

// exampleFilename |> (part2 adjustJoltageBF2)
// exampleFilename |> (part2 adjustJoltage2)
// exampleFilename |> (part2 (adjustJoltage hdTotalMinSteps))
// exampleFilename |> (part2 (adjustJoltage hdFurthestMinSteps))

// midFilename |> (part2 adjustJoltageBF2)
// midFilename |> (part2 adjustJoltage2)
// midFilename |> (part2 (adjustJoltage hdTotalMinSteps))
// midFilename |> (part2 (adjustJoltage hdTotalMinSteps2))
// midFilename |> (part2 (adjustJoltage hdFurthestMinSteps))
// midFilename |> (part2 (adjustJoltage hdFurthestMinSteps2))
// midFilename |> (part2 (adjustJoltage hdMax))

// ims |> Seq.filter (fun m -> 20 > m.hdist (JL (Array.zeroCreate m.diagram.Length))) |> Seq.map adjustJoltage |> Array.ofSeq;;
// ims[9] |> adjustJoltage;;
