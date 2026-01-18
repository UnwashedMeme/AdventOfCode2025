module AdventOfCode2025.Day10

open System
open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day10-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day10-input.txt")


type Button = | Button of int Set

type Machine =
    { diagram: string
      buttonWiringSchematics: Button seq
      joltage: string }
    member this.enabled state =
        state = this.diagram

let parseMachine (line: string) =
    let trimFL (s:string) = s[1..s.Length - 2]
    let parts = line.Split ' '
    let partLen = parts.Length
    let diagram = trimFL parts[0]
    let joltage = parts[partLen - 1]
    let buttons = 
        parts[1 .. partLen - 2] 
        |> Seq.map trimFL
        |> Seq.map (fun s-> s.Split ',' |> Seq.map int |> Set.ofSeq |> Button)
    
    {diagram=diagram; buttonWiringSchematics=buttons; joltage=joltage}

let readMachines fn =
    fn |> File.ReadLines |> Seq.filter (fun l -> l.Length > 0)
    |> Seq.map parseMachine 
    |> Array.ofSeq

let alterState (state:string) (Button buttons) =
    let ch i c=
        if Set.contains i buttons then
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
                        
                } |> Seq.fold (fun fs s -> Set.add s fs) states
            finder (count + 1) newstateset
    finder 0 (Set.singleton initialState)

// part1 answer 558
let part1 fn = 
    fn 
    |> readMachines
    |> Seq.map pressButtons
    |> Seq.sum
