module day2

open System.IO

let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day2-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day2-input.txt")

let read fn =
    File.ReadAllText(fn).Split(',')
    |> Array.toList
    |> List.map (fun s ->
        let parts = s.Split('-')
        int64 parts.[0], int64 parts.[1])

let expand (a: int64, b: int64) = seq { for n in a..b -> n }


let candidates1 (a: int64, b: int64) =
    expand (a,b) |> Seq.filter (fun n -> n.ToString().Length % 2 = 0)


let badid1 (a: int64) =
    let s = a.ToString()
    let halflen = s.Length / 2
    let left = s.Substring(0, halflen)
    let right = s.Substring(halflen)
    left = right

let badid2 (a: int64) =
    let s = a.ToString()
    let slen = s.Length
    seq {1 .. slen / 2} |> Seq.exists (fun i ->
        if slen % i > 0 then
            false
        else
            let count = slen / i
            if count * i = slen then
                let left = s.Substring(0, i)
                let expanded = String.replicate count left
                s = expanded
            else 
                do printfn "Length %d not divisible by %d" slen i
                false
    )


let part1 (args:string[]) =
    let fn = if args.Length > 0 then args.[0] else inputFilename
    do printfn "Reading input file: %s" fn
    let ranges = read fn
    let candies = ranges |> Seq.map candidates1 |> Seq.concat |> Seq.filter badid1 
    do printfn "Candidates: %A" candies
    do printfn "candy count: %d" (candies |> Seq.length)
    do printfn "Total: %d" (candies |> Seq.sum)
    0

[<EntryPoint>]
// part2's answer is 21932258645
let part2 args =
    let fn = if args.Length > 0 then args.[0] else inputFilename
    do printfn "Reading input file: %s" fn
    let ranges = read fn
    let candies = ranges |> Seq.map expand |> Seq.concat |> Seq.filter badid2 
    do printfn "Candidates: %A" candies
    do printfn "candy count: %d" (candies |> Seq.length)
    do printfn "Total: %d" (candies |> Seq.sum)
    0