open System
open System.IO
open System.Collections.Generic

let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day11-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day11-input.txt")

let readFile fn = fn |> File.ReadLines |> Seq.filter (fun l -> l.Length > 0)

let parseCables (input: string seq) = 
    let parseline (l: string) = 
        match   l.Split ": " with
        | [|head; pieces|] -> head, pieces.Split ' '
        | p -> failwith (sprintf "wtfbbq: %A" p)
    input |> Seq.map parseline |> Map.ofSeq



let allpaths start ends (dl:Map<string,string array>) =
    let rec buildpath  e =
        seq {
            match Map.tryFind e dl with
            | Some opts ->
                for o in opts do
                    //let nr = (o :: route)
                    if o = ends then
                        yield o
                    else 
                        yield! (buildpath  o)
            | None  -> ()
        }
    buildpath start

let part1 fn = 
    fn
    |> readFile 
    |> parseCables
    |> allpaths "you" "out"
    |> Seq.length

//printfn "part1: %A" (part1 inputFilename) // 477


// let countTo dst (dl:Map<string,string array>) =
//     dl 
//     |> Map.toSeq
//     |> Seq.filter (fun (k, v) -> 
//         Array.contains dst v
//     )
//     |> Seq.map fst


let allPathsTo start ends (dl:Map<string,string array>) =
    let mutable known = Map.empty
    let rec looper start = 
        if start = ends then 
            1L
        else
            match Map.tryFind  start known with
            | Some count -> count
            | None -> 
                match Map.tryFind start dl with
                | Some opts ->
                    let total = opts |> Seq.sumBy looper
                    do known <- Map.add start total known
                    total
                | None -> 0L
    looper start

let allPathsTo2 start ends (dl:Map<string,string array>) =
    let mutable known = Map.empty
    let rec looper start = 
        if start = ends then 
            1L
        else
            match Map.tryFind start dl with
                | Some opts ->
                    let optFn o = 
                        match Map.tryFind  o known with
                        | Some count -> count
                        | None -> 
                            let total = looper o
                            do known <- Map.add o total known
                            //do printfn "Wrote %A: %A" o total
                            total
                    opts |> Seq.sumBy optFn
                | None -> 0L               
    let total = looper start
    do printfn "%A -> %A: %A" start ends total
    total



let p2ed = """svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out"""

 
let p2edcables = p2ed.Split '\n' |> parseCables
let p2cables = readFile inputFilename |> parseCables

let part2 cableDiagram =
    let svrdac = 
        cableDiagram  |> allPathsTo2 "svr" "dac"
    let svrfft = 
        cableDiagram  |> allPathsTo2 "svr" "fft"
    let dacfft = 
        cableDiagram |> allPathsTo2 "dac" "fft"
    let fftdac      =
        cableDiagram |> allPathsTo2 "fft" "dac"
    let fftout = 
        cableDiagram |> allPathsTo2 "fft" "out"
    let dacout =
        cableDiagram |> allPathsTo2 "dac" "out"
    let sdfo = svrdac * dacfft * fftout
    let sfdo = svrfft * fftdac * dacout
    printfn "sdfo: %A" sdfo
    printfn "sfdo: %A" sfdo
    sdfo + sfdo
#time;
printfn "part2: %A" (part2 p2cables)

//383307150903216
