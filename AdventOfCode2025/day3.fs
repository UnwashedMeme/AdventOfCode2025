module AdventOfCode2025.Day3

open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day3-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day3-input.txt")


let readFile fn = 
    File.ReadLines fn |> 
    Seq.filter (fun line -> line.Length > 0) 


let max2OfBank (bank:string):int =
    let len = bank.Length
    let firstpart = bank.Substring(0, len-1)
    let firstnum = firstpart |> Seq.max
    let firstindex = firstpart.IndexOf(firstnum)
    let secondpart = bank.Substring(firstindex+1)
    let secondnum = secondpart |> Seq.max
    //do printfn "firstnum: %c, secondnum: %c" firstnum secondnum
    int(sprintf "%c%c" firstnum secondnum)

let max12ofBank (count:int) (bank:string):int64 =
    let rec loop (remaining:int) (avail:string) (acc:string) =
        if remaining = 0 then
            acc
        elif remaining > avail.Length then
            failwith "not enough digits"
        else
            let rem = remaining - 1
            let subst = avail.Substring(0, avail.Length - rem)
            let num = subst |> Seq.max
            let index = subst.IndexOf num
            loop rem (avail.Substring(index+1)) (acc + string num)
    int64 (loop 12 bank "")
    

// part1's answer is 17092
let part1 (args:string[])= 
    let fn = if args.Length > 0 then args.[0] else inputFilename
    readFile fn |> Seq.map max2OfBank |> Seq.sum |> printfn "Total: %d" 
    0

// part2's answer is 170147128753455
let part2 (args:string[])= 
    let fn = if args.Length > 0 then args.[0] else inputFilename
    readFile fn |> Seq.map (max12ofBank 12) |> Seq.sum |> printfn "Total: %d"
    0