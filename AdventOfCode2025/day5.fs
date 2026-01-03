module AdventOfCode2025.Day5

open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day5-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day5-input.txt")


let readFile fn =
    let freshRanges =
        File.ReadLines fn
        |> Seq.takeWhile (fun line -> line <> "")
        |> Seq.map (fun line ->
            let parts = line.Split('-') |> Seq.map int64 |> Seq.toArray
            parts[0], parts[1])
        |> Seq.toArray


    let ingredients =
        File.ReadLines fn
        |> Seq.skip (freshRanges.Length + 1)
        |> Seq.filter (fun line -> line.Length > 0)
        |> Seq.map int64
        |> Seq.toArray

    freshRanges, ingredients

let readFreshRanges fn =
    File.ReadLines fn
    |> Seq.takeWhile (fun line -> line <> "")
    |> Seq.map (fun line ->
        let parts = line.Split('-')
        let left,right = int64 parts[0], int64 parts[1]
        if left < right then left, right else right, left)
    |> Seq.toArray


let isFresh (ranges: (int64 * int64) array) (ingredient: int64) =
    ranges
    |> Seq.takeWhile (fun (start, _) -> start <= ingredient)
    |> Seq.exists (fun (_, rend) -> ingredient <= rend)

// part1's answer is 598
// Real: 00:00:00.005, CPU: 00:00:00.005, GC gen0: 0, gen1: 0, gen2: 0
// Real: 00:00:00.003, CPU: 00:00:00.002, GC gen0: 0, gen1: 0, gen2: 0
// val it: int = 598
let part1 fn =
    let freshRanges, ingredients = readFile fn
    let freshRanges = freshRanges |> Array.sortBy (fun (start, _) -> start)
    ingredients |> Seq.filter (isFresh freshRanges) |> Seq.length



type Tree =
    | Leaf of int64 * int64
    | Node of int64 * int64 * Tree * Tree
    | Empty

let rec leafCount =
    function
    | Empty -> 0
    | Leaf(_, _) -> 1
    | Node(_, _, l, r) -> leafCount l + leafCount r

let rec leavesOf t = 
    seq {
        match t with
        | Empty -> ()
        | Leaf _ -> yield t
        | Node(_, _, l, r) ->
            yield! leavesOf l
            yield! leavesOf r
    }

let span tree = 
    match tree with
    | Empty -> 0L, 0L
    | Leaf(s, e) -> s, e
    | Node(s, e, _, _) -> s, e


let rec ingredientCount tree =
    match tree with
    | Empty -> 0L
    | Leaf (ls, le) -> 1L + le - ls
    | Node(_, _, l, r) -> ingredientCount l + ingredientCount r


let overlaps t nl = 
    let ts,te = span t
    match nl with
    | Leaf(ls, le) when te < ls -> false
    | Leaf(ls, le) when le < ts -> false
    | Node(ns, ne, _, _) when te < ns -> false
    | Node(ns, ne, _, _) when ns < ts -> false
    | _ -> true
    


// still doesn't work
let rec merge (tree: Tree) (nl: Tree) =
    let mergeLeaves l1 l2 =
        let s1, e1 = l1
        let s2, e2 = l2
        // l1 consumes
        if s1 <= s2 && e2 <= e1 then Leaf l1
        // l2 consumes
        elif s2 <= s1 && e1 <= e2 then Leaf l2
        // l1 to left of l2
        elif e1 < s2 then Node(s1, e2, Leaf l1, Leaf l2)
        // l2 to left of l1
        elif e2 < s1 then Node(s2, e1, Leaf l2, Leaf l1)
        else Leaf (min s1 s2, max e1 e2)

    let nodeLeaf (lt:Tree) (rt:Tree) nl =
        let nls, nle = nl 
        let lts, lte = span lt
        let rts, rte = span rt
        do printfn "nodeLeaf lt: (%d,%d), rt: (%d,%d), nl: (%d,%d)" lts lte rts rte nls nle
        // nl is superset of both
        if nls <= lts && rte <= nle then
            Leaf nl
        // entirely in the middle
        elif lte < nls && nle < rts then
            let mergeleft= merge lt (Leaf nl)
            Node (lts, rte, mergeleft, rt)
        // entirely to the left
        elif nle < lts then
            Node (nls, rte, 
                Leaf nl, Node (lts, rte, lt, rt))
        // entirely to the right
        elif rte < nls then
            Node (lts, nle,
                Node (lts, rte, lt, rt),  Leaf nl)
        else
            let newleft = 
                if overlaps lt (Leaf nl) then
                    merge lt (Leaf (nls, min nle lte))
                else
                    lt
            let newRight =
                if overlaps rt (Leaf nl) then
                    merge rt (Leaf (max nls rts, nle))
                else
                    rt
            let midleaf =
                if nls < rts && lte < nle then
                    Leaf (max (lte + 1L) nls, min nle (rts - 1L))
                else 
                    Empty
            let newLeftAndMiddle = merge newleft midleaf
            Node (min lts nls, max rte nle, newLeftAndMiddle, newRight)

    match tree, nl with
    | Empty, _ -> nl
    | _, Empty -> tree
    | Leaf (s1,e1), Leaf (s2,e2) ->  
        mergeLeaves (s1,e1) (s2,e2)
    | Node (_,_, tl, rt), Leaf (nls, nle) ->
        nodeLeaf tl rt (nls, nle)
    | Leaf (nls, nle), Node (_,_, lt, rt) ->
        nodeLeaf lt rt (nls, nle)
    | Node (_,_, tl, rt), Node (_,_, lt2, rt2) ->
        merge (merge tl lt2) (merge rt rt2)


let buildTree (ranges: (int64 * int64) array) =
    ranges
    |> Seq.map (fun (s, e) -> Leaf(s, e))
    |> Seq.fold merge Empty

        
let printTree tree =
    let rec loop indent tree =
        match tree with
        | Empty -> printfn "%sEmpty" indent
        | Leaf(s, e) -> printfn "%sLeaf(%d,%d)" indent s e
        | Node(s, e, l, r) ->
            printfn "%sNode(%d,%d)" indent s e
            loop (indent + "  ") l
            loop (indent + "  ") r

    do loop "" tree
    do printfn "Ingredient count: %d" (ingredientCount tree)


let freshRanges = readFreshRanges exampleFilename;;
let inputRanges = readFreshRanges inputFilename;;
// freshRanges[0..2] |> buildTree |> printTree;;
// freshRanges[0..3] |> buildTree |> printTree;;
// let t = freshRanges[0..2] |> buildTree ;;
// t |> printTree;;
// let l = Leaf freshRanges[3];;
// merge t l |> printTree;;
// freshRanges |> buildTree |> printTree;;
// inputRanges |> buildTree |> printTree;;
    


let part2  =
    
    let ranges = readFreshRanges inputFilename
    let t = buildTree ranges
    ingredientCount t



