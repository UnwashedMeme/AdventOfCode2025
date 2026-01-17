module AdventOfCode2025.Day9

open System
open System.IO


let baseDir = __SOURCE_DIRECTORY__

let exampleFilename = Path.Combine(baseDir, "data/day9-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day9-input.txt")


type Point = int64 * int64


type Pair = Point * Point

let readPoints fn =
    fn
    |> File.ReadLines
    |> Seq.filter (fun l -> l.Length > 0)
    |> Seq.map (fun l ->
        let parts = l.Split ','
        int64 parts[0], int64 parts[1])


let pairup (points: Point array) =
    seq {
        for i in 0 .. points.Length - 1 do
            for j in i + 1 .. points.Length - 1 do
                yield points.[i], points.[j]
    }

let area (p: Pair) =
    let a, b = p
    let x1, y1 = a
    let x2, y2 = b
    (1L + (abs (x1 - x2))) * (1L + (abs (y1 - y2)))


type pr =
    | Corner
    | Edge
    | Outside
    | Inside

let checkPair (points: array<Point>) pair =
    let a, b = pair
    let (x1, y1), (x2, y2) = pair
    let minx, miny = min x1 x2, min y1 y2
    let maxx, maxy = max x1 x2, max y1 y2
    let x1, y1, x2, y2 = minx, miny, maxx, maxy

    let inside (x, y) =
        minx < x && x < maxx && miny < y && y < maxy

    if Seq.exists inside points then
        false
    else
        let corner p =
            p = (x1, y1) || p = (x1, y2) || p = (x2, y1) || p = (x2, y2)

        let outside (x, y) =
            x < minx || maxx < x || y < miny || maxy < y

        let between a b t = a < t && t < b
        let includes a b t = a <= t && t <= b

        let classify =
            function
            | p when corner p -> Corner
            | p when inside p -> Inside
            | p when outside p -> Outside
            | _ -> Edge

        let classifyAndPrint p =
            let c = classify p
            do printfn "%A: %A" p c
            c

        seq { 0 .. (points.Length + 4) }
        |> Seq.map (fun i -> points.[i % points.Length])
        |> Seq.map classifyAndPrint
        |> Seq.windowed 3
        |> Seq.map (fun a ->
            match a with
            | [| Inside; _; _ |] -> false
            | [| _; Inside; _ |] -> false
            | [| _; _; Inside |] -> false

            | [| Corner; Edge; Corner |] -> false
            | [| Corner; Corner; _ |] -> true
            | [| _; Corner; Corner |] -> true

            | [| Edge; Edge; _ |] -> false
            | [| _; Edge; Edge |] -> false

            | [| Outside; Corner; _ |] -> true
            | [| Corner; Outside; _ |] -> true
            | [| Outside; Outside; Outside |] -> true
            | [| _; Outside; Outside |] -> true
            | [| Outside; _; Outside |] -> true
            | [| Outside; Outside; _ |] -> true
            | [| a; b; c |] ->
                do printfn "unclassified %A, %A, %A" a b c
                false)
        |> Seq.forall id



type Orientation =
    | Collinear
    | Clockwise
    | CounterClockwise

/// Determines if two line segments intersect.
///
/// Theory:
/// Uses the concept of orientation of ordered triplets (p, q, r).
/// The orientation is determined by the sign of the cross product of vectors (q-p) and (r-q).
/// Two segments (p1,q1) and (p2,q2) intersect if and only if:
/// 1. General Case: (p1,q1,p2) and (p1,q1,q2) have different orientations AND (p2,q2,p1) and (p2,q2,q1) have different orientations.
/// 2. Special Case: The triplets are collinear and the projections of the segments on the x and y axes overlap.
///
/// This implementation returns true for both proper crossings and cases where segments touch or overlap collinearly.
let checkIntersect (a: Pair) (b: Pair) =
    let p1, q1 = a
    let p2, q2 = b

    let orientation p q r =
        let px, py = p
        let qx, qy = q
        let rx, ry = r
        let v = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)

        if v = 0L then Collinear
        elif v > 0L then Clockwise
        else CounterClockwise

    let onSegment p q r =
        let px, py = p
        let qx, qy = q
        let rx, ry = r
        qx <= max px rx && qx >= min px rx && qy <= max py ry && qy >= min py ry

    let o1 = orientation p1 q1 p2
    let o2 = orientation p1 q1 q2
    let o3 = orientation p2 q2 p1
    let o4 = orientation p2 q2 q1

    if o1 <> o2 && o3 <> o4 then true
    elif o1 = Collinear && onSegment p1 p2 q1 then true
    elif o2 = Collinear && onSegment p1 q2 q1 then true
    elif o3 = Collinear && onSegment p2 p1 q2 then true
    elif o4 = Collinear && onSegment p2 q1 q2 then true
    else false

/// Determines if two line segments intersect strictly.
///
/// Theory:
/// Similar to checkIntersect, this uses orientation tests.
/// However, it enforces a stricter condition:
/// 1. The segments must cross (orientations must differ for both pairs).
/// 2. Collinear cases (touching, overlapping) are explicitly rejected.
///
/// This is useful for determining if a rectangle edge crosses a polygon boundary,
/// where touching the boundary is permitted but crossing it is not.
let strictIntersect (a: Pair) (b: Pair) =
    let p1, q1 = a
    let p2, q2 = b

    let orientation p q r =
        let px, py = p
        let qx, qy = q
        let rx, ry = r
        let v = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)

        if v = 0L then Collinear
        elif v > 0L then Clockwise
        else CounterClockwise

    let o1 = orientation p1 q1 p2
    let o2 = orientation p1 q1 q2
    let o3 = orientation p2 q2 p1
    let o4 = orientation p2 q2 q1

    if o1 <> Collinear && o2 <> Collinear && o3 <> Collinear && o4 <> Collinear &&
       o1 <> o2 && o3 <> o4 then true
    else false

/// Checks if a point is inside or on the boundary of a polygon.
///
/// Theory:
/// 1. Boundary Check: Iterates through all edges. If the point is collinear with an edge (cross product is 0)
///    and lies within the bounding box of that edge, it is on the boundary (returns true).
/// 2. Ray Casting (Even-Odd Rule): If not on the boundary, a semi-infinite horizontal ray is cast from the point.
///    The algorithm counts how many times this ray intersects the edges of the polygon.
///    - Odd number of intersections: Point is Inside.
///    - Even number of intersections: Point is Outside.
///    This implementation uses integer arithmetic to avoid floating point errors.
let isPointInPolygon (points: Point array) (p: Point) =
    let x, y = p

    let onSegment p1 p2 =
        let x1, y1 = p1
        let x2, y2 = p2
        let cp = (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1)

        if cp = 0L then
            x >= min x1 x2 && x <= max x1 x2 && y >= min y1 y2 && y <= max y1 y2
        else
            false

    let n = points.Length

    if
        (Seq.pairwise points |> Seq.exists (fun (p1, p2) -> onSegment p1 p2))
        || onSegment points.[n - 1] points.[0]
    then
        true
    else
        let mutable inside = false
        let mutable j = n - 1

        for i in 0 .. n - 1 do
            let xi, yi = points.[i]
            let xj, yj = points.[j]

            let intersect =
                if (yi > y) <> (yj > y) then
                    let num = (xj - xi) * (y - yi)
                    let den = yj - yi
                    let lhs = (x - xi) * den
                    if den > 0L then lhs < num else lhs > num
                else
                    false

            if intersect then
                inside <- not inside

            j <- i

        inside

let checkPairInt (points: Point array) (pair: Pair) =
    let a, b = pair
    let (x1, y1), (x2, y2) = pair


    let inside (x, y) =
        min x1 x2 < x && x < max x1 x2 && min y1 y2 < y && y < max y1 y2

    // 1. Check if any polygon vertex is strictly inside the rectangle.
    if Seq.exists inside points then
        false
    else
        let oa, ob = (x1, y2), (x2, y1)
        let corners = [| a; b; oa; ob |]

        // 2. Check if all rectangle corners are inside the polygon.
        if not (Array.forall (isPointInPolygon points) corners) then
            false
        else
            // 3. Check if any rectangle edge strictly intersects any polygon edge.
            let rectEdges = 
                [| (a, oa); (oa, b); (b, ob); (ob, a) |]
            let polyEdges = 
                seq {
                    yield! Seq.pairwise points
                    yield (Array.last points, Array.head points)
                }

            let hasBadIntersection =
                polyEdges
                |> Seq.exists (fun pEdge ->
                    rectEdges |> Seq.exists (fun rEdge -> strictIntersect rEdge pEdge)
                )
            
            not hasBadIntersection



// part1 answer: 4765757080
// part2 4148829355 -- too high
// part2 4620185529 -- too high
// part2 193219548 -- too low
// part2 1305201780 -- wrong
// part2 1299330660 -- wrong
// part2 1299330660 -- ((94679L, 48332L), (4386L, 33943L))
// part2 1498673376 -- ((5664L, 67242L), (94679L, 50407L))
let maxArea (points: seq<Point>) =
    let points = points |> Array.ofSeq
    let pairs = pairup points

    let maxpair = pairs |> Seq.sortByDescending area |> Seq.find (checkPairInt points)
    let a = area maxpair
    do printfn "// part2 %A -- %A" a maxpair

let part2 = readPoints inputFilename |> maxArea
