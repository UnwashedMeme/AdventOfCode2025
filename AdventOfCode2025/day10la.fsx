#r "../packages/MathNet.Numerics/lib/netstandard2.0/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp/lib/netstandard2.0/MathNet.Numerics.FSharp.dll"


open System
open System.IO
open System.Numerics
open System.Runtime.InteropServices
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double

let baseDir = __SOURCE_DIRECTORY__

// Keep these comments
//19784 -- too low
//20301 -- too low

let exampleFilename = Path.Combine(baseDir, "data/day10-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day10-input.txt")

let midFilename = Path.Combine(baseDir, "data/day10-midsize.txt")

let trimFL (s: string) = s[1 .. s.Length - 2]

type Button = Button of int array

type Machine =
    { buttonWiringSchematics: Matrix<double>
      targetJoltage: Vector<double>}

    static member Parse (line: string) =
        let parts = line.Split ' '
        let partLen = parts.Length
        let joltageDiagram = parts[partLen - 1]
        let schematicWidth = ((trimFL joltageDiagram).Split ',').Length

        let parseButton (bs: string) =
            let newarr = Array.zeroCreate schematicWidth
            let coords = (trimFL bs).Split ',' |> Array.map int
            for c in coords do
                Array.set newarr c 1.0
            newarr
        let buttons = parts[1 .. partLen - 2] |> Array.map parseButton

        let parseJoltage js =
            (trimFL js).Split ',' |> Array.map double |> DenseVector.OfArray

        { buttonWiringSchematics = DenseMatrix.OfColumnArrays buttons
          targetJoltage = joltageDiagram |> parseJoltage }

let readMachines fn =
    fn
    |> File.ReadLines
    |> Seq.filter (fun l -> l.Length > 0)
    |> Seq.map Machine.Parse
    |> Array.ofSeq


let rec combinations n (list: int list) =
    match n, list with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) ->
        List.map (fun y -> x :: y)  (combinations (k-1) xs) @ combinations k xs

/// Solves BW = A for W >= 0 minimizing Sum(W)
/// This is a Linear Programming problem. Since N is small, we iterate Basic Feasible Solutions.
let solveMinL1 (b: Matrix<double>) (c: Vector<double>) =
    let rows = b.RowCount
    let cols = b.ColumnCount
    let tolerance = 1e-6

    let indices = [0 .. cols - 1]
    let maxK = min rows cols
    
    // Iterate through all possible subset sizes of columns to find a basic feasible solution.
    // This handles square singular matrices and underdetermined systems robustly.
    // We only need to check subsets up to the rank of the matrix (at most rows).
    seq { 1 .. maxK }
    |> Seq.collect (fun k -> combinations k indices)
    |> Seq.choose (fun idxList ->
        // Construct submatrix from selected columns
        let subMatrix = DenseMatrix.OfColumnVectors(idxList |> List.map (fun i -> b.Column(i)))
        
        try
            let qr = subMatrix.QR()
            if qr.IsFullRank then
                let subW = qr.Solve(c)
                // Check if it is a valid solution (residual ~ 0)
                let residual = (subMatrix * subW) - c
                if residual.L2Norm() < tolerance then
                    // Check feasibility (non-negativity) and integrality
                    let isInteger (x: double) = abs(x - round(x)) < tolerance
                    if subW |> Seq.forall (fun x -> x >= -tolerance && isInteger x) then
                        // Construct full vector
                        let fullW = DenseVector.Create(cols, 0.0)
                        List.iter2 (fun idx (val': double) -> fullW.[idx] <- if val' < 0.0 then 0.0 else val') idxList (Seq.toList subW)
                        Some (fullW :> Vector<double>)
                    else
                        None
                else
                    None
            else
                None
        with _ -> None // Singular matrix or solve failure
    )
    // We want to minimize the sum (L1 norm for non-negative vectors)
    |> Seq.minBy (fun w -> w.Sum())


let minimizeButtons (m:Machine) =
    // Try to find the optimal non-negative float solution first
    let w = solveMinL1 m.buttonWiringSchematics m.targetJoltage
    let sum = w.Sum()
    let res = m.buttonWiringSchematics * w
    if (m.targetJoltage - res).L2Norm() > 1e-4 then
        printfn "Bad solution: %32A yields JL=%32A but TJL=%32A" w res m.targetJoltage
    else 
        printfn "Found solution: %32A with sum %f" w sum
    sum

let es = readMachines exampleFilename
let ms = readMachines midFilename
let ims = readMachines inputFilename

printfn "--- Examples ---"
es |> Array.iter (fun m -> 
    try 
        minimizeButtons m |> ignore 
    with e -> printfn "Error on example: %O" e
)

printfn "--- Input ---"
let totalInput = 
    ims 
    |> Array.mapi (fun i m -> 
        printfn "Processing machine %d..." i
        let res = minimizeButtons m
        res
    )
    |> Array.sum

printfn "Total Min Sum for Input: %f" totalInput