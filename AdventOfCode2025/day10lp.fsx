#r "../packages/MathNet.Numerics/lib/netstandard2.0/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp/lib/netstandard2.0/MathNet.Numerics.FSharp.dll"

open System
open System.IO
open System.Numerics
open System.Collections.Generic
open MathNet.Numerics


// Attempts:
// 19784 -- too low
// 20301 -- too low
// 29317 -- just right


// Helper functions for B&B logic
let bigRatFloor (r: BigRational) =
    let num = r.Numerator
    let den = r.Denominator
    let (q, rem) = BigInteger.DivRem(num, den)
    if rem = BigInteger.Zero then q
    else if r < BigRational.Zero then q - 1I
    else q

let bigRatCeil (r: BigRational) =
    let num = r.Numerator
    let den = r.Denominator
    let (q, rem) = BigInteger.DivRem(num, den)
    if rem = BigInteger.Zero then q
    else if r > BigRational.Zero then q + 1I
    else q

let baseDir = __SOURCE_DIRECTORY__
let exampleFilename = Path.Combine(baseDir, "data/day10-example.txt")
let inputFilename = Path.Combine(baseDir, "data/day10-input.txt")

let trimFL (s: string) = s[1 .. s.Length - 2]

type Machine =
    { buttonWiringSchematics: BigRational[,]
      targetJoltage: BigRational[] }

    static member Parse (line: string) =
        let parts = line.Split ' '
        let partLen = parts.Length
        let joltageDiagram = parts[partLen - 1]
        let schematicWidth = ((trimFL joltageDiagram).Split ',').Length

        let parseButton (bs: string) =
            let newarr = Array.zeroCreate schematicWidth
            let coords = (trimFL bs).Split ',' |> Array.map int
            for c in coords do
                Array.set newarr c 1
            newarr
            
        let buttons = parts[1 .. partLen - 2] |> Array.map parseButton
        let numButtons = buttons.Length
        
        let matrix = Array2D.create schematicWidth numButtons BigRational.Zero
        for c in 0 .. numButtons - 1 do
            let btnArr = buttons.[c]
            for r in 0 .. schematicWidth - 1 do
                if btnArr.[r] = 1 then
                    matrix.[r, c] <- BigRational.One
                    
        let parseJoltage js =
            (trimFL js).Split ',' 
            |> Array.map (fun s -> BigRational.FromInt(int s))

        { buttonWiringSchematics = matrix
          targetJoltage = joltageDiagram |> parseJoltage }

let readMachines fn =
    fn
    |> File.ReadLines
    |> Seq.filter (fun l -> l.Length > 0)
    |> Seq.map Machine.Parse
    |> Array.ofSeq

type SimplexResult =
    | Optimal of BigRational * BigRational[]
    | Infeasible
    | Unbounded

let solveSimplex (A: BigRational[,]) (b: BigRational[]) (c: BigRational[]) =
    let rows = Array2D.length1 A
    let cols = Array2D.length2 A
    
    // Canonical form requires b >= 0
    let A_work = Array2D.copy A
    let b_work = Array.copy b
    for i in 0 .. rows - 1 do
        if b_work.[i] < BigRational.Zero then
            for j in 0 .. cols - 1 do
                A_work.[i, j] <- -A_work.[i, j]
            b_work.[i] <- -b_work.[i]

    let numVars = cols + rows
    let tableau = Array2D.create (rows + 1) (numVars + 1) BigRational.Zero

    // Fill Tableau
    for r in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            tableau.[r, j] <- A_work.[r, j]
        tableau.[r, cols + r] <- BigRational.One 
        tableau.[r, numVars] <- b_work.[r]

    // Phase 1 Objective: Minimize sum(z).
    // Reduced costs for x: -sum(A_col).
    for j in 0 .. cols - 1 do
        let mutable sum = BigRational.Zero
        for r in 0 .. rows - 1 do
            sum <- sum + A_work.[r, j]
        tableau.[rows, j] <- -sum
        
    tableau.[rows, numVars] <- -(Array.sum b_work)

    // Helper: Perform pivot on (leaveRow, enterCol)
    let doPivot (leaveRow: int) (enterCol: int) =
        let pivotVal = tableau.[leaveRow, enterCol]
        for j in 0 .. numVars do
            tableau.[leaveRow, j] <- tableau.[leaveRow, j] / pivotVal
        
        for i in 0 .. rows do
            if i <> leaveRow then
                let factor = tableau.[i, enterCol]
                if factor <> BigRational.Zero then
                    for j in 0 .. numVars do
                        tableau.[i, j] <- tableau.[i, j] - factor * tableau.[leaveRow, j]

    let rec pivotPhase1 (iter: int) =
        if iter > 1000 then false else
        let limit = cols 
        let mutable minVal = BigRational.Zero
        let mutable enterCol = -1
        
        for j in 0 .. limit - 1 do
            if tableau.[rows, j] < minVal then
                minVal <- tableau.[rows, j]
                enterCol <- j
        
        if enterCol = -1 then
            true 
        else
            let mutable minRatio = BigRational.FromBigInt(1000000000000I)
            let mutable leaveRow = -1
            let mutable foundRatio = false
            
            for i in 0 .. rows - 1 do
                let y = tableau.[i, enterCol]
                if y > BigRational.Zero then
                    let ratio = tableau.[i, numVars] / y
                    if not foundRatio || ratio < minRatio then
                        minRatio <- ratio
                        leaveRow <- i
                        foundRatio <- true
            
            if leaveRow = -1 then
                false 
            else
                doPivot leaveRow enterCol
                pivotPhase1 (iter + 1)

    if not (pivotPhase1 0) then Infeasible
    else
        if tableau.[rows, numVars] <> BigRational.Zero then Infeasible
        else
            // DRIVE ARTIFICIALS OUT OF BASIS
            for i in 0 .. rows - 1 do
                // Find basic column for row i
                let mutable basicCol = -1
                for j in 0 .. numVars - 1 do
                    if tableau.[i, j] = BigRational.One && tableau.[rows, j] = BigRational.Zero then
                        let mutable othersZero = true
                        for k in 0 .. rows - 1 do
                            if k <> i && tableau.[k, j] <> BigRational.Zero then othersZero <- false
                        if othersZero then basicCol <- j
                
                // If basic column is artificial (>= cols)
                if basicCol >= cols then
                    // Try to pivot out
                    let mutable swapCol = -1
                    for j in 0 .. cols - 1 do
                        if tableau.[i, j] <> BigRational.Zero then
                            swapCol <- j
                    
                    if swapCol <> -1 then
                        doPivot i swapCol

            // Phase 2
            for j in 0 .. numVars do tableau.[rows, j] <- BigRational.Zero
            for j in 0 .. cols - 1 do tableau.[rows, j] <- c.[j]
                
            for i in 0 .. rows - 1 do
                let mutable basicCol = -1
                for j in 0 .. cols - 1 do
                    if tableau.[i, j] = BigRational.One then
                        let mutable othersZero = true
                        for k in 0 .. rows - 1 do
                            if k <> i && tableau.[k, j] <> BigRational.Zero then othersZero <- false
                        if othersZero then basicCol <- j
                
                if basicCol <> -1 then
                    let factor = tableau.[rows, basicCol]
                    if factor <> BigRational.Zero then
                        for j in 0 .. numVars do
                            tableau.[rows, j] <- tableau.[rows, j] - factor * tableau.[i, j]
            
            let rec pivotPhase2 (iter: int) =
                if iter > 1000 then false else
                let limit = cols 
                let mutable minVal = BigRational.Zero
                let mutable enterCol = -1
                
                for j in 0 .. limit - 1 do
                    if tableau.[rows, j] < minVal then
                        minVal <- tableau.[rows, j]
                        enterCol <- j
                
                if enterCol = -1 then true
                else
                    let mutable minRatio = BigRational.FromBigInt(1000000000000I)
                    let mutable leaveRow = -1
                    let mutable foundRatio = false
                    
                    for i in 0 .. rows - 1 do
                        let y = tableau.[i, enterCol]
                        if y > BigRational.Zero then
                            let ratio = tableau.[i, numVars] / y
                            if not foundRatio || ratio < minRatio then
                                minRatio <- ratio
                                leaveRow <- i
                                foundRatio <- true
                    
                    if leaveRow = -1 then false // Unbounded
                    else
                        doPivot leaveRow enterCol
                        pivotPhase2 (iter + 1)

            if not (pivotPhase2 0) then Unbounded
            else
                let objVal = -tableau.[rows, numVars] 
                let resultX = Array.create cols BigRational.Zero
                
                for i in 0 .. rows - 1 do
                     let mutable basicCol = -1
                     for j in 0 .. cols - 1 do
                         if tableau.[i, j] = BigRational.One then
                             let mutable othersZero = true
                             for k in 0 .. rows - 1 do
                                 if k <> i && tableau.[k, j] <> BigRational.Zero then othersZero <- false
                             if othersZero then basicCol <- j
                     
                     if basicCol <> -1 then
                         resultX.[basicCol] <- tableau.[i, numVars]
                         
                Optimal (objVal, resultX)

let solveILP (machine: Machine) =
    let A = machine.buttonWiringSchematics
    let b = machine.targetJoltage
    let rows = Array2D.length1 A
    let cols = Array2D.length2 A
    let c = Array.create cols BigRational.One
    
    let mutable bestObj: Option<BigRational> = None
    let mutable bestSol: Option<BigRational[]> = None
    
    let queue = new Queue<(int * bool * bigint) list>()
    queue.Enqueue([])
    
    let mutable iter = 0
    let maxIter = 5000
    
    while queue.Count > 0 && iter < maxIter do
        iter <- iter + 1
        let constraints = queue.Dequeue()
        
        let numConstraints = constraints.Length
        let augRows = rows + numConstraints
        let augCols = cols + numConstraints
        let augA = Array2D.create augRows augCols BigRational.Zero
        let augB = Array.create augRows BigRational.Zero
        let augC = Array.create augCols BigRational.Zero
        
        for r in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                augA.[r, j] <- A.[r, j]
            augB.[r] <- b.[r]
            
        for j in 0 .. cols - 1 do
            augC.[j] <- c.[j]
            
        // removed infeasibleStart check
        for i in 0 .. numConstraints - 1 do
            let (vIdx, isUpper, bound) = constraints.[i]
            let rIdx = rows + i
            let sIdx = cols + i
            
            augA.[rIdx, vIdx] <- BigRational.One
            if isUpper then
                augA.[rIdx, sIdx] <- BigRational.One
                augB.[rIdx] <- BigRational.FromBigInt bound
            else
                augA.[rIdx, sIdx] <- -BigRational.One
                augB.[rIdx] <- BigRational.FromBigInt bound
                
        match solveSimplex augA augB augC with
        | Optimal (obj, fullSol) ->
            let isBetter = 
                match bestObj with
                | None -> true
                | Some bo -> obj < bo
            
            if isBetter then
                let sol = fullSol.[0 .. cols - 1]
                let mutable firstFrac = -1
                for j in 0 .. cols - 1 do
                    if not (sol.[j].IsInteger) then
                            if firstFrac = -1 then firstFrac <- j
                
                if firstFrac = -1 then
                    bestObj <- Some obj
                    bestSol <- Some sol
                else
                    let valR = sol.[firstFrac]
                    let fl = bigRatFloor valR
                    let cl = bigRatCeil valR
                    
                    queue.Enqueue((firstFrac, true, fl) :: constraints)
                    queue.Enqueue((firstFrac, false, cl) :: constraints)
        | _ -> () 
            
    bestSol

let minimizeButtons (m:Machine) =
    match solveILP m with
    | Some w ->
        let rows = Array2D.length1 m.buttonWiringSchematics
        let cols = Array2D.length2 m.buttonWiringSchematics
        let check = Array.create rows BigRational.Zero
        for r in 0 .. rows - 1 do
            let mutable rowSum = BigRational.Zero
            for c in 0 .. cols - 1 do
                rowSum <- rowSum + m.buttonWiringSchematics.[r, c] * w.[c]
            check.[r] <- rowSum
        
        if check = m.targetJoltage then
            let sum = w |> Array.sumBy int
            printfn "Found solution: %A sum %A" w sum
            sum
        else
            printfn "Verification FAILED: Calculated %A, Expected %A" check m.targetJoltage
            0
    | None ->
        printfn "No solution found."
        0

let es = readMachines exampleFilename
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
        let rank = 
             try
                 let d = Array2D.map (fun (r:BigRational) -> double r) m.buttonWiringSchematics
                 let mat = MathNet.Numerics.LinearAlgebra.Double.DenseMatrix.OfArray d
                 mat.Rank()
             with _ -> -1
        printf "Processing machine %03d (Rank %d, Cols %d)..." i rank (Array2D.length2 m.buttonWiringSchematics)
        let res = minimizeButtons m
        res
    )
    |> Array.sum

printfn "Total Min Sum for Input: %A" totalInput
