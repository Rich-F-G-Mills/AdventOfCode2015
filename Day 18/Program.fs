
open System
open System.IO


let iterateGrid grid lockCorners =
    let dimSize =
        grid |> Array2D.length1

    assert (Array2D.length1 grid = Array2D.length2 grid)

    let prevGrid =
        Array2D.zeroCreate dimSize dimSize

    let surroundingOffsets =
        Array.allPairs [| -1; 0; 1 |] [| -1; 0; 1 |]
        |> Array.except [| (0, 0) |]

    let corners =
        Array.allPairs [| 0; dimSize-1 |] [| 0; dimSize-1 |]

    if lockCorners then
        corners
        |> Array.iter (fun (x, y) -> grid.[x, y] <- true)

    fun () ->
        Array2D.blit grid 0 0 prevGrid 0 0 dimSize dimSize

        for x = 0 to (dimSize-1) do
            for y = 0 to (dimSize-1) do
                let mutable numSurrounding = 0

                for (dx, dy) in surroundingOffsets do
                    if x + dx >= 0 && x + dx < dimSize && y + dy >= 0 && y + dy < dimSize then
                        numSurrounding <- numSurrounding + if prevGrid.[x + dx, y + dy] then 1 else 0

                grid.[x, y] <-
                    (prevGrid.[x, y] && (numSurrounding = 2 || numSurrounding = 3))
                        || ((not prevGrid.[x, y]) && numSurrounding = 3)

        if lockCorners then
            corners
            |> Array.iter (fun (x, y) -> grid.[x, y] <- true)


let countActiveCells (grid: bool[,]) =
    grid
    |> Array2D.map Convert.ToInt32
    |> Seq.cast<int>
    |> Seq.sum


[<EntryPoint>]
let main _ =

    let startingGrid =
        let grid' =
            File.ReadAllLines "Inputs.txt"
            |> Array.map Array.ofSeq
            |> Array.map (Array.map ((=) '#'))
        
        let dimSize =
            grid' |> Array.length

        Array2D.init dimSize dimSize (fun x y -> grid'.[y].[x])

    let grid =
        startingGrid |> Array2D.copy

    let part1GridIterator =
        iterateGrid grid false
        
    for _ = 1 to 100 do
        part1GridIterator ()

    grid
    |> countActiveCells
    |> printfn "Part 1 answer = %i\n"


    let grid =
        startingGrid |> Array2D.copy

    let part2GridIterator =
        iterateGrid grid true

    for _ = 1 to 100 do
        part2GridIterator ()

    grid
    |> countActiveCells
    |> printfn "Part 2 answer = %i"

    0