
let PuzzleInput =
    {| Row = 2978; Column = 3083 |}


[<EntryPoint>]
let main _ =

    let codes =
        Seq.unfold (fun state ->
            let nextCode =
                (state * 2_525_33L) % 33_554_393L

            Some (nextCode, nextCode)) 20_151_125L

    let gridPositions =
        Seq.unfold (fun lastPosition ->
            let nextPos =
                match lastPosition with
                | (1, col) -> (col + 1, 1)
                | (row, col) -> (row - 1, col + 1)

            Some (nextPos, nextPos)) (1, 1)

    let grid =
        codes
        |> Seq.zip gridPositions

    grid
    |> Seq.find (fun ((row, col), _) ->
        row = PuzzleInput.Row && col = PuzzleInput.Column)
    |> snd
    |> printfn "Part 1 answer = %i\n"

    0