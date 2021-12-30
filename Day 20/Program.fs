
open System

let [<Literal>] PuzzleInput = 33100000


[<EntryPoint>]
let main _ =

    let maxElfIdx = 1000000

    let presentsDelivered =
        Array.zeroCreate maxElfIdx       

    Seq.init maxElfIdx (fun elfIdx ->
        Seq.initInfinite (fun idx -> (elfIdx, (idx + 1) * (elfIdx + 1)))
        |> Seq.takeWhile (fun (_, houseNo) -> houseNo < maxElfIdx))
    |> Seq.concat
    |> Seq.iter (fun (elfIdx, houseNo) ->
        presentsDelivered.[houseNo] <- presentsDelivered.[houseNo] + elfIdx * 10)

    presentsDelivered
    |> Array.findIndex ((<) PuzzleInput)
    |> printfn "Part 1 answer = %i\n"

    // Reset our array of presents delivered by house.
    do Array.Fill(presentsDelivered, 0)

    Seq.init maxElfIdx (fun elfIdx ->
        Seq.initInfinite (fun idx -> (elfIdx, (idx + 1) * (elfIdx + 1)))
        |> Seq.take 50
        |> Seq.takeWhile (fun (_, houseNo) -> houseNo < maxElfIdx))
    |> Seq.concat
    |> Seq.iter (fun (elfIdx, houseNo) ->
        presentsDelivered.[houseNo] <- presentsDelivered.[houseNo] + elfIdx * 11)

    presentsDelivered
    |> Array.findIndex ((<) PuzzleInput)
    |> printfn "Part 2 answer = %i"

    0