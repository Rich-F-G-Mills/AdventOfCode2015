
open System.IO

[<EntryPoint>]
let main _ =
    let mappedInput =
        File.ReadAllText "Inputs.txt"
        // Map each character recieved to the corresponding adjustment.
        |> Seq.map (
            function 
            | '(' -> 1
            | ')' -> -1
            | _ -> failwith "Unexpected character!")

    mappedInput
        |> Seq.sum
        |> printfn "Part 1 answer = %A"

    mappedInput
        |> Seq.scan (+) 0
        // Find point at which Santa enters the basement.
        |> Seq.takeWhile ((<) -1)
        |> Seq.length
        |> printfn "Part 2 answer = %A"

    0