// Learn more about F# at http://fsharp.org

open System

[<Literal>]
let PuzzleInput = "1113222113"

[<EntryPoint>]
let main argv =
    let rec process_input (buffer: char list) (accrued: string list) (remaining: char list) =
        let flush_buffer =
            lazy (
                if buffer.Length = 0 then 
                    ""
                else
                    (List.head buffer).ToString() + buffer.Length.ToString()
            )
            
        match remaining with
        | [] ->
            (flush_buffer.Value :: accrued)
            |> String.Concat
            |> List.ofSeq
            |> List.rev

        | x::xs ->
            if buffer.Length = 0 then
                process_input [x] accrued xs

            else
                if x = buffer.[0] then
                    process_input (x :: buffer) accrued xs

                else
                    process_input [x] (flush_buffer.Value :: accrued) xs
                
    List.fold (fun x y -> process_input [] [] x) (List.ofSeq PuzzleInput) [1..40]
    |> String.Concat
    |> String.length
    |> printfn "Part 1 answer = %i"

    List.fold (fun x y -> process_input [] [] x) (List.ofSeq PuzzleInput) [1..50]
    |> String.Concat
    |> String.length
    |> printfn "Part 2 answer = %i"
            
        
    0 // return an integer exit code
