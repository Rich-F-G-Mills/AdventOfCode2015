
open System
open System.IO

// Uses continuations where a maximum length is passed in to ensure that only sequences of the
// same length or shorter are returned.
// This results in a substantial performance boost!
let rec genPackageArrangements target (allocated: int list) remaining cont maxLen =
    match remaining with
    | r::_ when r = target ->
        seq {        
            yield r::allocated

            yield! cont (allocated.Length + 1)
        }
    | r::rs when r < target && allocated.Length < maxLen ->
        genPackageArrangements (target - r) (r::allocated) rs (fun mL -> genPackageArrangements target allocated rs cont mL) maxLen
    | _ -> cont maxLen 


let findMinimalArrangement: int list seq -> int64 =
    Seq.map (fun items -> items.Length, items |> List.map int64 |> List.reduce (*))
    >> Seq.sort
    >> Seq.head
    >> snd
        

[<EntryPoint>]
let main _ =
    let inputs =
        File.ReadAllLines "Inputs.txt"
        |> List.ofArray
        |> List.map int32
        |> List.sort

    let totalWeight =
        inputs
        |> List.sum

    genPackageArrangements (totalWeight / 3) [] inputs (fun _ -> Seq.empty) Int32.MaxValue
    |> findMinimalArrangement
    |> printfn "Part 1 answer = %i\n"

    genPackageArrangements (totalWeight / 4) [] inputs (fun _ -> Seq.empty) Int32.MaxValue
    |> findMinimalArrangement
    |> printfn "Part 2 answer = %i"

    0
