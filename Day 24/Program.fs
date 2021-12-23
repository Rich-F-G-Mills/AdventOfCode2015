
open System
open System.IO

//// This could be adjusted so that it only ever yields ever smaller arrangements.
//// Could be done using continuations?
//let rec genPackageArrangements target allocated remaining =
//    seq {
//        match remaining with
//        | [] -> ()
//        | r::_ when r = target ->
//            yield r::allocated
//        | r::_ when r > target -> ()
//        | r::rs ->
//            // Generate all remaining combinations assuming r was selected.
//            yield! (genPackageArrangements (target - r) (r::allocated) rs)
//            // Generate all remaining combinations assuming r was NOT selected.
//            yield! (genPackageArrangements target allocated rs)
//    }

// This could be adjusted so that it only ever yields ever smaller arrangements.
// Could be done using continuations?
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
