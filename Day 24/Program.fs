// Learn more about F# at http://fsharp.org

open System
open System.IO

let rec find target allocated remaining =
    seq {
        match remaining with
        | [] -> yield! Seq.empty
        | r::rs ->
            if r = target then
                yield r::allocated

            elif r > target then
                yield! Seq.empty

            else // r < target 
                yield! (find (target - r) (r::allocated) rs)
                yield! (find target allocated rs)
    }
        

[<EntryPoint>]
let main argv =
    let inputs =
        File.ReadAllLines "Inputs.txt"
        |> List.ofArray
        |> List.map int32
        |> List.sort

    let input_head, input_tail =
        List.head inputs, List.tail inputs

    let total_weight =
        inputs
        |> List.sum

    printfn "Total package weight = %i" total_weight

    assert (inputs |> List.distinct |> List.length = (inputs |> List.length))

    let combinations_part1 =
        find (total_weight / 3) [] inputs
        |> List.ofSeq

    combinations_part1
    |> List.length
    |> printfn "List length = %i"

    let min_length_part1 =
        combinations_part1
        |> List.map List.length
        |> List.min

    printfn "Min number of packages = %A" min_length_part1

    combinations_part1
    |> List.filter (fun x -> min_length_part1 = List.length x)
    |> List.map ((List.map int64) >> (List.reduce (*)))
    |> List.min
    |> printfn "Part 1 answer = %A\n\n"


    let combinations_part2 =
        find (total_weight / 4) [] inputs
        |> List.ofSeq

    combinations_part2
    |> List.length
    |> printfn "List length = %i"

    let min_length_part2 =
        combinations_part2
        |> List.map List.length
        |> List.min

    printfn "Min number of packages = %A" min_length_part2

    combinations_part2
    |> List.filter (fun x -> min_length_part2 = List.length x)
    |> List.map ((List.map int64) >> (List.reduce (*)))
    |> List.min
    |> printfn "Part 2 answer = %A"



    0 // return an integer exit code
