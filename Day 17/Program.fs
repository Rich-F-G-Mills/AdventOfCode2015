
open System.IO


[<EntryPoint>]
let main _ =

    let sizes =
        File.ReadAllLines "Inputs.txt"
        |> Array.map int
        |> Array.sort
        |> Array.toList

    let rec numberCombinations (allocated: int list) unallocated toFind (found: int list list) cont =
        if toFind = 0 then
            cont <| allocated :: found 
        elif toFind < 0 then
            cont found
        else
            match unallocated with
            | [] -> cont found
            | u::us ->
                numberCombinations (u::allocated) us (toFind - u) found (fun f ->
                    numberCombinations allocated us toFind f cont)
            
    let validCombinations =
        numberCombinations [] sizes 150 [] id

    validCombinations
    |> List.length
    |> printfn "Part 1 answer = %i\n"

    let minContainers =
        validCombinations
        |> List.map List.length
        |> List.min
    in
        validCombinations
        |> List.filter (fun cs -> cs.Length = minContainers)
        |> List.length
        |> printfn "Part 2 answer = %i"

    0