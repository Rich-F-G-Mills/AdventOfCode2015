// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions
    

[<EntryPoint>]
let main argv =
    let splitter =
        Regex @"^(\w+)\sto\s(\w+)\s=\s(\d+)$"

    let parse_row row =
        let matches = splitter.Match row
        List.tail [ for x in matches.Groups -> x.Value ]

    let distances =
        File.ReadAllLines "Inputs.txt"
        |> List.ofArray
        |> List.map parse_row
        |> List.map(fun x -> (x.[0], (x.[1], Int32.Parse x.[2])))
        |> List.map(fun (x, (y, z)) -> [(x, (y, z)); (y, (x, z))])
        |> List.concat
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, (List.map snd v))
        |> Map.ofList
        |> Map.map (fun _ y -> Map.ofList y)

    let places =
        distances
        |> Map.toList
        |> List.map fst

    let chains =
        let rec chain accrued remaining =
            [
                match remaining with
                | [] ->
                    let distance =
                        accrued
                        |> List.pairwise
                        |> List.map (fun (x, y) -> distances.[x].[y])
                        |> List.sum

                    yield distance

                | _ ->
                    for x in remaining do
                        yield! (chain (x::accrued) (remaining |> List.except [x]))
            ]

        chain [] places


    chains
    |> List.min
    |> printfn "Part 1 answer = %i"

    chains
    |> List.max
    |> printfn "Part 2 answer = %i"

    0 // return an integer exit code
