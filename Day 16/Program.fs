
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


[<EntryPoint>]
let main _ =
    
    let aunts =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (function
            | Regex.Match RegexOptions.None "^Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)$"
                { GroupValues = [ idx; p1; v1; p2; v2; p3; v3 ] } ->
                    (int idx, [(p1, int v1); (p2, int v2); (p3, int v3)])
            | _ -> failwith "Unable to parse aunt.")
        |> Map.ofArray

    let targetAunt =
        [
            ("children", 3)
            ("cats", 7)
            ("samoyeds", 2)
            ("pomeranians", 3)
            ("akitas", 0)
            ("vizslas", 0)
            ("goldfish", 5)
            ("trees", 3)
            ("cars", 2)
            ("perfumes", 1)
        ]
        
    let targetAuntSet = targetAunt |> Set.ofList in
        aunts
        |> Map.filter (fun _ -> Set >> Set.isSuperset targetAuntSet)
        |> Map.toArray
        |> Array.exactlyOne
        |> fst
        |> printfn "Part 1 answer = %i\n"

    let propertyComparer: (string * int * int) -> bool =
        function
        | "cats", target, actual
        | "trees", target, actual -> actual > target
        | "pomeranians", target, actual
        | "goldfish", target, actual -> actual < target
        | _, target, actual -> target = actual

    let targetAuntMap = targetAunt |> Map.ofList in
        aunts
        |> Map.map (fun _ ->
            List.map (fun (propName, propActualVal) ->
                (propName, targetAuntMap.[propName], propActualVal))
            >> List.forall propertyComparer)
        |> Map.toArray
        |> Array.filter snd
        |> Array.exactlyOne
        |> fst
        |> printfn "Part 2 answer = %i"

    0