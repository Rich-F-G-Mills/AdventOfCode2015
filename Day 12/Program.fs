
open System.IO
open System.Text.Json


let rec getElementTotals checkForRed (jsonElement: JsonElement) =
    match jsonElement.ValueKind with
    | JsonValueKind.Array ->
        jsonElement.EnumerateArray ()
        |> processJsonCollection checkForRed

    | JsonValueKind.Object ->
        // If we have an object... Then we need to make sure that none of the
        // property values are "red" (assuming the check is needed)
        let propValues =
            jsonElement.EnumerateObject ()
            |> Seq.map (fun jsonProp -> jsonProp.Value)
            |> Seq.toArray

        propValues
        |> Array.exists (fun pv ->
            checkForRed && pv.ValueKind = JsonValueKind.String && pv.ValueEquals("red"))
        |> function
            | true -> Some 0
            | false -> propValues |> processJsonCollection checkForRed

    | JsonValueKind.String ->
        Some 0

    | JsonValueKind.Number ->
        Some <| jsonElement.GetInt32 ()

    | elementType ->
        failwith $"Unidentified JSON element ({elementType.ToString()})."
    
and processJsonCollection checkForRed (elements: JsonElement seq) =
    let totals =
        elements
        |> Seq.map (getElementTotals checkForRed)
        |> Seq.toArray

    if totals |> Array.contains None then
        Some 0
    else
        Some (totals |> Array.choose id |> Array.sum)


[<EntryPoint>]
let main _ =
    
    let jsonElements =            
        File.ReadAllText "Inputs.txt"
        |> JsonDocument.Parse
        |> fun doc -> doc.RootElement

    jsonElements
    |> getElementTotals false
    |> Option.get
    |> printfn "Part 1 answer = %i\n"
    
    jsonElements
    |> getElementTotals true
    |> Option.get
    |> printfn "Part 2 answer = %i"   

    0