
open System
open System.IO
open FSharpx
open FSharpx.Text
open System.Text.RegularExpressions
open System.Buffers


type Mapping =
    { From: string
      To: string
      ToCount: int }


let private extractMapping =
    function
    | Regex.Match RegexOptions.None @"^(\w+)\s=>\s(\w+)$"
        { GroupValues = [ lhs; rhs ] } ->
            let rhsMolecules =
                Regex.Matches (rhs, "[A-Z][a-z]?")

            Some { From = lhs; To = rhs; ToCount = rhsMolecules.Count }
    | _ ->
        None


let private replaceSubStr length (newSubStr: string) (source: string) start =
    let newStrLength =
        source.Length + newSubStr.Length - length

    let state =
        struct {| Start = start; Length = length; NewSubStr = newSubStr; Source = source |}

    String.Create (newStrLength, state, SpanAction(fun output state ->
        let sourceSpan =
            state.Source.AsSpan()

        sourceSpan.Slice(0, state.Start).CopyTo(output)        
        state.NewSubStr.AsSpan().CopyTo(output.Slice(state.Start))
        sourceSpan.Slice(state.Start + state.Length).CopyTo(output.Slice(state.Start + state.NewSubStr.Length))
    ))

let generateCombinationsForMapping source before after =
    let rec findNext startIdx =
        seq {
            let foundIdx =
                source |> String.indexOfString' before startIdx

            if foundIdx >= 0 then
                yield foundIdx
                yield! findNext (foundIdx + 1)
        }

    let foundIdxs = findNext 0

    foundIdxs
    |> Seq.map (replaceSubStr before.Length after source)


[<EntryPoint>]
let main _ =
    let target =
        File.ReadAllText("Target.txt")

    let mappings =
        File.ReadAllLines("Mappings.txt")
        |> Array.choose extractMapping
        |> Array.sortByDescending (fun { ToCount = count } -> count)        

    mappings
    |> Seq.collect (fun  { From = from; To = to' } ->
        generateCombinationsForMapping target from to')
    |> Seq.distinct
    |> Seq.length
    |> printfn "Part 1 answer = %i"           

    // For part 2, we keep applying the mapping which results in the greatest reduction in molecules.
    let (result, mappingsApplied) =        
        let patterns =
            mappings
            |> Array.map (fun { To = to'} -> (to', Regex to'))
            |> Map.ofArray
    
        let rec inner (state: string) applied =
            mappings
            |> Array.tryFind (fun { To = to' } -> state.Contains to')
            |> function
                | Some ({ From = from; To = to' } as mapping) ->
                    inner <| patterns.[to'].Replace(state, from, 1) <| (mapping :: applied)
                | None ->
                    (state, applied)

        inner target []
        
    printfn "Part 2 answer = %i (With resulting medicine of '%s')" (mappingsApplied.Length) result

    0
