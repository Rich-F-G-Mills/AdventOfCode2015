
open System.IO


let charIsVowel =
    let vowels = [|'a'; 'e'; 'i'; 'o'; 'u'|]

    fun chr -> Array.contains chr vowels

let hasThreeVowels =
    Seq.filter charIsVowel
    >> Seq.length
    >> (<=) 3

let hasDoubles =
    Seq.pairwise
    >> Seq.exists (fun x -> (fst x) = (snd x))

let hasBadStrings =
    let badStrings = [|"ab"; "cd"; "pq"; "xy"|]

    fun (str: string) -> badStrings |> Seq.exists (str.Contains)

let hasTwoPairs =
    let rec findPairs =
        function
        | [] | [_] | [_;_] -> false
        | p::np::ps -> (List.contains p ps) || (findPairs (np::ps))

    List.ofSeq
    >> List.pairwise
    >> findPairs

let hasRepeatedLetter =
    let rec findTripple =
        function
        | [l1; l2; l3]::ts -> (l1 = l3) || (findTripple ts)
        | _ -> false

    List.ofSeq
    >> List.windowed 3
    >> findTripple


[<EntryPoint>]
let main argv =
    let strings =
        File.ReadAllLines "Inputs.txt"    

    let countValidStrings requirements =
        let allRequirementsMet str =
            Seq.forall ((|>) str) requirements

        strings
        |> Seq.filter allRequirementsMet
        |> Seq.length

    countValidStrings [hasThreeVowels; hasDoubles; not << hasBadStrings]
    |> printfn "Part 1 answer = %A"

    countValidStrings [hasTwoPairs; hasRepeatedLetter]
    |> printfn "Part 2 answer = %A"

    0
