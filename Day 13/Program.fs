
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


let [<Literal>] RePattern =
    @"^(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\.$"


let processArrangement =
    function
    | Regex.Match RegexOptions.None RePattern
        { GroupValues = [guest1; dir; delta; guest2] }
            when dir = "lose" || dir = "gain" ->
                (guest1, guest2), (int delta) * if dir = "lose" then -1 else 1

    | arrangement ->
        failwith $"Unable to parse '{arrangement}'."
        

[<EntryPoint>]
let main _ =
    let arrangements =
        let parsedArrangements =
            File.ReadAllLines "Inputs.txt"
            |> Array.map processArrangement

        let guestNames =
            parsedArrangements
            |> Array.collect (fun ((g1, g2), _) -> [| g1; g2 |])
            |> Array.distinct
            |> Array.sort

        assert (guestNames.Length = 8)

        parsedArrangements
        |> Array.map (fun ((g1, g2), h) ->
            (guestNames |> Array.findIndex ((=) g1), guestNames |> Array.findIndex ((=) g2)), h)
        |> Map.ofArray    
    
    let permutations =
        let primes =
            [| 1; 2; 3; 5; 7; 11; 13; |]

        let validProduct =
            primes
            |> Array.reduce (*)

        let maxCount =
            Seq.replicate 7 7
            |> Seq.reduce (*)
        
        let isValidPermutation idx =
            let mutable product = 1
            let mutable toReduce = idx
            
            while toReduce > 0 do
                product <- product * primes.[toReduce % 7]
                toReduce <- toReduce / 7

            product = validProduct

        let toArrayOffsets idx =
            let offsets =
                Array.zeroCreate 9

            let mutable toReduce = idx

            offsets.[0] <- 7
            offsets.[8] <- 7
            
            for idx = 1 to 7 do
                offsets.[idx] <- toReduce % 7
                toReduce <- toReduce / 7

            offsets

        Seq.init (maxCount - 1) id
        |> Seq.filter isValidPermutation
        |> Seq.map toArrayOffsets
        |> Seq.toArray

    let maxHappinessChange =
        permutations
        |> Array.map (
            Array.pairwise
            >> Array.map (fun (g1, g2) -> arrangements.[g1, g2] + arrangements.[g2, g1]))
        |> Array.maxBy Array.sum

    maxHappinessChange
    |> Array.sum
    |> printfn "Part 1 answer = %i\n"

    // For part 2, we need only remove the smallest happiness delta between 2 guests from the above...
    maxHappinessChange
    |> Array.min
    |> (-) (maxHappinessChange |> Array.sum)
    |> printfn "Part 2 answer = %i"

    0