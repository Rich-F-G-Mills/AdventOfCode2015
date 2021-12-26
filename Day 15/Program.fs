
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


let [<Literal>] Pattern =
    @"^(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$"


[<EntryPoint>]
let main _ =

    let ingredients =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (function
            | Regex.Match RegexOptions.None Pattern
                { GroupValues = [ _; cap; d; f; t; cal ] } ->
                    [| int cap; int d; int f; int t; int cal |]
            | _ -> failwith "Unable to parse ingredient.")

    let measures =
        Array.init 100 id
        |> Array.allPairs (Array.init 100 id)
        |> Array.allPairs (Array.init 100 id)
        |> Array.choose (fun (m1, (m2, m3)) ->
            if m2 <= 100 - m1 && m3 <= 100 - m1 - m2 then
                Some [| m1; m2; m3; 100 - m1 - m2 - m3 |]
            else
                None)

    let totals =
        measures
        |> Array.map (fun ms ->
            Array.init 5 (fun idx ->
                ingredients
                |> Seq.zip ms
                |> Seq.sumBy (fun (m, i) -> m * i.[idx])
                |> function | t -> Math.Max(t, 0)))

    totals
    |> Seq.map (Seq.take 4 >> Seq.reduce (*))
    |> Seq.max
    |> printfn "Part 1 answer = %i\n"

    totals
    |> Seq.map (fun ts -> ts |> Seq.take 4 |> Seq.reduce (*), ts.[4])
    |> Seq.filter (fun (_, cal) -> cal = 500)
    |> Seq.map fst
    |> Seq.max
    |> printfn "Part 2 answer = %i"

    0