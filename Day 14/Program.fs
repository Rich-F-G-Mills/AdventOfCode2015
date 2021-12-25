
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx
open FSharpx.Text


module Array =
    let zipN (seqs: seq<_> []) =
        seq {
            let iters =
                seqs 
                |> Seq.map (fun s -> s.GetEnumerator ())
                |> Array.ofSeq

            // Assumes infinite sequences!
            while true do
                yield Array.init iters.Length (fun idx -> iters.[idx].Current)
        }


let [<Literal>] Pattern =
    @"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.$"

type Reindeer =
    { Name: string
      Speed: int
      FlyDuration: int
      RestDuration: int }

let rec projectReindeer reindeer =
    seq {
        yield! Seq.replicate reindeer.FlyDuration reindeer.Speed
        yield! Seq.replicate reindeer.RestDuration 0
        yield! projectReindeer reindeer
    }
        

[<EntryPoint>]
let main _ =
    
    let reindeers =
        File.ReadAllLines "Inputs.txt"
        |> Array.map (function
            | Regex.Match RegexOptions.None Pattern
                { GroupValues = [ name; speed; flyDurn; restDurn ] } ->
                    { Name = name;
                      Speed = int speed;
                      FlyDuration = int flyDurn;
                      RestDuration = int restDurn }
            | _ -> failwith "Unable to parse.")
        |> Array.map projectReindeer

    let distances =
        reindeers
        |> Array.mapi (fun rIdx vs ->
            vs
            |> Seq.scan (+) 0
            |> Seq.tail
            |> Seq.take 2503
            |> Seq.indexed
            |> Seq.map (fun (tIdx, d) -> (rIdx, tIdx, d))
            |> Seq.toArray)
        |> Array.concat

    distances
    |> Array.map (fun (_, _, d) -> d)
    |> Array.max
    |> printfn "Part 1 answer = %i\n"

    let maxDistByTime =
        distances
        |> Array.groupBy (fun (_, tIdx, _) -> tIdx)
        |> Array.map (fun (tIdx, ds) ->
            (tIdx, ds |> Array.map (fun (_, _, d) -> d) |> Array.max))
        |> Array.sortBy fst
        |> Array.map snd

    distances
    |> Array.groupBy (fun (_, tIdx, _) -> tIdx)
    |> Array.map (fun (tIdx, ds) ->
        (tIdx, ds |> Array.map (fun (rIdx, _, d) -> (rIdx, d))))
    |> Array.sortBy fst
    |> Array.map snd
    |> Array.zip maxDistByTime
    |> Array.map (fun (maxD, ds) ->
        (maxD, ds |> Array.map (fun (rIdx, d) ->
            (rIdx, if d = maxD then 1 else 0))))
    |> Array.collect snd
    |> Array.groupBy fst
    |> Array.map (snd >> Array.sumBy snd)
    |> Array.max
    |> printfn "Part 2 answer = %i"

    0