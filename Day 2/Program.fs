
open System
open System.IO

let areaCalc (w, l, h) =
    let faceAreas =
        [|l*w; w*h; h*l|]

    let area =
        faceAreas
        |> Array.map ((*) 2)
        |> Array.sum

    area + Array.min faceAreas

let bowCalc (w, l, h) =
    let facePerimiters =
        [|l+w; w+h; h+l|]
        |> Array.map ((*) 2)

    let length =
        facePerimiters
        |> Array.min

    length + w * l * h

let dimSplitter (dim: string) =
    let split =
        dim.Split('x', 3)
        |> Array.map Int32.Parse

    split.[0], split.[1], split.[2]


[<EntryPoint>]
let main argv =
    let dims =
        File.ReadAllLines "Inputs.txt"
        |> Array.map dimSplitter

    dims
        |> Array.map areaCalc
        |> Array.sum
        |> printfn "Part 1 answer = %A"

    dims
        |> Array.map bowCalc
        |> Array.sum
        |> printfn "Part 2 answer = %A"

    0