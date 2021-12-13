
open System.IO

type Movement =
    | Up
    | Down
    | Left
    | Right

    static member fromChar = function
        | '^'  -> Up
        | 'v'  -> Down
        | '<'  -> Left
        | '>'  -> Right
        | _    -> failwith "Unexpected character!"

    static member toDelta = function
        | Up    -> (0, 1)
        | Down  -> (0, -1)
        | Left  -> (-1, 0)
        | Right -> (0, 1)


type Location =
    | Location of X: int * Y: int

    static member origin = Location (0, 0)


let inline (+) (Location (x, y)) (movement: Movement) =
    let (dx, dy) = movement |> Movement.toDelta
    
    Location (x + dx, y + dy)


let countAtLeast1Visit locations =
    locations
    |> List.countBy id
    |> List.map snd
    |> List.filter ((<) 0)
    |> List.length

let getHousesVisited =
    List.scan (+) Location.origin
    

[<EntryPoint>]
let main _ =
    let input =
        File.ReadAllText "Inputs.txt"
        |> List.ofSeq
        |> List.map Movement.fromChar

    input
    |> getHousesVisited
    |> countAtLeast1Visit
    |> printfn "Part 1 answer = %A"

    let santaMoves, roboSantaMoves =
        input
        |> List.indexed
        |> List.partition (fun x -> (fst x) % 2 = 0)

    let santaHouses =
        santaMoves |> List.map snd |> getHousesVisited

    let roboSantaHouses =
        roboSantaMoves |> List.map snd |> getHousesVisited

    santaHouses
    |> List.append roboSantaHouses
    |> countAtLeast1Visit
    |> printfn "Part 2 answer = %A"

    0
