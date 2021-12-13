
open System.IO
open System.Text.RegularExpressions


let splitter =
    Regex @"^(turn on|turn off|toggle)\s(\d+),(\d+)\sthrough\s(\d+),(\d+)$"


type ChangeType =
    | Toggle
    | TurnOn
    | TurnOff

type Instruction =
    | Instruction of ChangeType * (int * int) * (int * int)

    static member ConstructFromString (line: string) =
        let elements =
            splitter.Match(line).Groups.Values
            |> Seq.skip 1
            |> Seq.map (fun x -> x.Value)
            |> List.ofSeq

        let changeType =
            match elements.[0] with
            | "toggle" -> Toggle
            | "turn on" -> TurnOn
            | "turn off" -> TurnOff
            | _ -> failwith "Unknown instruction!"

        Instruction (
            changeType,
            (int elements.[1], int elements.[2]),
            (int elements.[3], int elements.[4])
        )
        

let part1Action changeType value =
    match changeType with
    | Toggle    -> 1 - value
    | TurnOn    -> 1
    | TurnOff   -> 0


let part2Action change_type value =
    match change_type with
    | Toggle    -> value + 2
    | TurnOn    -> value + 1
    | TurnOff   -> if value > 0 then value - 1 else 0


[<EntryPoint>]
let main _ =  
    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> List.ofArray
        |> List.map Instruction.ConstructFromString

    let getTotalBrightness action =
        let performAction (matrix: int[,]) instruction =
            let (Instruction (change_type, (left, bottom), (right, top))) = instruction
                    
            for x = left to right do
                for y = bottom to top do
                    matrix.[x,y] <- action change_type matrix.[x,y]
                    
            matrix

        instructions
        |> List.fold performAction (Array2D.zeroCreate<int> 1000 1000)
        |> Seq.cast<int>
        |> Seq.sum      
    
    getTotalBrightness part1Action
    |> printfn "Part 1 answer = %A\n"

    getTotalBrightness part2Action
    |> printfn "Part 2 answer = %A\n"
    
    0