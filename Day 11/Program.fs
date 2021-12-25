
open System


let [<Literal>] PuzzleInput = "vzbxkghb"


// This is not an ideal functional approach as the incrementation process has side-effects.

let incrementPassword (pwd: byte array) =

    let newPwd = pwd |> Array.copy

    let rec inner idx carry =
        let newChar =
            pwd.[idx] + if idx = 7 then 1uy else 0uy + if carry then 1uy else 0uy

        newPwd.[idx] <- newChar % 26uy

        if idx > 0 then           
            inner (idx - 1) (newChar > 25uy)
        else
            newPwd

    inner 7 false

let hasStraight (pwd: byte array) =
    let rec inner idx =
        if pwd.[idx+1] = pwd.[idx] + 1uy && pwd.[idx+2] = pwd.[idx] + 2uy then
            true
        elif idx < 5 then
            inner (idx + 1)
        else
            false

    inner 0

let hasInvalidCharacters =
    let invalidChars =
        [| 'i'; 'o'; 'l' |]
        |> Array.map byte
        |> Array.map (fun c -> c - byte 'a')

    fun (pwd: byte array) ->
        invalidChars
        |> Array.exists (fun c -> Array.contains c pwd)

let hasNonOverlappingPairs (pwd: byte array) =
    let rec findSecondPair chr idx =
        if pwd.[idx] = pwd.[idx+1] && pwd.[idx] <> chr then
            true
        elif idx < 6 then
            findSecondPair chr (idx + 1)
        else
            false        

    let rec findFirstPair idx =
        if pwd.[idx] = pwd.[idx+1] then
            findSecondPair pwd.[idx] (idx + 2)
        elif idx < 4 then
            findFirstPair (idx + 1)
        else
            false

    findFirstPair 0

let isValidPassword pwd =
    hasStraight pwd && (not <| hasInvalidCharacters pwd) && hasNonOverlappingPairs pwd


// Using a recursive sequence such as this allows us to create a sequence
// where the next element depends on the previous.
let rec generatePasswords startingPwd =
    seq {
        let nextPwd =
            incrementPassword startingPwd

        yield nextPwd
        yield! generatePasswords nextPwd
    }
               

[<EntryPoint>]
let main _ =

    let password =
        PuzzleInput
        |> Array.ofSeq
        |> Array.map byte
        |> Array.map (fun c -> c - byte 'a')

    let firstValidPassword =
        password
        |> generatePasswords
        |> Seq.find isValidPassword

    firstValidPassword
    |> Array.map (fun c -> char (c + byte 'a'))
    |> String
    |> printfn "Part 1 answer = %s\n"

    firstValidPassword
    |> generatePasswords
    |> Seq.find isValidPassword
    |> Array.map (fun c -> char (c + byte 'a'))
    |> String
    |> printfn "Part 2 answer = %s"
    
    0