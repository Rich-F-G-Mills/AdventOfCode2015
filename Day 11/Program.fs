// Learn more about F# at http://fsharp.org

open System

[<Literal>]
let Input = "vzbxkghb"

let incr (x: int array) =
    x.[0] <- x.[0] + 1

    for idx = 0 to 6 do
        if (x.[idx] = 26) then
            x.[idx] <- 0
            x.[idx+1] <- x.[idx+1] + 1

    x
        
    

[<EntryPoint>]
let main argv =
    let input =
        Input
        |> Seq.rev
        |> Array.ofSeq
        |> Array.map int32
        |> Array.map ((+) -(int32 'a'))

    input
    |> printfn "%A"

    input
    |> incr
    |> printfn "%A"
    0 // return an integer exit code
