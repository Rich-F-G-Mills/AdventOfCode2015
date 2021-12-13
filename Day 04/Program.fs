
open System
open System.Text
open System.Security.Cryptography


type MD5Hasher (prefix: string) =
    let md5 = MD5.Create()
    let inputBuilder = StringBuilder()
    let inputCharArray = Array.zeroCreate<Char> 100
    let inputByteArray = Array.zeroCreate<Byte> 100
    
    member this.checkHash (checker: byte array -> bool) (idx: int) =
        inputBuilder.Clear() |> ignore
        inputBuilder.Append(prefix) |> ignore
        inputBuilder.Append(idx.ToString()) |> ignore
        inputBuilder.CopyTo(0, inputCharArray, 0, inputBuilder.Length)

        let inputByteLen =
            Encoding.ASCII.GetBytes(inputCharArray, 0, inputBuilder.Length, inputByteArray, 0)

        md5.TransformFinalBlock(inputByteArray, 0, inputByteLen) |> ignore

        md5.Hash |> checker


[<EntryPoint>]
let main _ =
    let hasher = MD5Hasher "iwrupvqb"

    let isValidPart1Hash =
        hasher.checkHash (fun hash ->
            (hash.[0] = 0uy) && (hash.[1] = 0uy) && (hash.[2] &&& 0xF0uy = 0uy))

    let isValidPart2Hash =
        hasher.checkHash (fun hash ->
            (hash.[0] = 0uy) && (hash.[1] = 0uy) && (hash.[2] = 0uy))


    Seq.initInfinite id
    |> Seq.find isValidPart1Hash
    |> printfn "Part 1 answer = %i"
    
    Seq.initInfinite id
    |> Seq.find isValidPart2Hash
    |> printfn "Part 2 answer = %i"

    0