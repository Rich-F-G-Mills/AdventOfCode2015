
open System.IO


let rec convertCodeToMemory =
    function
    | '\\'::'\\'::cs 
    | '\\'::'"'::cs
    | '\\'::'x'::_::_::cs -> 1 + convertCodeToMemory cs
    | '\\'::_ -> failwith "Unknown escape sequence."
    | [] | '"'::[] -> 0
    | '"'::cs -> convertCodeToMemory cs
    | _::cs -> 1 + convertCodeToMemory cs


let rec convertMemoryToCode =
    function
    | '"'::cs 
    | '\\'::cs -> 2 + convertMemoryToCode cs
    | _::cs -> 1 + convertMemoryToCode cs
    | [] -> 2  // Add 2 for the opening and closing " mark.


[<EntryPoint>]
let main _ =

    let inputs =
        File.ReadAllLines "Inputs.txt"

    inputs
    |> Array.map (List.ofSeq >> convertCodeToMemory)
    |> Array.zip inputs
    |> Array.sumBy (fun (i, memLen) -> i.Length - memLen)
    |> printfn "Part 1 answer = %i\n"

    inputs
    |> Array.map (List.ofSeq >> convertMemoryToCode)
    |> Array.zip inputs
    |> Array.sumBy (fun (i, codeLen) -> codeLen - i.Length)
    |> printfn "Part 2 answer = %i"

    0    