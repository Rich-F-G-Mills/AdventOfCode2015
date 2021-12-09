// Learn more about F# at http://fsharp.org

open System
open System.IO
open FSharp.Reflection
open FParsec
open System.Diagnostics

type Target =
    | Register of string
    | Literal of int16
    member this.getValue (results: Map<string, int16>) =
        match this with
        | Register r -> results.[r]
        | Literal l -> l

let UnaryOps =
    Map.empty<string, int16 -> int16>
    |> Map.add "NOT" (fun x -> ~~~x)

let BinaryOps =
    Map.empty<string, int16 -> int16 -> int16>
    |> Map.add "AND" (fun x y -> x &&& y)
    |> Map.add "OR" (fun x y -> x ||| y)
    |> Map.add "LSHIFT" (fun x y -> x <<< (int32 y))
    |> Map.add "RSHIFT" (fun x y -> x >>> (int32 y))

type Expr =
    | Binary of string * Target * Target
    | Unary of string * Target
    | Target of Target

    member this.getDependencies =
        (match this with
        | Binary (_, t1, t2) -> [t1; t2]
        | Unary (_, t1) -> [t1]
        | Target (t1) -> [t1])
        |> List.choose (function | Register r -> Some r | _ -> None)


let association =
    spaces
    >>. pstring "->"
    >>. spaces
    >>. manyChars asciiLower

let target =
    (numberLiteral NumberLiteralOptions.DefaultInteger "number"
        |>> fun x -> Literal (int16 x.String))
    <|> (manyChars asciiLower |>> Register)

let unary_op_parser =
    UnaryOps
    |> Map.toList
    |> List.map fst
    |> List.map pstring
    |> choice

let binary_op_parser =
    BinaryOps
    |> Map.toList
    |> List.map fst
    |> List.map pstring
    |> choice

let assign_expr =
    target
    |>> Target 

let unary_expr =
    unary_op_parser
    .>> spaces
    .>>. target
    |>> Unary

let binary_expr =
    target
    .>> spaces
    .>>. binary_op_parser
    .>> spaces
    .>>. target
    |>> fun ((x,y),z) -> Binary (y, x, z) 

let expr =    
    (attempt unary_expr)
    <|> (attempt binary_expr)
    <|> assign_expr
    .>>. association

[<EntryPoint>]
let main argv =
    let parsed =
        File.ReadAllLines("Inputs.txt")
        |> List.ofArray
        |> List.map (run expr)
        |> List.map (
            function
            | Success (r, _, _) -> r
            | _ -> failwith "Failed parse encountered.")

    let dependencies =
        parsed
        |> List.map (fun x -> (snd x, (fst x).getDependencies))

    let rec order ordered remaining =
        let can_add x =
            snd x
            |> List.forall (fun x -> List.contains x ordered)

        let next_add =
            lazy(
                remaining
                |> List.find can_add
            )

        match remaining with
        | []  -> List.rev ordered
        | _ -> order 
                ((fst next_add.Value) :: ordered)
                (List.except [next_add.Value] remaining)

    let ordered =
        order [] dependencies
        |> List.map (fun x -> parsed |> List.filter (fun y -> x = snd y) |> List.exactlyOne)

    let eval results expr =
        let result =
            match (fst expr) with
            | Target t -> t.getValue results
            | Unary (f, t) -> UnaryOps.[f] (t.getValue results)
            | Binary (f, t1, t2) -> BinaryOps.[f] (t1.getValue results) (t2.getValue results)

        results
        |> Map.add (snd expr) result

    let results =
        ordered
        |> List.fold eval Map.empty

    printfn "%A" results.["a"]

    0 // return an integer exit code
