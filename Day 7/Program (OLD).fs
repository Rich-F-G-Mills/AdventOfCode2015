// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Collections
open System.Text.RegularExpressions

type Term =
    | Literal of Int16
    | Register of string

    member this.get_value (registers: Map<string, Int16>) =
        match this with
        | Literal x -> x
        | Register x -> registers.[x]

    static member parse_element (element: string) =
        // https://stackoverflow.com/questions/28691162/the-f-equivalent-of-cs-out
        let is_number, value = Int16.TryParse element

        if is_number then Literal value else Register element


type UnaryOperator =
    | NOT


type BinaryOperator =
    | AND | OR | LSHIFT | RSHIFT


type Instruction =
    | Assignment of Term
    | Unary of UnaryOperator * Term
    | Binary of BinaryOperator * Term * Term

    member this.evaluate (registers: Map<string, Int16>) =
        match this with
        | Assignment (term) -> term.get_value registers

        | Unary (op, term) ->
            match op with
            | NOT -> ~~~(term.get_value registers)

        | Binary (op, term1, term2) ->
            let t1, t2 = term1.get_value registers, term2.get_value registers

            match op with
            | AND -> t1 &&& t2
            | OR -> t1 ||| t2
            | LSHIFT -> t1 <<< Convert.ToInt32(t2)
            | RSHIFT -> t1 >>> Convert.ToInt32(t2)

    member this.get_precedents =
        match this with
        | Assignment (Register r) -> [r]
        | Unary (_, Register r) -> [r]
        | Binary (_, Register r1, Register r2) -> [r1; r2]
        | Binary (_, Register r1, _) -> [r1]
        | Binary (_, _, Register r2) -> [r2]
        | _ -> []        


type ParsedLine =
    { instruction: Instruction; target: string; precedents: string list }


let execute (registers: Map<string, Int16>) parsed_line =
    Map.add parsed_line.target (parsed_line.instruction.evaluate registers) registers
        
       
let parse_line (line: string) =
    let arrow_pos =
        line.IndexOf "->"

    let elements =
        line.Substring(0, arrow_pos).Trim().Split(' ')
        |> List.ofArray

    let instruction =
        match List.length elements with
        | 1 ->
            Assignment (Term.parse_element elements.[0])

        | 2 -> 
            if elements.[0] <> "NOT" then
                failwith "Unknown unary instruction."
            else
                Unary (NOT, Term.parse_element elements.[1])

        | 3 ->
            let op =
                match elements.[1] with
                | "AND" -> AND
                | "OR" -> OR
                | "LSHIFT" -> LSHIFT
                | "RSHIFT" -> RSHIFT
                | _ -> failwith "Unrecognised binary instruction."

            Binary (op, Term.parse_element elements.[0], Term.parse_element elements.[2])

        | count -> failwith $"Not expecting LHS with {count} elements."

    {
        instruction = instruction;
        target = line.Substring(arrow_pos + 2).Trim();
        precedents = instruction.get_precedents
    } : ParsedLine
  

let determine_ordering parsed_lines =
    let rec order ordered not_ordered =
        match not_ordered with
        | [] ->
            ordered
            |> List.map (fun x -> List.find (fun y -> x = y.target) parsed_lines)

        | _ ->
            let can_add =
                not_ordered
                |> List.filter (fun x -> x.precedents |> List.forall (fun y -> List.contains y ordered))
            
            order (can_add |> List.map (fun x -> x.target) |> List.append ordered) (not_ordered |> List.except can_add)

    order [] parsed_lines


[<EntryPoint>]
let main argv =
    let lines =
        File.ReadAllLines "Inputs.txt"
        |> List.ofArray
        |> List.map parse_line
        |> determine_ordering
        |> List.fold execute Map.empty<string, int16>
        |> Map.find "a"
        |> printfn "Part 1 answer = %A"

    0 // return an integer exit code
