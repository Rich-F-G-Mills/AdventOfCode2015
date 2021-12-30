
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


type Register =
    | A
    | B

type Instruction =
    | Hlf of Register
    | Tpl of Register
    | Inc of Register
    | Jmp of Offset: int
    | Jie of Register * Offset: int
    | Jio of Register * Offset: int

type State =
    { Offset: int
      Registers: Map<Register, int> }


let (|ReMatch|_|) =
    Regex.(|Match|_|) RegexOptions.None

let (|Register|_|) =
    function
    | "a" -> Some A
    | "b" -> Some B
    | _ -> None

let (|Int|_|) (str: string) =
    match Int32.TryParse str with
    | true, v -> Some v
    | false, _ -> None


let parseInstruction =
    function
    | ReMatch @"^hlf (a|b)$" { GroupValues = [ Register reg ] } ->
        Hlf reg
    | ReMatch @"^tpl (a|b)$" { GroupValues = [ Register reg ] } ->
        Tpl reg
    | ReMatch @"^inc (a|b)$" { GroupValues = [ Register reg ] } ->
        Inc reg
    | ReMatch @"^jmp ([+-]?\d+)$" { GroupValues = [ Int v ] } ->
        Jmp v
    | ReMatch @"^jie (a|b), ([+-]?\d+)$" { GroupValues = [ Register reg; Int offset ] } ->
        Jie (reg, offset)
    | ReMatch @"^jio (a|b), ([+-]?\d+)$" { GroupValues = [ Register reg; Int offset ] } ->
        Jio (reg, offset)
    | instr -> failwith $"Unable to parse '{instr}'."


let processInstruction (instructions: Instruction []) state =
    if state.Offset >= 0 && state.Offset < instructions.Length then
        let newState =
            match instructions.[state.Offset] with
            | Hlf reg ->
                { state with
                    Offset = state.Offset + 1
                    Registers = state.Registers |> Map.add reg (state.Registers.[reg] / 2) }

            | Tpl reg ->
                { state with
                    Offset = state.Offset + 1
                    Registers = state.Registers |> Map.add reg (state.Registers.[reg] * 3) }

            | Inc reg ->
                { state with
                    Offset = state.Offset + 1
                    Registers = state.Registers |> Map.add reg (state.Registers.[reg] + 1) }

            | Jmp offset ->
                { state with Offset = state.Offset + offset }

            | Jie (reg, offset) ->
                if state.Registers.[reg] % 2 = 0 then
                    { state with Offset = state.Offset + offset }
                else
                    { state with Offset = state.Offset + 1 }

            | Jio (reg, offset) ->
                if state.Registers.[reg] = 1 then
                    { state with Offset = state.Offset + offset }
                else
                    { state with Offset = state.Offset + 1 }

        Some (newState, newState)
    else
        None


[<EntryPoint>]
let main _ =

    let instructions =
        File.ReadAllLines "Inputs.txt"
        |> Array.map parseInstruction
        
    
    let initialState =
        { Offset = 0; Registers = [(A, 0); (B, 0)] |> Map.ofList }
    in
        Seq.unfold (processInstruction instructions) initialState
        |> Seq.last
        |> fun { Registers = rs } -> rs.[B]
        |> printfn "Part 1 answer = %i\n"


    let initialState =
        { Offset = 0; Registers = [(A, 1); (B, 0)] |> Map.ofList }
    in
        Seq.unfold (processInstruction instructions) initialState
        |> Seq.last
        |> fun { Registers = rs } -> rs.[B]
        |> printfn "Part 1 answer = %i\n"

    0