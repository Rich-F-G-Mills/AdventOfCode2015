
open System
open System.IO
open System.Text.RegularExpressions
open FSharpx.Text


type Item =
    { Cost: int
      Damage: int
      Armor: int }

type Inventory =
    { Weapons: Item list
      Armor: Item list
      Rings: Item list }

type ParticipantStats =
    { HitPoints: int
      Damage: int
      Armor: int }

type GameOutcome =
    | PlayerWins
    | BossWins

type ParsingMode =
    | Neither
    | WeaponSection
    | ArmorSection
    | RingSection


let (|ReMatch|_|) =
    Regex.(|Match|_|) RegexOptions.None

let (|Mode|_|) =
    function
    | "Weapons" -> Some WeaponSection
    | "Armor" -> Some ArmorSection
    | "Rings" -> Some RingSection
    | _ -> None

let (|Item|_|) =
    function
    | ReMatch @"^([a-zA-Z0-9 +]+?)\s+(\d+)\s+(\d+)\s+(\d+)$"
        { GroupValues = [ _; cost; damage; armor ] } ->

        Some { Cost = int cost; Damage = int damage; Armor = int armor }

    | _ -> None


let parseStockFile lines =
    let rec parseLine mode lines inventory =
        match (mode, lines) with
        | _, (ReMatch @"^(\w+):" { GroupValues = [ Mode newMode ]}) :: ls ->
            parseLine newMode ls inventory

        | Neither, line::_ ->
            failwith $"Cannot parse '{line}' when no mode is selected."

        | WeaponSection, (Item newItem) :: ls ->                   
            parseLine mode ls { inventory with Weapons = newItem :: inventory.Weapons }

        | ArmorSection, (Item newItem) :: ls ->                   
            parseLine mode ls { inventory with Armor = newItem :: inventory.Armor }

        | RingSection, (Item newItem) :: ls ->                   
            parseLine mode ls { inventory with Rings = newItem :: inventory.Rings }

        | _, [] -> inventory

        | _, line::_ ->
            failwith $"Unable to parse '{line}'."

    parseLine Neither (lines |> Seq.toList) { Weapons = []; Armor = []; Rings = [] }


let simulateGame playerStats bossStats =
    let rec playerTurn playerHP bossHP =
        let newBossHP =
            bossHP - Math.Max(playerStats.Damage - bossStats.Armor, 1)

        if newBossHP <= 0 then
            PlayerWins
        else
            bossTurn playerHP newBossHP          

    and bossTurn playerHP bossHP =
        let newPlayerHP =
            playerHP - Math.Max(bossStats.Damage - playerStats.Armor, 1)

        if newPlayerHP <= 0 then
            BossWins
        else
            playerTurn newPlayerHP bossHP

    playerTurn playerStats.HitPoints bossStats.HitPoints


[<EntryPoint>]
let main _ =
    // We could have just hardcoded the store inventory.. But that'd be too easy, right?
    let inventory =
        File.ReadAllLines "Stock.txt"
        |> Array.filter (not << String.IsNullOrEmpty)
        |> parseStockFile

    let purchaseCombinations =
        let optionalArmor =
            inventory.Armor
            |> List.map List.singleton
            |> List.append [[]]

        let optionalRing =
            inventory.Rings
            |> List.map List.singleton
            |> List.append [[]]

        let optionalRings =
            optionalRing
            |> List.allPairs optionalRing
            |> List.map ((<||) List.append)
            |> List.filter (function
                // We don't care about the ordering.
                | [r1; r2] when r1.Cost < r2.Cost -> true
                | [_] -> true
                | [] -> true
                | _ -> false)
            |> List.distinct

        optionalRings
        |> List.allPairs optionalArmor
        |> List.map ((<||) List.append)
        |> List.allPairs inventory.Weapons
        |> List.map List.Cons
        |> List.map (List.reduce (fun lhs rhs ->
            { Cost = lhs.Cost + rhs.Cost; Armor = lhs.Armor + rhs.Armor; Damage = lhs.Damage + rhs.Damage }))

    let bossStats =
        { HitPoints = 104; Damage = 8; Armor = 1 }

    let gameOutcomes =
        purchaseCombinations
        |> List.map (fun items ->
            let playerStats =
                { HitPoints = 100; Damage = items.Damage; Armor = items.Armor }

            simulateGame playerStats bossStats)

    gameOutcomes
    |> List.zip purchaseCombinations
    |> List.filter (function | _, PlayerWins -> true | _ -> false)
    |> List.map (fun (items, _) -> items.Cost)
    |> List.min
    |> printfn "Part 1 answer = %i\n"

    gameOutcomes
    |> List.zip purchaseCombinations
    |> List.filter (function | _, BossWins -> true | _ -> false)
    |> List.map (fun (items, _) -> items.Cost)
    |> List.max
    |> printfn "Part 2 answer = %i"

    0