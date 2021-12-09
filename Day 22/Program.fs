
open System

let BossStats =
    {| HitPoints = 13; Damage = 8 |}


module Spell =

    type ImpactMetrics =
        { Mana: int
          ManaCost: int
          HitPoints: int
          Armour: int
          BossHitPoints: int }

    type Type =
        | MagicMissile
        | Drain
        | Shield
        | Poison
        | Recharge

    type Impact =
        | Apply of ImpactMetrics
        | ApplyAndCease of ImpactMetrics

    type Applicator =
        Applicator of (int -> Impact)


    let (nullSpell: ImpactMetrics) =
        { Mana = 0; ManaCost = 0; HitPoints = 0; Armour = 0; BossHitPoints = 0 }


    let instantSpellFactory (name: string) impact =
        Applicator (fun time ->
            if time = 0 then
                ApplyAndCease impact
            else
                failwith $"Cannot cast ${name} for longer than a single turn.")

    let continuousSpellFactory (name: string) (duration: int) initialImpact residualImpact =
        Applicator (fun time ->
            if time = 0 then
                Apply initialImpact

            elif time < duration then
                Apply residualImpact

            elif time = duration then
                ApplyAndCease residualImpact

            else
                failwith $"Cannot cast {name} for longer than {duration} turns.")
            

module MagicMissile =

    let private effect =
        { Spell.nullSpell with
            Mana = -53
            ManaCost = 53
            BossHitPoints = -4 }

    let apply =
        Spell.instantSpellFactory "Magic Missile" effect


module Drain =

    let private effect =
        { Spell.nullSpell with
            Mana = -73
            ManaCost = 73
            HitPoints = 2
            BossHitPoints = -2 }

    let apply =
        Spell.instantSpellFactory "Drain" effect


module Shield =
    
    let private effectNonInitial =
        { Spell.nullSpell with
            Armour = 7 }

    let private effectInitial =
        { effectNonInitial with
            Mana = -113
            ManaCost = 113 }

    let apply =
        Spell.continuousSpellFactory "Shield" 6 effectInitial effectNonInitial


module Poison =
   
   let private effectNonInitial =
       { Spell.nullSpell with
           BossHitPoints = -3 }

   let private effectInitial =
       { effectNonInitial with
           Mana = -173
           ManaCost = 173 }

   let apply =
       Spell.continuousSpellFactory "Poison" 6 effectInitial effectNonInitial


module Recharge =
   
   let private effectNonInitial =
       { Spell.nullSpell with
           Mana = 101 }

   let private effectInitial =
       { Spell.nullSpell with
           Mana = -229
           ManaCost = 229 }

   let apply =
       Spell.continuousSpellFactory "Recharge" 6 effectInitial effectNonInitial
            

type PlayedBy =
    | Player
    | Boss

and ActiveGameState =
    { LastPlayed: PlayedBy
      TurnIdx: int
      Mana: int
      ManaSpent: int
      HitPoints: int      
      Armour: int
      ActiveSpells: ActiveSpell list
      BossHitPoints: int }

and GameState =
    | ActiveGame of ActiveGameState
    | PlayerWonGame
    | BossWonGame

and ActiveSpell =
    { Type: Spell.Type
      CastAtTurnIdx: int }


let spellApplicatorForType =
    function
    | Spell.MagicMissile -> MagicMissile.apply
    | Spell.Drain -> Drain.apply
    | Spell.Shield -> Shield.apply
    | Spell.Poison -> Poison.apply
    | Spell.Recharge -> Recharge.apply
    

let updateGameStateForSpell (state: ActiveGameState) (spell: ActiveSpell) =
    let { Type = spellType; CastAtTurnIdx = castAtIdx } = spell
    
    let (Spell.Applicator applicator) =
        spellType |> spellApplicatorForType
    
    let spellImpact = applicator (state.TurnIdx - castAtIdx)
        
    let (spellImpactMetrics, newActiveSpells) =
        match spellImpact with
        | Spell.Apply impact ->
            impact, state.ActiveSpells
    
        | Spell.ApplyAndCease impact ->
            let newActiveSpells =
                state.ActiveSpells
                |> List.filter (fun { Type = spellType' } -> spellType' <> spellType)
    
            impact, newActiveSpells
    
    { state with
        Mana = state.Mana + spellImpactMetrics.Mana
        ManaSpent = state.ManaSpent + spellImpactMetrics.ManaCost
        HitPoints = state.HitPoints + spellImpactMetrics.HitPoints
        Armour = state.Armour + spellImpactMetrics.Armour
        BossHitPoints = state.BossHitPoints + spellImpactMetrics.BossHitPoints
        ActiveSpells = newActiveSpells }


let updateGameStateForPlayer (newSpellType: Spell.Type) (state: GameState) =
    match state with
    | ActiveGame state ->
        // Reset the armour adjustment, increment the turn index and update current player
        let state =
            { state with
                LastPlayed =
                    match state.LastPlayed with
                    | Player -> failwith "Player cannot have consecutive turns."
                    | Boss -> Player
                TurnIdx = state.TurnIdx + 1
                Armour = 0 }
            
        // Cast any already active player spells.
        let state =
            state.ActiveSpells
            |> List.fold updateGameStateForSpell state

        // Create a newly activated spell.
        let newActiveSpell =
            let spellAlreadyExists =
                state.ActiveSpells
                |> List.exists (fun { Type = spellType } ->
                    spellType = newSpellType)

            if spellAlreadyExists then
                failwith $"Cannot apply {newSpellType} as it is already in effect."

            else
                { Type = newSpellType; CastAtTurnIdx = state.TurnIdx }

        // Update the list of active spells.
        let state =
            { state with
                ActiveSpells = newActiveSpell :: state.ActiveSpells }

        // Update for the impact of the newly activated spell and return.
        let state =
            updateGameStateForSpell state newActiveSpell

        // Check to see if the game should still be active.
        if state.Mana < 0 then
            BossWonGame
        elif state.BossHitPoints <= 0 then
            PlayerWonGame
        else
            ActiveGame state

    | _ -> failwith "Cannot update a game that has already finished."


let updateGameStateForBoss (state: GameState) =
    match state with
    | ActiveGame state ->
        // Reset the armour adjustment, increment the turn index and update current player
        let state =
            { state with
                LastPlayed =
                    match state.LastPlayed with
                    | Boss -> failwith "Boss cannot have consecutive turns."
                    | Player -> Boss
                TurnIdx = state.TurnIdx + 1
                Armour = 0 }
            
        // Cast any already active player spells.
        let state =
            state.ActiveSpells
            |> List.fold updateGameStateForSpell state

        // Update for the boss' strike, assuming the boss is still alive after applying active spells.
        if state.BossHitPoints > 0 then
            if BossStats.Damage >= state.HitPoints then
                BossWonGame
            else
                let state =
                    // Don't forget that boss attacks always deal at least 1 damage.
                    { state with
                        HitPoints = state.HitPoints - Math.Max (BossStats.Damage - state.Armour, 1) }

                ActiveGame state
        else
            PlayerWonGame

    | _ -> failwith "Cannot update a game that has already finished."


[<EntryPoint>]
let main argv =
    let startingState =
        { LastPlayed = Boss
          TurnIdx = -1
          Mana = 250
          ManaSpent = 0
          HitPoints = 10
          Armour = 0
          BossHitPoints = BossStats.HitPoints
          ActiveSpells = [] } |> ActiveGame

    let gameActions = [
        updateGameStateForPlayer Spell.Poison
        updateGameStateForBoss
        updateGameStateForPlayer Spell.MagicMissile
    ]

    List.scan (>>) id gameActions
    |> Seq.map ((|>) startingState)
    |> Seq.skip 1
    |> Seq.iter (printfn "%A\n\n")

    0