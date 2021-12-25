
open System


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
       { Spell.nullSpell with
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
       Spell.continuousSpellFactory "Recharge" 5 effectInitial effectNonInitial
            

type PlayedBy =
    | Player
    | Boss

type ActiveSpell =
    { Type: Spell.Type
      CastAtTurnIdx: int }

type GameDifficulty =
    | Normal
    | Hard

type ActiveGameState =
    { LastPlayed: PlayedBy
      TurnIdx: int
      Mana: int
      ManaSpent: int
      HitPoints: int      
      Armour: int
      ActiveSpells: ActiveSpell list
      BossHitPoints: int
      BossDamage: int
      Difficulty: GameDifficulty }

type GameState =
    | ActiveGame of State: ActiveGameState
    | PlayerWonGame of ManaSpent: int
    | BossWonGame
    | InvalidGame of Reason: string


let spellApplicatorForType =
    function
    | Spell.MagicMissile -> MagicMissile.apply
    | Spell.Drain -> Drain.apply
    | Spell.Shield -> Shield.apply
    | Spell.Poison -> Poison.apply
    | Spell.Recharge -> Recharge.apply
    

let updateGameStateForSpell (state: ActiveGameState) = function
    | { Type = spellType; CastAtTurnIdx = castAtIdx } ->    
        let (Spell.Applicator applicator) =
            spellType |> spellApplicatorForType
    
        let (spellImpactMetrics, newActiveSpells) =
            match applicator (state.TurnIdx - castAtIdx) with
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


let updateGameStateForPlayer (newSpellType: Spell.Type) = function
    | ActiveGame state ->
        // Reset the armour adjustment, increment the turn index and update current player
        let state =
            { state with
                LastPlayed = Player
                TurnIdx = state.TurnIdx + 1
                HitPoints =
                    match state.Difficulty with                    
                    | Hard -> state.HitPoints - 1
                    | Normal -> state.HitPoints
                Armour = 0 }

        if state.HitPoints <= 0 then
            BossWonGame

        else            
            // Cast any already active player spells.
            let state =
                state.ActiveSpells
                |> List.fold updateGameStateForSpell state

            // Is the requested spell already active?
            let spellAlreadyExists =
                state.ActiveSpells
                |> List.exists (fun { Type = spellType } ->
                    spellType = newSpellType)

            if spellAlreadyExists then
                InvalidGame $"Cannot apply {newSpellType} as it is already in effect."

            else
                // Create a newly activated spell.
                let newActiveSpell =
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
                    PlayerWonGame state.ManaSpent
                else
                    ActiveGame state

    | _ -> failwith "Cannot update a game that has already finished."


let updateGameStateForBoss = function
    | ActiveGame state ->
        // Reset the armour adjustment, increment the turn index and update current player
        let state =
            { state with
                LastPlayed = Boss
                TurnIdx = state.TurnIdx + 1
                Armour = 0 }
            
        // Cast any already active player spells.
        let state =
            state.ActiveSpells
            |> List.fold updateGameStateForSpell state

        // Update for the boss' strike, assuming the boss is still alive after applying active spells.
        if state.BossHitPoints > 0 then
            let state =
                // Don't forget that boss attacks always deal at least 1 damage.
                { state with
                    HitPoints = state.HitPoints - Math.Max (state.BossDamage - state.Armour, 1) }

            // Check to see if the boss has defeated us.
            if state.HitPoints <= 0 then
                BossWonGame
            else
                ActiveGame state
        else
            PlayerWonGame state.ManaSpent

    | _ -> failwith "Cannot update a game that has already finished."


let rec generateGames startingState minManaSpent (cont: int -> int) =
    match startingState with
    | ActiveGame innerState as activeState when innerState.ManaSpent < minManaSpent ->
        let contShield mms =
            iterateGame activeState Spell.Shield mms cont

        let contRecharge mms =
            iterateGame activeState Spell.Recharge mms contShield

        let contPoison mms =
            iterateGame activeState Spell.Poison mms contRecharge

        let contDrain mms =
            iterateGame activeState Spell.Drain mms contPoison

        iterateGame activeState Spell.MagicMissile minManaSpent contDrain               
                
    | PlayerWonGame manaSpent ->
        cont <| Math.Min(manaSpent, minManaSpent)

    | ActiveGame _ | BossWonGame | InvalidGame _ ->
        cont minManaSpent

and iterateGame gameState newSpell minManaSpent (cont: int -> int) =
    match updateGameStateForPlayer newSpell gameState with
    | ActiveGame _ as newActiveState ->
        let newStateAfterBoss =
            updateGameStateForBoss newActiveState

        generateGames newStateAfterBoss minManaSpent cont

    | PlayerWonGame manaSpent ->
        cont <| Math.Min(manaSpent, minManaSpent)

    | BossWonGame | InvalidGame _ ->
        cont minManaSpent


[<EntryPoint>]
let main _ =
    let startingState =
        { LastPlayed = Boss
          TurnIdx = -1
          Mana = 500
          ManaSpent = 0
          HitPoints = 50
          Armour = 0
          BossHitPoints = 51
          BossDamage = 9
          ActiveSpells = []
          Difficulty = Normal }

    generateGames (ActiveGame startingState) Int32.MaxValue id
    |> printfn "Part 1 answer = %i\n"

    let hardStartingState =
        { startingState with Difficulty = Hard }
    
    generateGames (ActiveGame hardStartingState) Int32.MaxValue id
    |> printfn "Part 2 answer = %i"

    0