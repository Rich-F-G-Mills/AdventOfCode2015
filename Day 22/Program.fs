
open System

let Boss =
    {| HitPoints = 51; Damage = 9 |}


type Turn =
    | Player
    | Boss

type GameState =
    { Turn: Turn
      TurnIdx: int
      Mana: int
      ManaSpent: int
      HitPoints: int      
      Armour: int
      BossHitPoints: int }


module Spell =

    type ImpactMetrics =
        { Mana: int
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


    let nullSpell =
        { Mana = 0; HitPoints = 0; Armour = 0; BossHitPoints = 0 }: Impact


module MagicMissile =

    let private effect =
        { Spell.nullSpell with
            Mana = -53
            BossHitPoints = -4 }




module Drain =

    let private effect =
        { Spell.nullSpell with
            Mana = -73
            HitPoints = 2
            BossHitPoints = -2 }


module Shield =
    
    let private effectNonInitial =
        { Spell.nullSpell with
            Armour = 7 }

    let private effectInitial =
        { effectNonInitial with
            Mana = -113 }


[<EntryPoint>]
let main argv =
    let startingState =
        { Turn = Player
          Mana = 500
          ManaSpent = 0
          HitPoints = 50
          Armour = 0
          BossHitPoints = Boss.HitPoints }