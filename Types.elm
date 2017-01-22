module Types exposing (..)

type alias Position =
    {
        x : Float,
        y : Float
    }

type alias Vector =
    {
        x : Float,
        y : Float
    }
type alias Duration = Int

type Effect = -- All ints are duration left
    Necrotic Duration | Overcharged Duration | Speedy Duration | Armored Duration

type alias Cooldown = Duration

type Bullet =
    BasicShot | FastShot | HomingShot

type Weapon =
    Basic Cooldown | Fast Cooldown | MultiShot Cooldown | Homing Cooldown

type Powerup =
      Health -- Has a random chance of dropping from any enemy
    | FastWeapon -- Drops from the last enemy of stage1
    | MultiShotWeapon -- Drops from the last enemy of stage2
    | HomingWeapon -- Drops from the last enemy of stage3

type Enemy =
    Simple | Hardened | Psychotic | Godlike

type ButtonState =
    Pause | Left | Right | Forward | Back | Shoot | NoButton | Cheat

type alias EntityPowerup =
    {
        position : Position,
        look : Vector,
        kind : Powerup
    }

type alias EntityPlayer =
    {
        position : Position,
        velocity : Vector,
        effects : List Effect,
        weapon : Weapon,
        powerups : List Powerup,
        health : Int
    }

type alias EntityEnemy =
    {
        position : Position,
        effects : List Effect,
        weapon : Weapon,
        health : Int,
        kind : Enemy
    }

type alias EntityBullet =
    {
        position : Position,
        direction : Vector,
        kind : Bullet
    }

type alias Model =
    {
        backgroundY : Int,
        points : Int,
        player : EntityPlayer,
        enemies : List EntityEnemy,
        button : ButtonState,
        bullets : List EntityBullet,
        paused : Bool,
        wave : Int -- Time left before the next wave attacks
    }

type Msg =
    NoUpdate | Tick | Key ButtonState