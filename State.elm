module State exposing (..)

import Html exposing (Html)
import Time exposing (Time)
import Keyboard

import Types exposing (..)
import Helpers exposing (..)
import List exposing(..)

init : (Model, Cmd Msg)
init =
  (
  {
    backgroundY = 0,
    points = 0,
    player = {
      position = {x = 400.0, y = 700.0},
      velocity = vec 0.0 0.0,
      effects = [],
      weapon = Basic 0,
      powerups = [],
      health = 100
    },
    powerups = [],
    enemies = [],
    button = NoButton,
    bullets = [],
    paused = True,
    wave = 5000
  },
  Cmd.none
  )

handleButtons : Model -> Model
handleButtons model =
  let player = model.player in
  {model|player =
    {player|velocity =
      vecAddMax
        player.velocity
        (case model.button of
          Left -> vec -0.04 0
          Right -> vec 0.04 0
          Forward -> vec 0 -0.04
          Back -> vec 0 0.04
          _ -> vec 0 0)
        0.8}}

newBullet : Position -> Vector -> Weapon -> BulletOrigin -> List EntityBullet
newBullet pos dir weap origin =
  case weap of
    MultiShot c ->
      -- Multiple bullets
      [{
        position = pos,
        firedBy = origin,
        velocity = vecAdd dir (vecDiv (vecInv dir) 3),
        kind = BasicShot
        },
        {
        position = pos,
        firedBy = origin,
        velocity =
          let newvec = vecAdd dir (vec -1 0) in
          vecAdd newvec (vecDiv (vecInv newvec) 3),
        kind = BasicShot
        },
        {
        position = pos,
        firedBy = origin,
        velocity =
          let newvec = vecAdd dir (vec 1 0) in
          vecAdd newvec (vecDiv (vecInv newvec) 3),
        kind = BasicShot
        }]
    _ ->
      [{
        position = pos,
        firedBy = origin,
        velocity =
          case weap of
            Basic c -> vecAdd dir (vecDiv (vecInv dir) 3)
            Fast c -> dir
            _ -> vecAdd dir (vecDiv (vecInv dir) 3),
        kind =
          case weap of
            Basic c -> BasicShot
            Fast c -> FastShot
            MultiShot c -> BasicShot
            Homing c -> HomingShot
      }]

shootBullet : EntityPlayer -> Weapon -> List EntityBullet
shootBullet player weapon =
  newBullet player.position (vec 0 -1) weapon ThePlayer

shootBulletEnemy : EntityEnemy -> Weapon -> List EntityBullet
shootBulletEnemy enemy weapon =
  newBullet enemy.position (vec 0 1) weapon TheEnemy

handleShoot : Model -> Model
handleShoot model =
  case model.button of
    Shoot ->
      (let player = model.player in
      let weap = player.weapon in
      case weap of
        Basic c ->
          if c > 0 then
            model
          else
            {model|player = {player|weapon = Basic 300},
                  bullets =
                    List.append
                      (shootBullet player weap)
                      model.bullets}
        Fast c ->
          if c > 0 then
            model
          else
            {model|player = {player|weapon = Fast 300},
                  bullets =
                    List.append
                      (shootBullet player weap)
                      model.bullets}
        MultiShot c ->
          if c > 0 then
            model
          else
            {model|player = {player|weapon = MultiShot 300},
                  bullets =
                    List.append
                      (shootBullet player weap)
                      model.bullets}
        Homing c ->
          if c > 0 then
            model
          else
            {model|player = {player|weapon = Homing 300},
                  bullets =
                    List.append
                      (shootBullet player weap)
                      model.bullets})
    _ -> model

handleMovement : Model -> Model
handleMovement model =
  let player = model.player in
  {model|player =
    {player|position =
      {x = player.position.x + player.velocity.x,
       y = player.position.y + player.velocity.y}}}


handleDrag : Model -> Model
handleDrag model =
  let player = model.player in
  {model|player = 
    {player|velocity =
      {x =
        if player.velocity.x > 0 then
          player.velocity.x - 0.01
        else if player.velocity.x < 0 then
          player.velocity.x + 0.01
        else
          player.velocity.x,
      y =
        if player.velocity.y > 0 then
          player.velocity.y - 0.01
        else if player.velocity.y < 0 then
          player.velocity.y + 0.01
        else
          player.velocity.y}}}


handleBackground : Model -> Model
handleBackground model =
  if model.backgroundY >= 800 then
    {model|backgroundY = 0}
  else
    {model|backgroundY = model.backgroundY + 1}


handleWeaponCooldown : Model -> Model
handleWeaponCooldown model =
  let player = model.player in
  if (case player.weapon of
        Basic c -> c
        Fast c -> c
        MultiShot c -> c
        Homing c -> c) > 0 then
    {model|player = {player|weapon =
      case player.weapon of
        Basic c -> Basic (c - 1)
        Fast c -> Fast (c - 1)
        MultiShot c -> MultiShot (c - 1)
        Homing c -> Homing (c - 1)}}
  else
    model

moveBullet : EntityBullet -> EntityBullet
moveBullet bullet =
  let pos = bullet.position in
  {bullet|position =
    {pos|x = pos.x + bullet.velocity.x, y = pos.y + bullet.velocity.y}}

getNearestEnemy : Position -> Model -> EntityEnemy
getNearestEnemy pos model =
  List.foldl
    (\current nearest ->
      if (dist pos current.position) < (dist pos nearest.position) then
        current
      else
        nearest)
    {
      position = {x=-80000,y=-80000},
      effects = [],
      weapon = Basic 0,
      health = 10,
      kind = Simple
    }
    model.enemies

moveTowards : Position -> Vector -> Position -> Vector
moveTowards mypos vel enemy =
  {vel|x =
    if mypos.x > enemy.x && vel.x >= 0 then
      vel.x - 0.09
    else if mypos.x < enemy.x && vel.x <= 0 then
      vel.x + 0.09
    else
      vel.x
    }

attractBullet : EntityBullet -> Model -> EntityBullet
attractBullet bullet model =
  case bullet.kind of
    HomingShot ->
      case bullet.firedBy of
        ThePlayer ->
          if List.length model.enemies > 0 then 
            {bullet|velocity = moveTowards bullet.position bullet.velocity (getNearestEnemy bullet.position model).position}
          else
            bullet
        _ ->
          -- TODO
          bullet
    _ -> bullet


handleBullets : Model -> Model
handleBullets model =
  let moved = {model|bullets = List.map (\b -> moveBullet b) model.bullets} in
  let moved2 =
    {moved|bullets =
      List.map (\b -> attractBullet b moved) moved.bullets} in
  moved2

collidesEnemy : (EntityBullet, EntityEnemy) -> Float -> Bool
collidesEnemy (b, e) r =
  if b.firedBy == ThePlayer then
    (dist b.position e.position) < r
  else
    False

addPowerups : Model -> Model
addPowerups model =
  if List.length model.enemies == 0 then
    if model.points >= 300 then
      {model|powerups = [{position = {x=400, y=400}, kind = HomingWeapon}]}
    else if model.points >= 200 then
      {model|powerups = [{position = {x=400, y=400}, kind = MultiShotWeapon}]}
    else if model.points >= 100 then
      {model|powerups = [{position = {x=400, y=400}, kind = FastWeapon}]}
    else
      model
  else
    model

removeAll : List a -> List a -> List a
removeAll ls toRemove =
  filter (\x -> not (member x toRemove)) ls

handleBulletCollision : Model -> Model
handleBulletCollision model =
  let combined = concat (map (\b -> map (\e -> (b, e)) model.enemies) model.bullets) in
  if length combined == 0 then
    model
  else
    let (deadB, deadE) = unzip (filter (\(b, e) ->
      (dist b.position e.position) < 15 && b.firedBy == ThePlayer) combined) in
    let (aliveB, aliveE) = (removeAll model.bullets deadB,removeAll model.enemies deadE) in
    addPowerups {model|bullets=aliveB,enemies=aliveE,points=model.points + ((length model.enemies)-(length aliveE))*25}

  --let bullets = model.bullets in
  --let enemies = model.enemies in
  --if List.length bullets > 0 && List.length enemies > 0 then
  --  let player = model.player in
  --  let friendlyBullets = List.filter (\b -> b.firedBy == ThePlayer) bullets in
  --  let combined = List.concat <| List.map (\x -> List.map (\y -> (x, y)) enemies) friendlyBullets in
  --  let collisions = getCollisions combined in
  --  let (aliveBullets, aliveEnemies) = List.unzip collisions in
  --  let newmodel = {model|bullets = aliveBullets, enemies = aliveEnemies, points = model.points + ((List.length enemies) - (List.length aliveEnemies)) * 25} in
  --  addPowerups newmodel
  --else
  --  model

handlePlayerBulletCollision : Model -> Model
handlePlayerBulletCollision model =
  let pos = model.player.position in
  let player = model.player in
  let bullets = model.bullets in
  let collisions = List.filter (\x -> (dist pos x.position < 15) && x.firedBy == TheEnemy) bullets in
  let newmodel = {model|player = {player|health = player.health - (List.length collisions) * 10}} in
  {newmodel|bullets = List.filter (\x -> not (List.member x collisions)) bullets}

handlePickupPowerup : Model -> Model
handlePickupPowerup model =
  if (List.length model.powerups) > 0 && (dist model.player.position {x=400,y=400}) < 40 then
    let player = model.player in
    {model|powerups=[],player={player|weapon=
      case List.head model.powerups of
        Just n ->
          case n.kind of
            FastWeapon -> Fast 0
            MultiShotWeapon -> MultiShot 0
            HomingWeapon -> Homing 0
            _ -> Basic 0
        _ -> Basic 0}}
  else
    model

handleCheat : Model -> Model
handleCheat model =
  case model.button of
    Cheat ->
      let player = model.player in
      {model|player = {player|weapon =
        case player.weapon of
          Basic c ->
            if c > 0 then
              Basic c
            else
              Fast 50
          Fast c ->
            if c > 0 then
              Fast c
            else
              MultiShot 50
          MultiShot c ->
            if c > 0 then
              MultiShot c
            else
              Homing 50
          Homing c ->
            if c > 0 then
              Homing c
            else
              Basic 50}}
    _ ->
      model

destroyOffscreenBullets : Model -> Model
destroyOffscreenBullets model =
  {model|bullets = List.filterMap 
    (\b ->
      if b.position.x > 800 || b.position.x < 0 ||
         b.position.y > 800 || b.position.y < 0 then
        Nothing
      else
        Just b
    )
    model.bullets
  }

handleAddEnemies : Model -> Model
handleAddEnemies model =
  if (List.length model.enemies) > 0 then
    model
  else
    {model|enemies =
      if model.points >= 300 then
        [{
          position = {x = 400, y = 40},
          effects = [],
          weapon = Homing 0,
          health = 10,
          kind = Godlike
        },
        {
          position = {x = 200, y = 40},
          effects = [],
          weapon = Homing 0,
          health = 10,
          kind = Godlike
        },
        {
          position = {x = 600, y = 40},
          effects = [],
          weapon = Homing 0,
          health = 10,
          kind = Godlike
        },
        {
          position = {x = 400, y = 80},
          effects = [],
          weapon = Homing 0,
          health = 10,
          kind = Godlike
        }]
      else if model.points >= 200 then
        [{
          position = {x = 400, y = 40},
          effects = [],
          weapon = MultiShot 0,
          health = 10,
          kind = Psychotic
        },
        {
          position = {x = 200, y = 40},
          effects = [],
          weapon = MultiShot 0,
          health = 10,
          kind = Psychotic
        },
        {
          position = {x = 600, y = 40},
          effects = [],
          weapon = MultiShot 0,
          health = 10,
          kind = Psychotic
        },
        {
          position = {x = 400, y = 80},
          effects = [],
          weapon = MultiShot 0,
          health = 10,
          kind = Psychotic
        }]
      else if model.points >= 100 then
        [{
          position = {x = 400, y = 40},
          effects = [],
          weapon = Fast 0,
          health = 10,
          kind = Hardened
        },
        {
          position = {x = 200, y = 40},
          effects = [],
          weapon = Fast 0,
          health = 10,
          kind = Hardened
        },
        {
          position = {x = 600, y = 40},
          effects = [],
          weapon = Fast 0,
          health = 10,
          kind = Hardened
        },
        {
          position = {x = 400, y = 80},
          effects = [],
          weapon = Fast 0,
          health = 10,
          kind = Hardened
        }]
      else
        [{
          position = {x = 400, y = 40},
          effects = [],
          weapon = Basic 0,
          health = 10,
          kind = Simple
        },
        {
          position = {x = 200, y = 40},
          effects = [],
          weapon = Basic 0,
          health = 10,
          kind = Simple
        },
        {
          position = {x = 600, y = 40},
          effects = [],
          weapon = Basic 0,
          health = 10,
          kind = Simple
        },
        {
          position = {x = 400, y = 80},
          effects = [],
          weapon = Basic 0,
          health = 10,
          kind = Simple
        }]
    }

handleEnemyLogic : Model -> Model
handleEnemyLogic model = model

handleTick : Model -> Model
handleTick model =
  model |>
    handleCheat |>
    destroyOffscreenBullets |>
    handlePickupPowerup |>
    handleDrag |>
    handleButtons |>
    handleWeaponCooldown |>
    handleShoot |>
    handleBullets |>
    handleBulletCollision |>
    handleAddEnemies |>
    handleEnemyLogic |>
    handlePlayerBulletCollision |>
    handleMovement |>
    handleBackground


handleKey : ButtonState -> Model -> Model
handleKey key model =
  case key of
    Pause -> {model|paused = (not model.paused)}
    _ -> {model|button = key}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (case msg of
    -- Debug.log "model"
    Key key -> (handleKey key model)
    Tick -> handleTick model
    _ -> model,
  Cmd.none)


getButtonFromCode : Keyboard.KeyCode -> ButtonState
getButtonFromCode code =
  case code of
    37 -> Left
    39 -> Right
    38 -> Forward
    40 -> Back
    65 -> Left
    87 -> Forward
    68 -> Right
    83 -> Back
    27 -> Pause
    67 -> Cheat
    32 -> Shoot
    _ -> NoButton

handleKeyDown : Keyboard.KeyCode -> ButtonState -> Msg
handleKeyDown code currentButton =
  let button = getButtonFromCode code in
  case button of
    NoButton -> NoUpdate
    _ ->
      if currentButton == button then
        NoUpdate
      else
        Key button


handleKeyUp : Keyboard.KeyCode -> ButtonState -> Msg
handleKeyUp code currentButton =
  let button = getButtonFromCode code in
  if button == currentButton then
    Key NoButton
  else
    NoUpdate


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [
    Keyboard.downs (\k -> handleKeyDown k model.button),
    Keyboard.ups (\k -> handleKeyUp k model.button),
    Time.every 5 (\t -> if model.paused then NoUpdate else Tick)
  ]