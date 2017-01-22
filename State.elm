module State exposing (..)

import Html exposing (Html)
import Time exposing (Time)
import Keyboard

import Types exposing (..)
import Helpers exposing (..)

init : (Model, Cmd Msg)
init =
    (
    {
        backgroundY = 0,
        points = 335,
        player = {
            position = {x = 400.0, y = 700.0},
            velocity = vec 0.0 0.0,
            effects = [],
            weapon = Basic 0,
            powerups = [],
            health = 100
        },
        enemies = [],
        button = NoButton,
        bullets = [],
        paused = True,
        wave = 5000
    },
    Cmd.none
    )

--moveplayer : ButtonState -> EntityPlayer -> EntityPlayer
--moveplayer key player =
--    let pos = player.position in
--    {
--    player|position =
--        case key of
--            Forward -> {pos|y = pos.y - 0.5}
--            Back -> {pos|y = pos.y + 0.5}
--            Right -> {pos|x = pos.x + 0.5}
--            Left -> {pos|x = pos.x - 0.5}
--            _ -> pos
--    }

--addVelocities : Velocity -> Velocity -> Float -> Velocity
--addVelocities v1 v2 m =
--    {v1|x = v1.x + v2.x, y = v1.y + v2.y}

handleButtons : Model -> Model
handleButtons model = model


handleMovement : Model -> Model
handleMovement model = model

handleTick : Model -> Model
handleTick model =
    model |>
        handleButtons |>
        handleMovement



handleKey : ButtonState -> Model -> Model
handleKey key model =
    case key of
        Pause -> {model|paused = (not model.paused)}
        _ -> {model|button = key}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (case msg of
        Key key -> Debug.log "model" (handleKey key model)
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
        64 -> Left
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