module View exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html

import Types exposing (..)

border : List (Svg Msg)
border =
  [
    rect [x "0", y "0", width "800", height "3", fill "#0B79CE"] [],
    rect [x "0", y "0", width "3", height "800", fill "#0B79CE"] [],
    rect [x "0", y "797", width "800", height "3", fill "#0B79CE"] [],
    rect [x "797", y "0", width "3", height "800", fill "#0B79CE"] []
  ]

drawText : String -> Int -> Int -> Int -> Svg Msg
drawText str nX nY size =
  text_ [x (toString nX), y (toString nY), fontFamily "Verdana", fontSize (toString size)]
      [ Html.text str ]


drawTextColored : String -> Int -> Int -> Int -> String -> Svg Msg
drawTextColored str nX nY size col =
  text_ [x (toString nX), y (toString nY), fontFamily "Verdana", fontSize (toString size), fill col]
      [ Html.text str ]

paused : List (Svg Msg)
paused =
  [
    drawText "Paused" (400 - 65) 385 35,
    drawText "Instructions:" 30 30 15,
    drawText "(c) Cheat | (wasd or arrows) Move | (esc) Pause | (space) Shoot" 45 45 15
  ]

drawHealth : Int -> List (Svg Msg)
drawHealth health =
  [
    rect [x "200", y "770", width "400", height "20", fill "#FF0000"] [],
    rect [x "200", y "770", width (toString (health * 4)), height "20", fill "#00ff00"] [],
    drawText (String.concat ["HP : ", toString health, " / 100"]) (400-45) 785 15
  ]


drawBackground : Int -> List (Svg Msg)
drawBackground backgroundY =
  let noise = [35, 330, 650] in
  List.concat <|
    List.map
      (\nX ->
        List.map
          (\nY ->
            drawBox (nX + (nY) % 30) ((nY - nX + backgroundY) % 800) 4 8 "#faaffa")
          noise)
      noise

drawPoints : Int -> List (Svg Msg)
drawPoints points =
  let (reducedPoints, bg) =
    if points >= 300 then
      (points % 100, "#ff00cb")
    else if points >= 200 then
      (points % 100, "#d4ff00")
    else if points >= 100 then
      (points % 100, "#59a35d")
    else
      (points, "#42363f")
  in
  [
    rect [x "200", y "740", width "400", height "20", fill bg] [],
    rect [x "200", y "740", width (toString (reducedPoints * 4)), height "20", fill "#00ffff"] [],
    drawText (String.concat ["Points : ", toString reducedPoints, " / 100"]) (400-45) 755 15
  ]

drawEnemies : List EntityEnemy -> List (Svg Msg)
drawEnemies enemies =
  List.map
    (\e ->
      case e.kind of
        Simple ->
          drawBoxFloat (e.position.x - 8) (e.position.y - 8) 16 16 "#994a85"
        Hardened ->
          drawBoxFloat (e.position.x - 8) (e.position.y - 8) 16 16 "#59a35d"
        Psychotic ->
          drawBoxFloat (e.position.x - 8) (e.position.y - 8) 16 16 "#d4ff00"
        Godlike ->
          drawBoxFloat (e.position.x - 8) (e.position.y - 8) 16 16 "#ff00cb"
    )
    enemies

triangle : Position -> Float -> String -> Svg Msg
triangle pos size color =
  let (a, b, c) =
    (
      {x = pos.x - (size / 2.0),
       y = pos.y + (size / 2.0)},
      {x = pos.x + (size / 2.0),
       y = pos.y + (size / 2.0)},
      {x = pos.x,
       y = pos.y - (size / 2.0)}) in
  let getPoint = (\p ->
    String.concat [
      toString <| round p.x,
      ",",
      toString <| round p.y,
      " "
    ]
  ) in
  polygon [
    points (String.concat [
      getPoint a,
      getPoint b,
      getPoint c
    ])
  ] []

drawPlayer : EntityPlayer -> List (Svg Msg)
drawPlayer player =
  [
    triangle player.position 20.0 "#ffac47"
  ]

drawBoxFloat : Float -> Float -> Int -> Int -> String -> Svg Msg
drawBoxFloat x y w h col =
  drawBox (round x) (round y) w h col

drawBullets : List EntityBullet -> List (Svg Msg)
drawBullets bullets =
  List.map
    (\b ->
      drawBoxFloat (b.position.x - 5) (b.position.y - 5) 10 10 "#39d3ad"
    )
    bullets


drawBox : Int -> Int -> Int -> Int -> String -> Svg Msg
drawBox iX iY w h f =
  rect
    [
      x (toString iX),
      y (toString iY),
      width (toString w),
      height (toString h),
      fill f
    ] []

drawWeaponGraphic : Int -> List (Svg Msg)
drawWeaponGraphic cooldown =
  if cooldown == 0 then
    [ drawBox 30 750 40 40 "#000000",
      drawBox 40 730 20 30 "#000000" ]
  else
    [ drawBox 30 750 40 40 "#ff0000",
      drawBox 40 730 20 30 "#ff0000" ]

drawWeapon : Weapon -> List (Svg Msg)
drawWeapon weapon =
  case weapon of
    Basic cooldown ->
      List.append (drawWeaponGraphic cooldown)
      [(drawTextColored "Basic" 40 770 8 "#ffffff")]
    Fast cooldown ->
      List.append (drawWeaponGraphic cooldown)
      [(drawTextColored "Fast" 42 770 8 "#ffffff")]
    MultiShot cooldown ->
      List.append (drawWeaponGraphic cooldown)
      [(drawTextColored "MultiShot" 30 770 8 "#ffffff")]
    Homing cooldown ->
      List.append (drawWeaponGraphic cooldown)
      [(drawTextColored "Homing" 35 770 8 "#ffffff")]



drawView : Model -> List (Svg Msg)
drawView model =
  if model.paused then
    List.concat [
      border,
      paused
    ]
  else
    List.concat [
      border,
      drawBackground model.backgroundY,
      drawBullets model.bullets,
      drawPlayer model.player,
      drawEnemies model.enemies,
      drawPoints model.points,
      drawHealth model.player.health,
      drawWeapon model.player.weapon
    ]


view model =
  svg
    [ viewBox "0 0 800 800", width "800px"]
    (drawView model)