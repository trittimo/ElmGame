module Helpers exposing (..)

import Types exposing (..)

vec : Float -> Float -> Vector
vec fX fY =
  {x = fX, y = fY}

addUpTo a b n =
  if a + b > n then
    n
  else if a + b < -n then
    -n
  else
    a + b

vecAddMax : Vector -> Vector -> Float -> Vector
vecAddMax a b m =
  {a|x = addUpTo a.x b.x m, y = addUpTo a.y b.y m}

vecAdd : Vector -> Vector -> Vector
vecAdd a b =
  {a|x = a.x + b.x, y = a.y + b.y}

vecInv : Vector -> Vector
vecInv a =
  {a|x = -a.x, y = -a.y}

vecDiv : Vector -> Float -> Vector
vecDiv a f =
  {a|x = a.x / f, y = a.y / f}

dist : Position -> Position -> Float
dist a b =
  sqrt ((a.x - b.x)^2 + (a.y - b.y)^2)