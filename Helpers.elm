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
