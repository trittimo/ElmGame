import Types exposing (..)

import Html exposing (Html)

import State
import View

main : Program Never Model Msg
main =
    Html.program
    {
        init = State.init,
        update = State.update,
        subscriptions = State.subscriptions,
        view = View.view
    }