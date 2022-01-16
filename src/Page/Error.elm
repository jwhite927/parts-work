module Page.Error exposing (view)

import Element as El
import Element.Font as Font
import Style exposing (gray)
import Types exposing (..)
import Utility exposing (..)



-- VIEW


view : List (El.Element FrontendMsg)
view =
    [ errorView ]
        |> pushBottomBarAway


errorView : El.Element msg
errorView =
    El.column
        [ El.width <| El.maximum 1152 El.fill
        , El.centerX
        , Font.family
            [ Font.typeface "Roboto"
            , Font.sansSerif
            ]
        , Font.color gray
        ]
        [ El.el [El.centerX, El.padding 50] (El.text "404 Page not Found")
        ]
