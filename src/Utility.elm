module Utility exposing (pushBottomBarAway)

import Element as El
import Types exposing (..)

pushBottomBarAway : List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
pushBottomBarAway elements =
    elements ++ [ El.el [ El.height (El.px 1000) ] El.none ]

