module Style exposing (Icon(..), blue, darkBlue, drawIcon, gitHubIcon, gray, lavender, makeIcon, offLavender, offWhite, offYellow, red, scaled, sunIcon, white, yellow)

import Color as Cl
import Element as El
import Svg
import Svg.Attributes as SvgA



-- Font


scaled : Int -> Int
scaled scale =
    El.modular 16 1.5 scale
        |> floor



-- Colors


blue =
    El.rgb255 124 175 194


darkBlue =
    El.rgb255 64 93 104


gray : El.Color
gray =
    El.rgb255 73 73 73


yellow : El.Color
yellow =
    El.rgb255 247 202 136


offYellow : El.Color
offYellow =
    El.rgb255 237 192 126


red : El.Color
red =
    El.rgb255 171 70 66


white : El.Color
white =
    El.rgb255 255 255 255


offWhite : El.Color
offWhite =
    El.rgb255 250 250 250


lavender : El.Color
lavender =
    El.rgb255 186 139 175


offLavender : El.Color
offLavender =
    El.rgb255 176 129 165



-- Icons


sunIcon : Icon
sunIcon =
    makeIcon "sun-solid.svg"


gitHubIcon : Icon
gitHubIcon =
    makeIcon "github-brands.svg"


type Icon
    = Icon String


makeIcon : String -> Icon
makeIcon filename =
    Icon ("/icons/" ++ filename)


drawIcon attrs (Icon iconName) =
    El.el
        attrs
    <|
        El.html <|
            Svg.svg
                [ SvgA.width "32"
                , SvgA.height "32"
                , SvgA.viewBox "0 0 32 32"
                ]
                [ Svg.image
                    [ SvgA.x "0"
                    , SvgA.y "0"
                    , SvgA.width "32"
                    , SvgA.height "32"
                    , SvgA.xlinkHref iconName
                    ]
                    []
                ]
