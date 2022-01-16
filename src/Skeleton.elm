module Skeleton exposing (view)

import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Page.Error as Error
import Page.Ifs as Ifs
import Page.Login as Login
import Style
import Types exposing (..)



-- VIEW


view : FrontendModel -> El.Element FrontendMsg
view model =
    let
        kids =
            case model.page of
                Login ->
                    Login.view model

                Ifs ->
                    Ifs.view model

                NotFound ->
                    Error.view
    in
    El.column
        [ El.width <| El.maximum 1000 El.fill
        , El.centerX
        , El.spacing 20
        , Font.family
            [ Font.typeface "Roboto"
            , Font.sansSerif
            ]
        , Font.color Style.gray
        ]
        (List.concat
            [ [ navBar model ]
            , kids
            , [ footer ]
            ]
        )


navBar : FrontendModel -> El.Element FrontendMsg
navBar model =
    let
        row =
            El.wrappedRow [ El.width <| El.maximum 1000 El.fill, El.spacing 15 ]

        baseButtons =
            []
    in
    case model.user of
        Just user ->
            row
                (baseButtons |> withLoggedIn user)

        Nothing ->
            row
                (baseButtons |> withRightNavButton "/login" "Log In")


withLeftNavButton : String -> String -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withLeftNavButton url name elements =
    elements
        ++ [ El.link
                [ El.centerY
                , El.mouseOver [ Font.color Style.blue, Background.color Style.white ]
                , El.focused [ Background.color Style.offWhite ]
                , Border.rounded 3
                , El.padding 10
                , Font.size 17
                ]
                { url = url
                , label = El.text <| String.toUpper name
                }
           ]


withRightNavButton : String -> String -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withRightNavButton url name elements =
    elements
        ++ [ El.link
                rightNavStyle
                { url = url
                , label = El.text <| String.toUpper name
                }
           ]


rightNavStyle : List (El.Attribute msg)
rightNavStyle =
    [ El.centerY
    , El.mouseOver [ Font.color Style.blue, Background.color Style.white ]
    , El.focused [ Background.color Style.offWhite ]
    , Border.rounded 3
    , El.padding 10
    , Font.size 17
    , El.alignRight
    ]


withLoggedIn : FrontendUser -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withLoggedIn user elements =
    elements ++ [ El.el rightNavStyle <| El.text user.email ]


footer : El.Element FrontendMsg
footer =
    El.paragraph
        [ Font.size 8
        , El.padding 10
        , Border.rounded 3
        , Font.color Style.white
        -- , Background.color Style.red
        , El.alignBottom
        ]
        []
