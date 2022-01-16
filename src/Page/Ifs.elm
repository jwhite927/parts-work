module Page.Ifs exposing (view)

import Color
import Dict exposing (Dict)
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Style
import TypedSvg as TS
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core as TSC
import TypedSvg.Events as TSE
import TypedSvg.Types as TST
import Types exposing (..)
import VirtualDom



-- DONE Make it so parts can't overlap
-- DONE Make parts resizable
-- TODO Improve resize UX / Appearance
-- TODO Use an hsluv color scheme that looks nice
-- TODO Make it so part color can be changed
-- TODO Make it so relationships between parts can be annotated
-- TODO Add a part journaling feature
-- TODO Add view functions
--   1. TODO Make view mode
--   2. TODO Add zoom functionality
--   3. TODO Add pan functionality
-- VIEW


view : FrontendModel -> List (El.Element FrontendMsg)
view model =
    let
        board =
            El.el [ El.inFront <| viewMenus model, Border.color Style.gray, Border.width 3, El.centerX ] (El.html <| boardSvg model)
    in
    case model.user of
        Just _ ->
            [ board ]

        Nothing ->
            [ Input.button
                [ Font.size 18, Font.color Style.red, El.centerX ]
                { label = El.text "You are working in scratch mode. Log in to save your work"
                , onPress = Just <| RedirectToLoginFrom Ifs
                }
            , board
            ]


viewMenus model =
    case model.ifsMode of
        EditParts editMode ->
            case editMode of
                CreatingPart ->
                    El.row [ El.centerX, El.centerY, El.spacing 8 ]
                        [ Input.text
                            [ El.centerY
                            , El.width (El.px 500)
                            , El.spacing 8
                            , onEnter GotNewPart
                            ]
                            { onChange = UpdateNewPartName
                            , text = model.ifsNewPartName
                            , placeholder = Nothing
                            , label = Input.labelAbove [] (El.text "What is the name of this part?")
                            }
                        , Input.button
                            [ El.alignBottom
                            , El.width (El.px 71)
                            , El.height (El.px 44)
                            , Background.color Style.offLavender
                            , El.mouseOver [ Background.color Style.offLavender ]
                            , Font.color <| El.rgb255 255 255 255
                            , Border.rounded 3
                            , El.centerX
                            ]
                            { onPress = Just GotNewPart
                            , label = El.el [ El.centerY, El.centerX ] <| El.text "Save"
                            }
                        ]

                _ ->
                    El.row [ El.spacing 5, El.padding 10 ]
                        [ drawButton "Add Part" <| SelectIfsMode <| EditParts CreatingPart
                        , drawButton "View" <| SelectIfsMode ViewMode
                        , drawButton "Zoom In" <| ChangeView ZoomIn
                        , drawButton "Zoom Out" <| ChangeView ZoomOut
                        ]

        ViewMode ->
            El.row [ El.spacing 5 ]
                [ drawButton "Edit" <| SelectIfsMode <| EditParts Normal
                ]


drawButton : String -> msg -> El.Element msg
drawButton label action =
    Input.button
        [ El.padding 7
        , Background.color Style.lavender
        , El.mouseOver [ Background.color Style.offLavender ]
        , Font.color <| El.rgb255 255 255 255
        , Font.size 14
        , Border.rounded 3
        ]
        { onPress = Just action
        , label =
            El.el
                [ El.centerX, El.centerY ]
            <|
                El.text label
        }


boardSvg : FrontendModel -> Html FrontendMsg
boardSvg model =
    TS.svg
        ([ viewBox model.ifsView.x model.ifsView.y model.ifsView.w model.ifsView.h
         , id "boardSvg"
         , width <| TST.px 800
         , height <| TST.px 600
         ]
            |> withEndEditPartEvent model
            |> withMovingPartEvent model
        )
        (drawParts (Dict.values model.ifsParts)
            -- |> withButtons
            |> withMoveVector model
        )


withEndEditPartEvent : FrontendModel -> List (TSC.Attribute FrontendMsg) -> List (TSC.Attribute FrontendMsg)
withEndEditPartEvent model elements =
    case model.ifsMode of
        EditParts _ ->
            TSE.onMouseUp EndEditPart
                :: elements

        _ ->
            elements

withMovingPartEvent : FrontendModel -> List (TSC.Attribute FrontendMsg) -> List (TSC.Attribute FrontendMsg)
withMovingPartEvent model elements =
    case model.ifsMode of
        EditParts editMode ->
            case editMode of
                MovingPart ->
                    typeConverter (TSE.on "mousemove" moveDecoder)
                        :: elements

                ResizePart ->
                    typeConverter (TSE.on "mousemove" resizeDecoder)
                        :: elements

                _ ->
                    elements

        _ ->
            elements


moveDecoder : VirtualDom.Handler FrontendMsg
moveDecoder =
    Decode.map2 createNewPos (Decode.field "offsetX" Decode.float) (Decode.field "offsetY" Decode.float)
        |> VirtualDom.Normal


resizeDecoder : VirtualDom.Handler FrontendMsg
resizeDecoder =
    Decode.map (\x -> NewPartSize x) (Decode.field "offsetX" Decode.float)
        |> VirtualDom.Normal


typeConverter : VirtualDom.Attribute msg -> TSC.Attribute msg
typeConverter msg =
    msg


createNewPos : Float -> Float -> FrontendMsg
createNewPos x y =
    NewPartPos { x = x, y = y }


drawParts : List Part -> List (TSC.Svg FrontendMsg)
drawParts parts =
    List.map partCircle parts


withMoveVector model elements =
    case model.ifsMode of
        EditParts editMode ->
            case editMode of
                Normal ->
                    elements

                _ ->
                    case model.ifsEditing of
                        Just part ->
                            case model.ifsEditTarget of
                                Just target ->
                                    TS.g []
                                        [ TS.line
                                            [ x1 <| xPx part.xy
                                            , y1 <| yPx part.xy
                                            , x2 <| xPx target.xy
                                            , y2 <| yPx target.xy
                                            , stroke <| TST.Paint Color.blue
                                            ]
                                            []
                                        , TS.circle
                                            [ cx <| xPx target.xy
                                            , cy <| yPx target.xy
                                            , r <| TST.px target.r
                                            , fill <| TST.Paint Color.white
                                            , stroke <| TST.Paint Color.black
                                            , strokeDasharray "5,5"
                                            ]
                                            [ TS.animateTransform
                                                [ attributeType TST.AttributeTypeXml
                                                , attributeName "transform"
                                                , animateTransformType TST.AnimateTransformTypeRotate
                                                , from3 0 target.xy.x target.xy.y
                                                , to3 360 target.xy.x target.xy.y
                                                , dur <| TST.Duration "5s"
                                                , repeatCount TST.RepeatIndefinite
                                                ]
                                                []
                                            ]
                                        ]
                                        :: elements

                                Nothing ->
                                    elements

                        Nothing ->
                            elements

        _ ->
            elements


xPx : Position -> TST.Length
xPx vec =
    TST.px <| vec.x


yPx : Position -> TST.Length
yPx vec =
    TST.px <| vec.y


partCircle : Part -> TSC.Svg FrontendMsg
partCircle part =
    let
        resizePosition =
            Position (part.xy.x + part.r) part.xy.y

        shiftedX =
            withPos part resizePosition
    in
    TS.g
        []
        [ TS.circle
            ([ cx <| xPx part.xy
             , cy <| yPx part.xy
             , r (TST.px part.r)
             , fill <| TST.Paint Color.blue
             ]
                |> withMoveDragEvents part.name
            )
            []
        , partTextHelper part.xy.x part.xy.y 20 part.name
        , TS.circle
            [ cx <| xPx shiftedX.xy
            , cy <| yPx part.xy
            , r (TST.px 10)
            , fill <| TST.Paint Color.black
            , resizeDragEvent part.name
            ]
            []
        ]


partTextHelper : Float -> Float -> Float -> String -> TSC.Svg FrontendMsg
partTextHelper textX textY size name =
    TS.text_
        ([ x (TST.px textX)
         , y (TST.px textY)
         , textAnchor TST.AnchorMiddle
         , dominantBaseline TST.DominantBaselineMiddle
         , fontSize (TST.px size)
         , fontFamily [ "Helvetica", "sans-serif" ]
         , fill <| TST.Paint Color.white
         , noSelect
         ]
            |> withMoveDragEvents name
        )
        [ TSC.text name ]


noSelect : TSC.Attribute msg
noSelect =
    style "pointer-events: none"


withMoveDragEvents name attrs =
    List.concat
        [ [ TSE.onMouseDown <| StartMovingPart name
          ]
        , attrs
        ]


resizeDragEvent name =
    TSE.onMouseDown <| StartResizingPart name


textHelper : Float -> Float -> Float -> String -> TSC.Svg msg
textHelper textX textY size string =
    TS.text_
        [ x <| TST.percent textX
        , y <| TST.percent textY
        , textAnchor TST.AnchorMiddle
        , dominantBaseline TST.DominantBaselineMiddle
        , fontSize (TST.px size)
        , fontFamily [ "Helvetica", "sans-serif" ]
        , fill <| TST.Paint Color.white
        , noSelect
        ]
        [ TSC.text string ]


onEnter : msg -> El.Attribute msg
onEnter msg =
    El.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
