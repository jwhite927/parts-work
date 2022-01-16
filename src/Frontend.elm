module Frontend exposing (app)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element as El
import Env
import Lamdera
import Math.Vector2 as Vec
import Skeleton
import Types exposing (..)
import Url
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)


app =
    Lamdera.frontend
        { init = init
        , view = view
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


subscriptions : a -> Sub msg
subscriptions _ =
    Sub.none


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    stepUrl url
        { key = key
        , page = Ifs
        , returnPage = Ifs
        , user = Nothing
        , loginEmail = ""
        , loginCurrPass = ""
        , loginNewPass1 = ""
        , loginNewPass2 = ""
        , loginMode = SigningIn
        , loginErrors = emptyErrors
        , ifsParts =
            Dict.fromList
                [ ( "Joyful", Part { x = 200, y = 500 } "Joyful" 40 )
                , ( "Angry", Part { x = 500, y = 200 } "Angry" 40 )
                ]
        , ifsMode = EditParts Normal
        , ifsNewPartName = ""
        , ifsEditing = Nothing
        , ifsEditTarget = Nothing
        , ifsView = { x = 0, y = 0, w = ifsInitViewW, h = ifsInitViewH }
        , error = None
        }


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title =
        getTitle model.page
    , body =
        [ El.layout [] <| Skeleton.view model ]
    }


getTitle : Page -> String
getTitle page =
    case page of
        Ifs ->
            "Animind - Parts Work"

        NotFound ->
            "Animind - Not Found"

        Login ->
            "Animind - Log In"


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update message model =
    case message of
        NoOpFrontendMsg ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        TypedUsername s ->
            ( { model | loginEmail = s }, Cmd.none )

        TypedCurrPass s ->
            ( { model | loginCurrPass = s }, Cmd.none )

        TypedNewPass1 s ->
            ( { model | loginNewPass1 = s }, Cmd.none )

        TypedNewPass2 s ->
            ( { model | loginNewPass2 = s }, Cmd.none )

        ChangeMode s ->
            ( { model | loginMode = s }, Cmd.none )

        SubmitLogin ->
            ( { model
                | loginEmail = ""
                , loginCurrPass = ""
                , loginErrors = emptyErrors
              }
            , UserLoggedIn { email = model.loginEmail, password = model.loginCurrPass }
                |> Lamdera.sendToBackend
            )

        DevLogin ->
            ( { model
                | loginEmail = ""
                , loginCurrPass = ""
                , loginErrors = emptyErrors
              }
            , UserLoggedIn Env.devLogin
                |> Lamdera.sendToBackend
            )

        SubmitNewUser ->
            ( { model
                | loginEmail = ""
                , loginCurrPass = ""
                , loginNewPass1 = ""
                , loginNewPass2 = ""
                , loginErrors = emptyErrors
              }
            , UserRegistered { email = model.loginEmail, password = model.loginNewPass1 }
                |> Lamdera.sendToBackend
            )

        SubmitErrors loginErrors ->
            ( { model | loginErrors = loginErrors }, Cmd.none )

        UpdateNewPartName text ->
            ( { model | ifsNewPartName = text }, Cmd.none )

        SelectIfsMode mode ->
            ( { model | ifsMode = mode }, Cmd.none )

        GotNewPart ->
            if model.ifsNewPartName == "" then
                ( model, Cmd.none )

            else
                let
                    newParts =
                        Dict.insert
                            model.ifsNewPartName
                            (Part { x = 500, y = 500 } model.ifsNewPartName 40)
                            model.ifsParts
                in
                ( { model
                    | ifsParts = newParts
                    , ifsNewPartName = ""
                    , ifsMode = EditParts Normal
                  }
                , maybeSaveParts model
                )

        StartMovingPart name ->
            ( { model | ifsMode = EditParts MovingPart, ifsEditing = Dict.get name model.ifsParts }, Cmd.none )

        StartResizingPart name ->
            ( { model | ifsMode = EditParts ResizePart, ifsEditing = Dict.get name model.ifsParts }, Cmd.none )

        NewPartPos xy ->
            case model.ifsEditing of
                Just part ->
                    let
                        newX =
                            (xy.x * model.ifsView.w / ifsInitViewW) + model.ifsView.x

                        newY =
                            (xy.y * model.ifsView.h / ifsInitViewH) + model.ifsView.y

                        newPos =
                            withPos part { x = newX, y = newY }
                    in
                    if inParts model newPos then
                        ( model, Cmd.none )

                    else
                        ( { model | ifsEditTarget = Just newPos }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewPartSize x ->
            case model.ifsEditing of
                Just part ->
                    let
                        newX =
                            (x * model.ifsView.w / ifsInitViewW) + model.ifsView.x

                        newRadius =
                            withR part <| abs <| newX - part.xy.x
                    in
                    if inParts model newRadius then
                        ( model, Cmd.none )

                    else
                        ( { model | ifsEditTarget = Just newRadius }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewScrollPos y ->
            ( model, Cmd.none )

        EndEditPart ->
            ( resolvePartEdit model, maybeSaveParts model )

        SaveLayout ->
            ( model, Cmd.none )

        RedirectToLoginFrom page ->
            ( { model | returnPage = page, page = Login }, Cmd.none )

        ChangeView move ->
            case move of
                ZoomIn ->
                    ( { model | ifsView = zoomIn model.ifsView }, Cmd.none )

                ZoomOut ->
                    ( { model | ifsView = zoomOut model.ifsView }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


emptyPart : Part
emptyPart =
    Part { x = 5, y = 5 } "" 0


whZoomAmount : Float
whZoomAmount =
    1.5


zoomIn : IfsView -> IfsView
zoomIn box =
    let
        newW =
            box.w / whZoomAmount

        newH =
            box.h / whZoomAmount

        xDiff =
            findDiff box.w newW

        yDiff =
            findDiff box.h newH
    in
    { x = box.x + xDiff, y = box.y + yDiff, w = newW, h = newH }


zoomOut : IfsView -> IfsView
zoomOut box =
    let
        newW =
            box.w * whZoomAmount

        newH =
            box.h * whZoomAmount

        xDiff =
            findDiff box.w newW

        yDiff =
            findDiff box.h newH
    in
    { x = box.x - xDiff, y = box.y - yDiff, w = newW, h = newH }


findDiff : Float -> Float -> Float
findDiff current new =
    abs (current - new) / 2


maybeSaveParts : FrontendModel -> Cmd FrontendMsg
maybeSaveParts model =
    case model.user of
        Just user ->
            Lamdera.sendToBackend <| SaveParts user.email model.ifsParts

        Nothing ->
            Cmd.none



-- ROUTER


stepUrl : Url.Url -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
stepUrl url model =
    let
        parser =
            oneOf
                [ route top
                    ( { model | page = Ifs }, Cmd.none )
                , route (s "login")
                    ( { model | page = Login, returnPage = model.page }, Cmd.none )
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        GotUser user parts ->
            ( { model | user = Just user, page = model.returnPage, ifsParts = parts }, Cmd.none )

        Problem error ->
            ( { model | error = error }, Cmd.none )


resolvePartEdit : FrontendModel -> FrontendModel
resolvePartEdit model =
    { model
        | ifsEditing = Nothing
        , ifsMode = EditParts Normal
        , ifsEditTarget = Nothing
        , ifsParts =
            case model.ifsEditTarget of
                Just target ->
                    Dict.insert target.name
                        target
                        model.ifsParts

                Nothing ->
                    model.ifsParts
    }


inParts : FrontendModel -> Part -> Bool
inParts model part =
    model.ifsParts
        |> Dict.values
        |> List.filter (\x -> x.name /= (Maybe.withDefault emptyPart model.ifsEditing).name)
        |> List.any (\x -> insidePart x part)


insidePart : Part -> Part -> Bool
insidePart first second =
    Vec.distance (Vec.fromRecord first.xy) (Vec.fromRecord second.xy) < (first.r + second.r + 1)
