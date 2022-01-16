module Backend exposing (..)

import Dict exposing (Dict)
import Env
import Lamdera exposing (ClientId, SessionId)
import Random as Rnd
import Random.Char exposing (english)
import Random.String as RndStr
import SHA256 as Sha
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { users = Dict.empty, nextGuid = Nothing }
    , generateGuid Nothing
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        GotNextGuid user guid ->
            case user of
                Just entry ->
                    ( { model | nextGuid = Just guid }, Lamdera.sendToBackend (UserRegistered entry) )

                Nothing ->
                    ( { model | nextGuid = Just guid }, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UserRegistered user ->
            case model.nextGuid of
                Just guid ->
                    let
                        newUser =
                            createUser guid user
                    in
                    ( { model | users = Dict.insert user.email newUser model.users }
                    , Cmd.batch
                        [ generateGuid Nothing
                        , Lamdera.sendToFrontend clientId <| GotUser (userToFrontend newUser) Dict.empty
                        ]
                    )

                Nothing ->
                    ( model, generateGuid <| Just user )

        UserLoggedIn user ->
            case Dict.get user.email model.users of
                Just entry ->
                    if entry.password == saltedPassword user.password entry.salt then
                        ( model, Lamdera.sendToFrontend clientId <| GotUser (userToFrontend entry) entry.parts )

                    else
                        ( model, Lamdera.sendToFrontend clientId <| Problem WrongPassword )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId <| Problem UnknownUser )

        SaveParts email parts ->
            case Dict.get email model.users of
                Just entry ->
                    ( { model
                        | users =
                            Dict.insert
                                email
                                (userWithParts entry parts)
                                model.users
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId <| Problem UnknownUser )

        NoOpToBackend ->
            ( model, Cmd.none )


createUser : String -> { email : String, password : String } -> BackendUser
createUser guid entry =
    let
        salt =
            guid
                |> Sha.fromString
                |> Sha.toBase64
    in
    { email = entry.email
    , password = saltedPassword entry.password salt
    , salt = salt
    , isAdmin = checkAdmin entry.email
    , parts = Dict.empty
    }


userWithParts : BackendUser -> Dict PartName Part -> BackendUser
userWithParts user parts =
    { user | parts = parts }


saltedPassword : String -> String -> String
saltedPassword password saltBase64 =
    password
        ++ saltBase64
        |> Sha.fromString
        |> Sha.toBase64


checkAdmin : String -> Bool
checkAdmin email =
    List.member email Env.adminEmails


guidGenerator : Rnd.Generator String
guidGenerator =
    RndStr.string 36 english


generateGuid : Maybe { email : String, password : String } -> Cmd BackendMsg
generateGuid next =
    Rnd.generate (GotNextGuid next) guidGenerator


userToFrontend : BackendUser -> FrontendUser
userToFrontend user =
    { email = user.email
    , isAdmin = user.isAdmin
    }
