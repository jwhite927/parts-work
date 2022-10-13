module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Url


type Page
    = NotFound
    | Ifs
    | Login


type alias FrontendUser =
    { email : String
    , isAdmin : Bool
    }


type LoginMode
    = SigningIn
    | SigningUp


type alias FormErrors =
    { validEmail : Bool
    , currPass : Bool
    , newPassMatch : Bool
    }


type alias PartName =
    String


type alias Position =
    { x : Float
    , y : Float
    }


type alias Part =
    { xy : Position
    , name : PartName
    , r : Float
    }


type IfsEditPartsMode
    = Normal
    | CreatingPart
    | MovingPart
    | ResizePart


type IfsMode
    = EditParts IfsEditPartsMode
    | ViewMode


type alias IfsView =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type LoginError
    = None
    | WrongPassword
    | UnknownUser


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    , returnPage : Page
    , user : Maybe FrontendUser
    , loginEmail : String
    , loginCurrPass : String
    , loginNewPass1 : String
    , loginNewPass2 : String
    , loginMode : LoginMode
    , loginErrors : FormErrors
    , ifsParts : Dict.Dict PartName Part
    , ifsMode : IfsMode
    , ifsNewPartName : String
    , ifsEditing : Maybe Part
    , ifsEditTarget : Maybe Part
    , ifsView : IfsView
    , error : LoginError
    }


type alias BackendUser =
    { email : String
    , password : String
    , salt : String
    , isAdmin : Bool
    , parts : Dict.Dict PartName Part
    }


type alias BackendModel =
    { users : Dict.Dict String BackendUser
    , nextGuid : Maybe String
    }


type ViewChange
    = ZoomIn
    | ZoomOut
    | PanLeft


type FrontendMsg
    = NoOpFrontendMsg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TypedUsername String
    | TypedCurrPass String
    | TypedNewPass1 String
    | TypedNewPass2 String
    | ChangeMode LoginMode
    | SubmitLogin
    | SubmitNewUser
    | SubmitErrors FormErrors
    | GotNewPart
    | SelectIfsMode IfsMode
    | UpdateNewPartName String
    | SaveLayout
    | StartMovingPart PartName
    | StartResizingPart PartName
    | NewPartPos Position
    | NewPartSize Float
    | NewScrollPos Float
    | EndEditPart
    | DevLogin
    | RedirectToLoginFrom Page
    | ChangeView ViewChange


type ToBackend
    = NoOpToBackend
    | UserLoggedIn
        { email : String
        , password : String
        }
    | UserRegistered
        { email : String
        , password : String
        }
    | SaveParts String (Dict.Dict PartName Part)


type BackendMsg
    = NoOpBackendMsg
    | GotNextGuid
        (Maybe
            { email : String
            , password : String
            }
        )
        String


type ToFrontend
    = NoOpToFrontend
    | GotUser FrontendUser (Dict.Dict PartName Part)
    | Problem LoginError
