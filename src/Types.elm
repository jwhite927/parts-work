module Types exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2)
import Url


type alias FrontendModel =
    { key : Nav.Key
    , page : Page
    , returnPage : Page
    , user : Maybe FrontendUser
    , loginEmail : String
    , loginCurrPass : String
    , loginNewPass1 : String
    , loginNewPass2 : String
    , loginMode : LoginMode
    , loginErrors : FormErrors
    , ifsParts : Dict PartName Part
    , ifsMode : IfsMode
    , ifsNewPartName : String
    , ifsEditing : Maybe Part
    , ifsEditTarget : Maybe Part
    , ifsView: IfsView
    , error : LoginError
    }


emptyErrors : FormErrors
emptyErrors =
    { validEmail = False
    , currPass = False
    , newPassMatch = False
    }


type Page
    = NotFound
    | Ifs
    | Login


type alias FormErrors =
    { validEmail : Bool
    , currPass : Bool
    , newPassMatch : Bool
    }


type LoginError
    = None
    | WrongPassword
    | UnknownUser


type LoginMode
    = SigningIn
    | SigningUp


type IfsMode
    = EditParts IfsEditPartsMode
    | ViewMode


type IfsEditPartsMode
    = Normal
    | CreatingPart
    | MovingPart
    | ResizePart

type alias PartName =
    String


type alias Part =
    { xy : Position
    , name : PartName
    , r : Float
    }


type alias Position =
    { x : Float, y : Float }

type alias IfsView =
    { x : Float
    , y : Float, w: Float, h: Float }

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


type alias BackendModel =
    { users : Dict String BackendUser
    , nextGuid : Maybe String
    }


type BackendMsg
    = NoOpBackendMsg
    | GotNextGuid (Maybe { email : String, password : String }) String


type ToFrontend
    = NoOpToFrontend
    | GotUser FrontendUser (Dict PartName Part)
    | Problem LoginError


type ToBackend
    = NoOpToBackend
    | UserLoggedIn { email : String, password : String }
    | UserRegistered { email : String, password : String }
    | SaveParts String (Dict PartName Part)


type alias FrontendUser =
    { email : String
    , isAdmin : Bool
    }


type alias BackendUser =
    { email : String
    , password : String
    , salt : String
    , isAdmin : Bool
    , parts : Dict PartName Part
    }


withPos part xy =
    Part xy part.name part.r

withR part r =
    Part part.xy part.name r

ifsInitViewW =
    800

ifsInitViewH =
    600
