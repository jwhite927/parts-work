module Page.Login exposing (view)

import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Email
import Style
import Types exposing (..)
import Utility exposing (..)



-- TODO Guard against account already exists
-- VIEW


view : FrontendModel -> List (El.Element FrontendMsg)
view model =
    [ viewLogin model ]


viewLogin : FrontendModel -> El.Element FrontendMsg
viewLogin model =
    El.column [ El.centerX, El.padding 20, El.spacing 20 ]
        ([]
            |> withActivityText model
            |> withErrorText model
            |> withEmailField model
            |> withPasswordField model
            |> withSubmitButton model
            |> withToggleText model
            |> withDevLogin
            |> pushBottomBarAway
        )


withDevLogin elements =
    elements
        ++ [ Input.button
                [ Background.color Style.offLavender
                , El.mouseOver [ Background.color Style.offLavender ]
                , Font.size 15
                , Font.color <| El.rgb255 255 255 255
                , Border.rounded 3
                , El.centerX
                ]
                { onPress = Just DevLogin
                , label =
                    El.el
                        [ El.paddingXY 16 7
                        , Font.light
                        ]
                        (El.text "DevLogin")
                }
           ]


withActivityText : FrontendModel -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withActivityText model elements =
    let
        text =
            case model.loginMode of
                SigningIn ->
                    "Log In"

                SigningUp ->
                    "Sign Up"
    in
    elements
        ++ [ El.el
                [ Font.size 32, El.centerX ]
                (El.text text)
           ]


withErrorText : FrontendModel -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withErrorText model elements =
    let
        style =
            [ Font.size 18, El.centerX, Font.color Style.red ]
    in
    case model.error of
        WrongPassword ->
            elements
                ++ [ El.el
                        style
                        (El.text "Password incorrect, please try again.")
                   ]

        UnknownUser ->
            elements
                ++ [ El.el
                        style
                        (El.text "Username not recognized, please try again or register.")
                   ]

        _ ->
            elements


withEmailField : FrontendModel -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withEmailField model elements =
    let
        text =
            if model.loginErrors.validEmail then
                Input.labelAbove [ Font.color Style.red ] <| El.text "Email - Please enter a valid loginEmail"

            else
                Input.labelAbove [] <| El.text "Email"
    in
    elements
        ++ [ Input.email
                [ Font.size 18 ]
                { onChange = TypedUsername
                , text = model.loginEmail
                , placeholder = Nothing
                , label = text
                }
           ]


withPasswordField : FrontendModel -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withPasswordField model elements =
    case model.loginMode of
        SigningIn ->
            withCurrentPasswordField model elements

        SigningUp ->
            withNewPasswordFields model elements


withCurrentPasswordField : FrontendModel -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withCurrentPasswordField model elements =
    let
        text =
            if model.loginErrors.currPass then
                Input.labelAbove [ Font.color Style.red ] <| El.text "Password - Password cannot be empty"

            else
                Input.labelAbove [] <| El.text "Password"
    in
    elements
        ++ [ Input.currentPassword
                [ Font.size 18 ]
                { onChange = TypedCurrPass
                , text = model.loginCurrPass
                , placeholder = Maybe.Nothing
                , show = False
                , label = text
                }
           ]


withNewPasswordFields : FrontendModel -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withNewPasswordFields model elements =
    let
        text =
            if model.loginErrors.newPassMatch then
                Input.labelAbove [ Font.color Style.red ] <| El.text "Confirm Password - Passwords Must match"

            else
                Input.labelAbove [] <| El.text "Confirm Password"
    in
    elements
        ++ [ Input.newPassword
                [ Font.size 18 ]
                { onChange = TypedNewPass1
                , text = model.loginNewPass1
                , placeholder = Maybe.Nothing
                , show = False
                , label =
                    Input.labelAbove
                        []
                    <|
                        El.text "New Password"
                }
           , Input.newPassword
                [ Font.size 18 ]
                { onChange = TypedNewPass2
                , text = model.loginNewPass2
                , placeholder = Maybe.Nothing
                , show = False
                , label = text
                }
           ]


withSubmitButton : FrontendModel -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withSubmitButton model elements =
    elements
        ++ [ Input.button
                [ Background.color Style.offLavender
                , El.mouseOver [ Background.color Style.offLavender ]
                , Font.size 15
                , Font.color <| El.rgb255 255 255 255
                , Border.rounded 3
                , El.centerX
                ]
                { onPress = handleSubmit model
                , label =
                    El.el
                        [ El.paddingXY 16 7
                        , Font.light
                        ]
                        (El.text "Submit")
                }
           ]


withToggleText : FrontendModel -> List (El.Element FrontendMsg) -> List (El.Element FrontendMsg)
withToggleText model elements =
    let
        text =
            case model.loginMode of
                SigningIn ->
                    "Don't have an account? Sign up instead."

                SigningUp ->
                    "Already have an account? Log in instead."

        otherMode =
            case model.loginMode of
                SigningIn ->
                    SigningUp

                SigningUp ->
                    SigningIn
    in
    elements
        ++ [ Input.button
                [ El.mouseOver [ Font.color Style.blue ]
                , Font.size 12
                , Font.color Style.darkBlue

                -- , Border.rounded 3
                , El.centerX
                ]
                { onPress = Just <| ChangeMode otherMode
                , label =
                    El.el
                        [ El.paddingXY 16 7
                        , Font.light
                        ]
                        (El.text text)
                }
           ]


handleSubmit : FrontendModel -> Maybe FrontendMsg
handleSubmit model =
    let
        loginErrors =
            { validEmail = not <| isValidEmail model.loginEmail
            , currPass = String.isEmpty model.loginCurrPass
            , newPassMatch = model.loginNewPass1 /= model.loginNewPass2
            }
    in
    case model.loginMode of
        SigningUp ->
            if loginErrors.validEmail || loginErrors.newPassMatch then
                Just <| SubmitErrors loginErrors

            else
                Just SubmitNewUser

        SigningIn ->
            if loginErrors.validEmail || loginErrors.currPass then
                Just <| SubmitErrors loginErrors

            else
                Just SubmitLogin


isValidEmail : String -> Bool
isValidEmail loginEmail =
    case Email.fromString loginEmail of
        Just _ ->
            True

        Nothing ->
            False
