module Counter exposing (main)

import Html exposing (div, text, button)
import Html.Events exposing (onClick)
import Retroactive


type alias Model =
    Int


type Op
    = Inc
    | Dec
    | GoTo7


type Msg
    = Do Op
    | Undo
    | Redo


inc =
    Retroactive.operation Inc (\i -> i + 1) (\i -> i - 1)


dec =
    Retroactive.operation Inc (\i -> i - 1) (\i -> i + 1)


goTo7 m =
    Retroactive.operation Inc (\i -> 7) (\i -> m)


init =
    ( 1, Cmd.none )


view model =
    div []
        [ div []
            [ button [ onClick <| Do Inc ] [ text "+1" ]
            , button [ onClick <| Do Dec ] [ text "-1" ]
            , button [ onClick <| Do GoTo7 ] [ text "Go to 7" ]
            , button [ onClick Undo ] [ text "Undo" ]
            , button [ onClick Redo ] [ text "Redo" ]
            ]
        , div [] [ text <| toString model ]
        ]


update msg model =
    case msg of
        Do op ->
            let
                operation =
                    case op of
                        Inc ->
                            inc

                        Dec ->
                            dec

                        GoTo7 ->
                            goTo7 model
            in
                ( Retroactive.Do operation, Cmd.none )

        Undo ->
            ( Retroactive.Undo, Cmd.none )

        Redo ->
            ( Retroactive.Redo, Cmd.none )


subscriptions model =
    Sub.none


main : Program Never (Retroactive.History Op Model) Msg
main =
    Retroactive.program { init = init, update = update, view = view, subscriptions = subscriptions }
