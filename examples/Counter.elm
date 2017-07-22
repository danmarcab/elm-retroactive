module Counter exposing (main)

import Html exposing (div, text, button)
import Html.Events exposing (onClick)
import Retroactive


type alias Model =
    Int


type Op
    = Inc
    | Dec


type Msg
    = Incr
    | Decr
    | Undo
    | Redo


inc =
    Retroactive.operation Inc (\i -> i + 1) (\i -> i - 1)


dec =
    Retroactive.operation Inc (\i -> i - 1) (\i -> i + 1)


init =
    ( 1, Cmd.none )


view model =
    div []
        [ div []
            [ button [ onClick Incr ] [ text "+1" ]
            , button [ onClick Decr ] [ text "-1" ]
            , button [ onClick Undo ] [ text "Undo" ]
            , button [ onClick Redo ] [ text "Redo" ]
            ]
        , div [] [ text <| toString model ]
        ]


update msg model =
    case msg of
        Incr ->
            ( Retroactive.Do inc, Cmd.none )

        Decr ->
            ( Retroactive.Do dec, Cmd.none )

        Undo ->
            ( Retroactive.Undo, Cmd.none )

        Redo ->
            ( Retroactive.Redo, Cmd.none )


subscriptions model =
    Sub.none


main : Program Never (Retroactive.History Op Model) Msg
main =
    Retroactive.program { init = init, update = update, view = view, subscriptions = subscriptions }
