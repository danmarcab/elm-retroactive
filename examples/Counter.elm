module Counter exposing (main)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Retroactive exposing (Action, History, HistoryOptions, Operation)


type alias Model =
    Int


type Op
    = Inc
    | Dec
    | GoTo7


type Msg
    = Do Op
    | Undo
    | Redo Op


inc : Operation Op Model
inc =
    Retroactive.operation Inc (\i -> i + 1) (\i -> i - 1)


dec : Operation Op Model
dec =
    Retroactive.operation Dec (\i -> i - 1) (\i -> i + 1)


goTo7 : Model -> Operation Op Model
goTo7 m =
    Retroactive.operation GoTo7 (\i -> 7) (\i -> m)


init : ( Model, Cmd Msg )
init =
    ( 1, Cmd.none )


view : HistoryOptions Op -> Model -> Html Msg
view ({ undo, redo } as options) model =
    let
        actionButtons =
            [ button [ onClick <| Do Inc ] [ text "+1" ]
            , button [ onClick <| Do Dec ] [ text "-1" ]
            , button [ onClick <| Do GoTo7 ] [ text "Go to 7" ]
            ]

        redoButtons =
            List.map (\o -> button [ onClick <| Redo o ] [ text <| "Redo: " ++ toString o ]) redo

        historyButtons =
            case undo of
                Just o ->
                    (button [ onClick Undo ] [ text <| "Undo: " ++ toString o ]) :: redoButtons

                Nothing ->
                    redoButtons
    in
        div []
            [ div []
                (actionButtons ++ historyButtons)
            , div [] [ text <| toString model ]
            , div [] [ text <| toString options ]
            ]


update : HistoryOptions Op -> Msg -> Model -> ( Action Op Model, Cmd Msg )
update options msg model =
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

        Redo o ->
            ( Retroactive.Redo o, Cmd.none )


subscriptions model =
    Sub.none


main : Program Never (History Op Model) Msg
main =
    Retroactive.program { init = init, update = update, view = view, subscriptions = subscriptions }
