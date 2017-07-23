module CounterNoProgram exposing (main)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Retroactive exposing (Action, History, HistoryOptions, Operation)


type alias Model =
    { counter : History Op Counter
    }


type alias Counter =
    Int


type Op
    = Inc
    | Dec
    | GoTo7


type Msg
    = CounterMsg CounterMsg


type CounterMsg
    = Do Op
    | Undo
    | Redo Op


inc : Operation Op Counter
inc =
    Retroactive.operation Inc (\i -> i + 1) (\i -> i - 1)


dec : Operation Op Counter
dec =
    Retroactive.operation Dec (\i -> i - 1) (\i -> i + 1)


goTo7 : Counter -> Operation Op Counter
goTo7 m =
    Retroactive.operation GoTo7 (\i -> 7) (\i -> m)


init : ( Model, Cmd Msg )
init =
    ( { counter = Retroactive.init 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.map CounterMsg <| Retroactive.view counterView model.counter


counterView : HistoryOptions Op -> Counter -> Html CounterMsg
counterView ({ undo, redo } as options) counter =
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
            , div [] [ text <| toString counter ]
            , div [] [ text <| toString options ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg subMsg ->
            let
                ( newCounter, cmd ) =
                    Retroactive.update counterUpdate subMsg model.counter
            in
                ( { model | counter = newCounter }, Cmd.map CounterMsg cmd )


counterUpdate : HistoryOptions Op -> CounterMsg -> Counter -> ( Action Op Counter, Cmd CounterMsg )
counterUpdate options msg counter =
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
                            goTo7 counter
            in
                ( Retroactive.Do operation, Cmd.none )

        Undo ->
            ( Retroactive.Undo, Cmd.none )

        Redo o ->
            ( Retroactive.Redo o, Cmd.none )


subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }
