module Retroactive exposing (Action(..), Operation, operation, History, HistoryOptions, program)

import Html
import Graph exposing (Graph)
import Set exposing (Set)


type Action o m
    = Do (Operation o m)
    | Undo
    | Redo o


type Operation o m
    = Operation (RealOperation o m)
    | Init


type alias RealOperation o m =
    { kind : o
    , do : m -> m
    , undo : m -> m
    }


type alias HistoryOptions o =
    { undo : Maybe o
    , redo : List o
    }


operation : o -> (m -> m) -> (m -> m) -> Operation o m
operation kind do undo =
    Operation
        { kind = kind
        , do = do
        , undo = undo
        }


type History o m
    = History
        { operations : Graph Int (Operation o m) ()
        , lastOperation : Int
        , currentModel : m
        , nextId : Int
        }


program :
    { init : ( m, Cmd msg )
    , update : msg -> m -> ( Action o m, Cmd msg )
    , view : HistoryOptions o -> m -> Html.Html msg
    , subscriptions : m -> Sub msg
    }
    -> Program Never (History o m) msg
program guest =
    Html.program
        { init = init guest.init
        , update = update guest.update
        , view = view guest.view
        , subscriptions = subscriptions guest.subscriptions
        }


init : ( m, Cmd msg ) -> ( History o m, Cmd msg )
init ( model, cmd ) =
    ( History
        { operations = Graph.insertData 0 Init Graph.empty
        , lastOperation = 0
        , currentModel = model
        , nextId = 1
        }
    , cmd
    )


update : (msg -> m -> ( Action o m, Cmd msg )) -> msg -> History o m -> ( History o m, Cmd msg )
update updater msg (History h) =
    let
        ( action, cmd ) =
            updater msg h.currentModel

        newHistory =
            case action of
                Do (Operation op) ->
                    perform op (History h)

                Do Init ->
                    Tuple.first <| init ( h.currentModel, cmd )

                Undo ->
                    undo (History h)

                Redo o ->
                    redo o (History h)
    in
        ( newHistory, cmd )


view : (HistoryOptions o -> m -> Html.Html msg) -> History o m -> Html.Html msg
view viewer (History h) =
    viewer (historyOptions (History h)) h.currentModel


subscriptions : (m -> Sub msg) -> History o m -> Sub msg
subscriptions subscriber (History h) =
    subscriber h.currentModel


perform : RealOperation o m -> History o m -> History o m
perform op (History { operations, lastOperation, currentModel, nextId }) =
    let
        outgoingOps =
            Graph.outgoing lastOperation operations
                |> Set.toList
                |> List.map (\id -> ( id, Graph.getData id operations ))
                |> List.filter (\( id, maybeOp ) -> Maybe.andThen getKind maybeOp == Just op.kind)

        ( newOperations, newLastOp, newNextId ) =
            case outgoingOps of
                [ ( id, Just _ ) ] ->
                    ( operations, id, nextId )

                _ ->
                    let
                        ops =
                            Graph.insertData nextId (Operation op) operations
                                |> Graph.insertEdge lastOperation nextId
                    in
                        ( ops, nextId, nextId + 1 )
    in
        History
            { operations = newOperations
            , lastOperation = newLastOp
            , currentModel = op.do currentModel
            , nextId = newNextId
            }


undo : History o m -> History o m
undo (History h) =
    let
        maybeLastOp =
            Graph.getData h.lastOperation h.operations

        incoming =
            Graph.incoming h.lastOperation h.operations
                |> Set.toList

        newLastOp =
            case incoming of
                [ id ] ->
                    id

                _ ->
                    h.lastOperation
    in
        case maybeLastOp of
            Just (Operation op) ->
                History { h | lastOperation = newLastOp, currentModel = op.undo h.currentModel }

            Just Init ->
                History h

            Nothing ->
                History h


redo : o -> History o m -> History o m
redo opId (History h) =
    let
        outgoingOps =
            Graph.outgoing h.lastOperation h.operations
                |> Set.toList
                |> List.map (\id -> ( id, Graph.getData id h.operations ))
                |> List.filter (\( id, maybeOp ) -> Maybe.andThen getKind maybeOp == Just opId)
    in
        case outgoingOps of
            [ ( id, Just (Operation op) ) ] ->
                History { h | lastOperation = id, currentModel = op.do h.currentModel }

            _ ->
                History h


historyOptions : History o m -> HistoryOptions o
historyOptions (History h) =
    let
        maybeUndo =
            Graph.getData h.lastOperation h.operations
                |> Maybe.andThen getKind

        redoOps =
            Graph.outgoing h.lastOperation h.operations
                |> Set.toList
                |> List.filterMap (\id -> Graph.getData id h.operations)
                |> List.filterMap getKind
    in
        { undo = maybeUndo, redo = redoOps }


getKind : Operation o m -> Maybe o
getKind op =
    case op of
        Operation operation ->
            Just operation.kind

        Init ->
            Nothing
