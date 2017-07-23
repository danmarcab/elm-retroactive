module Retroactive
    exposing
        ( Action(..)
        , Operation
        , operation
        , History
        , HistoryOptions
        , program
        , programWithFlags
        , init
        , update
        , view
        , subscriptions
        )

{-| This library lets you have a retroactive model. This means you can undo / redo
changes in your model. Retroactive means you can have different branches of future (redo).

The library can be use as a [`Program`](#using-program), so all your model will be retroactive, or
[`not using Program`](#not-using-program), where you decide what parts of your model you want to make retroactive.

# Actions and Operations

This library forces you to do all updates to your model through actions (see [`Action`](#Action)).
This means the only way of updating your model is to return an [`Action`](#Action) in your `update` function.

@docs Action, Operation, operation

# History and HistoryOptions
@docs History, HistoryOptions

# Using program

The easiest way to use this library is to use [`program`](#program). You then provide
your `init`, `update`, `view` and `subscriptions` as you would do with `Html.Program`.

Note that there are some differences, for more info check [`program`](#program).

You can check `Counter.elm` for an example.

@docs program, programWithFlags

# Not using program

If you don't want to use [`program`](#program), you can also follow the next steps to
integrate it your existing app.

- Store in your model a `[`program`](#program) Op SubModel`, being `SubModel` the part of your model you want to make retroactive
- Use [`init`](#init), [`update`](#update), [`view`](#view), [`subscriptions`](#subscriptions) of this module when you need to deal with `SubModel`

This usually means you end up building `initSubModel`, `updateSubModel` and so on, and use them like:

    type alias Model =
        { subModel : History Op SubModel
        }

    updateSubModel historyOptions msg subModel =
        -- Some update that returns (Action Op SubModel, Cmd msg)

    update msg model =
        case msg of
            SubModelMsg subMsg ->
                let
                    (newSubModel, cmd) =
                        Retroactive.update subModelUpdate subMsg model.subModel
                in
                    ({model | subModel = newSubModel}, Cmd.map SubModelMsg cmd)

    subModelView historyOptions subModel =
        -- Some view

    view model =
        Html.map SubModelMsg <| Retroactive.view subModelView model.subModel

You can check `CounterNoProgram.elm` for an example.

@docs init, update, view, subscriptions

-}

import Html exposing (Html)
import Graph exposing (Graph)
import Set exposing (Set)


{-| Represents an action on your model. It can be:

- `Do operation`: performs the [`Operation`](#Operation) `operation`.
- `Undo`: undoes the last operation.
- `Redo opId`: redoes the operation with identifier `opId`.

    type Op
        = Inc
        | Dec

    type alias Model = Int

    inc =
        operation Inc (\m -> m + 1) (\m -> m - 1)
    dec =
        operation Dec (\m -> m 1 1) (\m -> m + 1)

    update historyOptions msg model =
        case msg of
            Msg1 ->
                (Do inc, Cmd.none)
            UndoMsg ->
                (Undo, Cmd.none)
            RedoMsg op ->
                (Redo op, Cmd.none)
-}
type Action op model
    = Do (Operation op model)
    | Undo
    | Redo op


{-| Represents an operation on your model. It will carry information to know how to do / undo the operation.

To create operations you use [`operation`](#operation).
-}
type Operation op model
    = Operation (RealOperation op model)
    | Init


type alias RealOperation op model =
    { kind : op
    , do : model -> model
    , undo : model -> model
    }


{-| Represents the valid operations you can undo / redo on your model.
It will be passed to your `update` and `view` functions.

You can use this information to only show / try to perform the valid operations.

There is at most one undo operation, but there can be multiple redo operations.
This is because you can undo to a certain point, and then start another branch to the future.
-}
type alias HistoryOptions op =
    { undo : Maybe op
    , redo : List op
    }


{-| Creates an [`Operation`](#Operation) for your model. You need to provide a value of a custom type as identifier, a function to
perform the operation in your model, and a function to undo the operation.

    type Op
        = Inc
        | Dec

    type alias Model = Int

    operation Inc (\m -> m + 1) (\m -> m - 1)
    operation Dec (\m -> m 1 1) (\m -> m + 1)

-}
operation : op -> (model -> model) -> (model -> model) -> Operation op model
operation kind do undo =
    Operation
        { kind = kind
        , do = do
        , undo = undo
        }


{-| Stores the History of your model.
-}
type History op model
    = History
        { operations : Graph Int (Operation op model) ()
        , lastOperation : Int
        , currentModel : model
        , nextId : Int
        }


{-| Creates a program using Retroactive. Note that the signature of the provided `update` and `view` are different
from [`Html.program`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#program)

The first difference is your `update` function needs to return [`Action`](#Action) instead of the usual `Model`.

The second difference is your `update` and `view` functions will receive a [`HistoryOptions`](#HistoryOptions) as first
parameter.
-}
program :
    { init : ( model, Cmd msg )
    , update : HistoryOptions op -> msg -> model -> ( Action op model, Cmd msg )
    , view : HistoryOptions op -> model -> Html msg
    , subscriptions : model -> Sub msg
    }
    -> Program Never (History op model) msg
program guest =
    Html.program
        { init = Tuple.mapFirst init guest.init
        , update = update guest.update
        , view = view guest.view
        , subscriptions = subscriptions guest.subscriptions
        }


{-| Works the same as [`program`](#program), but it can also handle flags. See
[`Html.programWithFlags`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html#programWithFlags)
for more information.
-}
programWithFlags :
    { init : flags -> ( model, Cmd msg )
    , update : HistoryOptions op -> msg -> model -> ( Action op model, Cmd msg )
    , view : HistoryOptions op -> model -> Html msg
    , subscriptions : model -> Sub msg
    }
    -> Program flags (History op model) msg
programWithFlags guest =
    Html.programWithFlags
        { init = (\flags -> Tuple.mapFirst init (guest.init flags))
        , update = update guest.update
        , view = view guest.view
        , subscriptions = subscriptions guest.subscriptions
        }


{-| Initializes a [`History`](#History) from the initial model.

You should only need this if you are using the library [`not using program`](#not-using-program)
-}
init : model -> History op model
init model =
    History
        { operations = Graph.insertData 0 Init Graph.empty
        , lastOperation = 0
        , currentModel = model
        , nextId = 1
        }


{-| Updates a [`History`](#History).

You pass your `update` as first parameter, A [`HistoryOptions`](#HistoryOptions) will be passed to your `update`.

You should only need this if you are using the library [`not using program`](#not-using-program)
-}
update : (HistoryOptions op -> msg -> model -> ( Action op model, Cmd msg )) -> msg -> History op model -> ( History op model, Cmd msg )
update updater msg (History h) =
    let
        ( action, cmd ) =
            updater (historyOptions (History h)) msg h.currentModel

        newHistory =
            case action of
                Do (Operation op) ->
                    perform op (History h)

                Do Init ->
                    init h.currentModel

                Undo ->
                    undo (History h)

                Redo o ->
                    redo o (History h)
    in
        ( newHistory, cmd )


{-| View for a [`History`](#History).

You pass your `view` as first parameter, A [`HistoryOptions`](#HistoryOptions) will be passed to your `view`.

You should only need this if you are using the library [`not using program`](#not-using-program)
-}
view : (HistoryOptions op -> model -> Html msg) -> History op model -> Html msg
view viewer (History h) =
    viewer (historyOptions (History h)) h.currentModel


{-| Get Subscriptions from [`History`](#History).

You should only need this if you are using the library [`not using program`](#not-using-program)
-}
subscriptions : (model -> Sub msg) -> History op model -> Sub msg
subscriptions subscriber (History h) =
    subscriber h.currentModel



-- ACTION HELPERS


perform : RealOperation op model -> History op model -> History op model
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


undo : History op model -> History op model
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


redo : op -> History op model -> History op model
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


historyOptions : History op model -> HistoryOptions op
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


getKind : Operation op model -> Maybe op
getKind op =
    case op of
        Operation operation ->
            Just operation.kind

        Init ->
            Nothing
