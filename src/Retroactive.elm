module Retroactive exposing (Action(..), Operation, operation, History, program)

import Html


type Action o m
    = Do (Operation o m)
    | Undo
    | Redo


type Operation o m
    = Operation
        { kind : o
        , do : m -> m
        , undo : m -> m
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
        { previous : List (Operation o m)
        , current : m
        , next : List (Operation o m)
        }


program :
    { init : ( m, Cmd msg )
    , update : msg -> m -> ( Action o m, Cmd msg )
    , view : m -> Html.Html msg
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
    ( History { previous = [], current = model, next = [] }, cmd )


update : (msg -> m -> ( Action o m, Cmd msg )) -> msg -> History o m -> ( History o m, Cmd msg )
update updater msg (History h) =
    let
        ( action, cmd ) =
            updater msg h.current

        newHistory =
            case action of
                Do op ->
                    perform op (History h)

                Undo ->
                    undo (History h)

                Redo ->
                    redo (History h)
    in
        ( newHistory, cmd )


view : (m -> Html.Html msg) -> History o m -> Html.Html msg
view viewer (History h) =
    viewer h.current


subscriptions : (m -> Sub msg) -> History o m -> Sub msg
subscriptions subscriber (History h) =
    subscriber h.current


perform : Operation o m -> History o m -> History o m
perform (Operation op) (History { previous, current, next }) =
    History
        { previous = (Operation op) :: previous
        , current = op.do current
        , next = []
        }


undo : History o m -> History o m
undo (History h) =
    case h.previous of
        (Operation lastOp) :: moreOps ->
            History
                { previous = moreOps
                , current = lastOp.undo h.current
                , next = (Operation lastOp) :: h.next
                }

        [] ->
            History h


redo : History o m -> History o m
redo (History h) =
    case h.next of
        (Operation nextOp) :: moreOps ->
            History
                { previous = (Operation nextOp) :: h.previous
                , current = nextOp.do h.current
                , next = moreOps
                }

        [] ->
            History h
