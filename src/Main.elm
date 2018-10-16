module Pages.Kanban exposing (Model, init, main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils exposing (..)
import Json.Decode as D
import Json.Encode as E


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



--------------------------------------------------------------------------------
-- MODEL
--------------------------------------------------------------------------------

type Status
    = Backlog
    | Todo
    | Doing
    | Done
    | Archived


type alias Model =
    { issues : List Issue
    , index : Int
    , input : String
    }


type alias Issue =
    { description : String
    , status : Status
    }


init : Model
init =
    { issues = decoded, index = 0, input = "" }


{-| Create simple flag element
-}
issue : String -> Issue
issue title =
    Issue title Todo


incr : Status -> Status
incr status =
    case status of
        Backlog ->
            Todo

        Todo ->
            Doing

        Doing ->
            Done

        Done ->
            Archived
        
        Archived ->
            Archived


decr : Status -> Status
decr status =
    case status of
        Backlog ->
            Backlog

        Todo ->
            Backlog

        Doing ->
            Todo

        Done ->
            Doing

        Archived ->
            Done

--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Incr Int
    | Decr Int
    | Add
    | Input String


update : Msg -> Model -> Model
update msg m =
    case msg of
        Incr i ->
            { m
                | issues =
                    mapAt i
                        (\x -> { x | status = incr x.status })
                        m.issues
            }

        Decr i ->
            { m
                | issues =
                    mapAt i
                        (\x -> { x | status = decr x.status })
                        m.issues
            }

        Add ->
            { m
                | index = m.index + 1
                , issues = issue m.input :: m.issues
                , input = ""
            }

        Input st ->
            { m | input = st }

        _ ->
            m



--------------------------------------------------------------------------------
-- VIEW FUNCTIONS
--------------------------------------------------------------------------------

view : Model -> Html Msg
view m =
    let
        issueViews =
            List.indexedMap (\i x -> ( x.status, viewIssue i x )) m.issues

        board : Status -> Html Msg
        board s =
            viewList ul li <|
                List.map Tuple.second <|
                    List.filter (Tuple.first >> (==) s) issueViews
    in
    div []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "main.css" ] []
        , h1 [] [ text "Kanban board" ]
        , div [style "display" "flex"
        , style "justify-content" "space-around"
        , style "text-align" "center"
        ]
        [ viewBoard "Backlog" (board Backlog)
        , button [style "align-self" "flex-start"] [text "Backlog"]
        , viewBoard "To-do" (board Todo)
        , viewBoard "Doing" (board Doing)
        , viewBoard "Done" (board Done)
        , button [style "align-self" "flex-start"] [text "Archived"]
        , viewBoard "Archived" (board Archived)
        ]
        , Html.form [ onSubmit Add, style "margin-top" "5rem" ]
            [ input [ placeholder "New issue", value m.input, onInput Input ] []
            , input [ type_ "submit" ] []
            ]
        ]

viewBoard : String -> Html Msg -> Html Msg
viewBoard title issues =
    div [style "border-radius" "2px"
        , style "border-style" "solid"
        , style "min-width" "15rem"
        , style "min-height" "25rem"
        , classList[
            ("hidden", title == "Backlog"),
            ("hidden", title == "Archived")
          ]
        ]
        [ h2 [style "margin" "1rem"] [ text title ]
        , issues
        ]


viewIssue : Int -> Issue -> Html Msg
viewIssue i obj =
    div [style "margin" "0.5rem"]
        [ arrow "<= " Backlog (Decr i) obj.status
        , text obj.description
        , arrow " =>" Archived (Incr i) obj.status
        ]


arrow : String -> Status -> a -> Status -> Html a
arrow arr except msg status =
    let
        cmds =
            if status == except then
                [ class "fade" ]

            else
                [ onClick msg ]
    in
    span cmds [ text arr ]

decoded =
    D.decodeString issuesListDecoder json_example |> Result.withDefault []


issuesListDecoder : D.Decoder (List Issue)
issuesListDecoder = D.list issueDecoder

issueDecoder : D.Decoder Issue
issueDecoder =
    D.map2 Issue
        (D.field "description" D.string)
        (D.field "status" statusDecoder)

statusDecoder : D.Decoder Status
statusDecoder =
    D.string |> D.andThen (\x ->
        case x of
            "backlog" ->
                D.succeed Backlog

            "doing" ->
                D.succeed Doing

            "done" ->
                D.succeed Done

            "archived" ->
                D.succeed Archived

            "todo" ->
                D.succeed Todo

            y ->
                D.fail <| "Status desconhecido '" ++ y ++ "'"
    )


--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


example : Model
example =
    { init
        | issues =
            [ issue "Start board"
            , issue "Create CSS"
            , issue "Learn Haskell"
            ]
    }

json_example =
    """
[
    {
        "description": "Dividir em 5 boards",
        "status": "done"
    }, {
        "status": "doing",
        "description": "Importar de JSON"
    }, {
        "status": "todo",
        "description": "Exportar para JSON"
    }, {
        "status": "backlog",
        "description": "esconder por padrão os boards Backlog e Archived"
    }
]
"""