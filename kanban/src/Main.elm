module Pages.Kanban exposing (Model, init, main)

import Animation exposing (px)
import Array
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D
import Json.Encode as E
import Utils exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
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
    , archivedStyle : Animation.State
    , backlogStyle : Animation.State
    }


type alias Issue =
    { description : String
    , status : Status
    }


init : ( Model, Cmd Msg )
init =
    ( { issues = decoded
      , index = 0
      , input = ""
      , archivedStyle = Animation.style [ Animation.opacity 0.0 ]
      , backlogStyle = Animation.style [ Animation.opacity 0.0 ]
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
        [ Animation.subscription ArchivedAnimate [ m.backlogStyle ]
        , Animation.subscription BacklogAnimate [ m.archivedStyle ]
        ]


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


arch : Status -> Status
arch status =
    case status of
        Backlog ->
            Archived

        Todo ->
            Archived

        Doing ->
            Archived

        Done ->
            Archived

        Archived ->
            Archived


unarch : Status -> Status
unarch status =
    case status of
        Backlog ->
            Todo

        Todo ->
            Todo

        Doing ->
            Todo

        Done ->
            Todo

        Archived ->
            Todo



--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Incr Int
    | Decr Int
    | Arch Int
    | Unarch Int
    | Add
    | Input String
    | BacklogAnimate Animation.Msg
    | ArchivedAnimate Animation.Msg
    | FadeInArchived
    | FadeOutArchived
    | FadeInBacklog
    | FadeOutBacklog
    | GotIssues (Result Http.Error (List Issue))
    | GetIssues


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        Unarch i ->
            ( { m
                | issues =
                    mapAt i
                        (\x -> { x | status = unarch x.status })
                        m.issues
              }
            , Cmd.none
            )

        Arch i ->
            ( { m
                | issues =
                    mapAt i
                        (\x -> { x | status = arch x.status })
                        m.issues
              }
            , Cmd.none
            )

        Incr i ->
            ( { m
                | issues =
                    mapAt i
                        (\x -> { x | status = incr x.status })
                        m.issues
              }
            , Cmd.none
            )

        Decr i ->
            ( { m
                | issues =
                    mapAt i
                        (\x -> { x | status = decr x.status })
                        m.issues
              }
            , Cmd.none
            )

        Add ->
            ( { m
                | index = m.index + 1
                , issues = issue m.input :: m.issues
                , input = ""
              }
            , Cmd.none
            )

        Input st ->
            ( { m | input = st }, Cmd.none )

        FadeInBacklog ->
            ( { m
                | backlogStyle =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 1
                            ]
                        ]
                        m.backlogStyle
              }
            , Cmd.none
            )

        FadeInArchived ->
            ( { m
                | archivedStyle =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 1
                            ]
                        ]
                        m.archivedStyle
              }
            , Cmd.none
            )

        FadeOutBacklog ->
            ( { m
                | backlogStyle =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 0
                            ]
                        ]
                        m.backlogStyle
              }
            , Cmd.none
            )

        FadeOutArchived ->
            ( { m
                | archivedStyle =
                    Animation.interrupt
                        [ Animation.to
                            [ Animation.opacity 0
                            ]
                        ]
                        m.archivedStyle
              }
            , Cmd.none
            )

        BacklogAnimate animMsg ->
            ( { m
                | archivedStyle = Animation.update animMsg m.archivedStyle
              }
            , Cmd.none
            )

        ArchivedAnimate animMsg ->
            ( { m
                | backlogStyle = Animation.update animMsg m.backlogStyle
              }
            , Cmd.none
            )

        GetIssues ->
            ( m, Http.send GotIssues getIssues )

        GotIssues result ->
            case result of
                Err httpError ->
                    ( m, Cmd.none )

                Ok i ->
                    ( { m | issues = i }, Cmd.none )

        _ ->
            ( m, Cmd.none )



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
        [ Html.node "link"
            [ Html.Attributes.rel "stylesheet"
            , Html.Attributes.href "main.css"
            ]
            []
        , h1 [] [ text "Kanban board" ]
        , div
            [ style "display" "flex"
            , style "justify-content" "space-around"
            , style "text-align" "center"
            ]
            [ div (Animation.render m.backlogStyle ++ [])
                [ viewBoard "Backlog" (board Backlog) ]
            , button
                [ onClick FadeInBacklog
                , class "big-button"
                ]
                [ text "Backlog" ]
            , viewBoard "To-do" (board Todo)
            , viewBoard "Doing" (board Doing)
            , viewBoard "Done" (board Done)
            , button
                [ onClick FadeInArchived
                , class "big-button"
                ]
                [ text "Archived" ]
            , div (Animation.render m.archivedStyle ++ [])
                [ viewBoard "Archived" (board Archived) ]
            ]
        , Html.form [ onSubmit Add, style "margin-top" "5rem" ]
            [ input [ placeholder "New issue", value m.input, onInput Input ] []
            , input [ type_ "submit" ] []
            ]
        ]


viewBoard : String -> Html Msg -> Html Msg
viewBoard title issues =
    div [ class "board" ]
        [ div [ class "board-header" ]
            [ h2 [ style "margin" "1rem" ] [ text title ]
            , p
                [ class "x-button"
                , onClick FadeOutArchived
                , classList
                    [ ( "hidden", title == "To-do" )
                    , ( "hidden", title == "Doing" )
                    , ( "hidden", title == "Done" )
                    , ( "hidden", title == "Backlog" )
                    ]
                ]
                [ text "X" ]
            , p
                [ class "x-button"
                , onClick FadeOutBacklog
                , classList
                    [ ( "hidden", title == "To-do" )
                    , ( "hidden", title == "Doing" )
                    , ( "hidden", title == "Done" )
                    , ( "hidden", title == "Archived" )
                    ]
                ]
                [ text "X" ]
            ]
        , issues
        ]


viewIssue : Int -> Issue -> Html Msg
viewIssue i obj =
    div []
        [ div
            [ style "margin" "0.5rem"
            , style "display" "inline-flex"
            ]
            [ span
                [ class "x-button"
                , classList
                    [ ( "hidden", obj.status == Backlog )
                    , ( "hidden", obj.status == Archived )
                    ]
                ]
                [ arrow " <= " Backlog (Decr i) obj.status ]
            , span
                [ class "x-button"
                , classList
                    [ ( "hidden", obj.status == Backlog )
                    , ( "hidden", obj.status == Todo )
                    , ( "hidden", obj.status == Doing )
                    , ( "hidden", obj.status == Done )
                    ]
                ]
                [ arrow " << " Todo (Unarch i) obj.status ]
            , p [ class "description" ]
                [ text obj.description ]
            , span
                [ class "x-button"
                , classList
                    [ ( "hidden", obj.status == Archived ) ]
                ]
                [ arrow " => " Archived (Incr i) obj.status ]
            ]
        , span
            [ class "x-button"
            , classList [ ( "hidden", obj.status == Archived ) ]
            ]
            [ arrow "v" Archived (Arch i) obj.status ]
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
issuesListDecoder =
    D.list issueDecoder


issueDecoder : D.Decoder Issue
issueDecoder =
    D.map2 Issue
        (D.field "description" D.string)
        (D.field "status" statusDecoder)


statusDecoder : D.Decoder Status
statusDecoder =
    D.string
        |> D.andThen
            (\x ->
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


getIssues : Http.Request (List Issue)
getIssues =
    Http.get "http://localhost:3000/issues" issuesListDecoder



--------------------------------------------------------------------------------
-- EXAMPLES
--------------------------------------------------------------------------------


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
