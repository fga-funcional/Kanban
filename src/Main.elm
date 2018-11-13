module Pages.Kanban exposing (Model, init, main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils exposing (..)
import Json.Decode as D
import Json.Encode as E
import Animation exposing (px)



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
    , style : Animation.State
    , style2 : Animation.State
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
      , style = Animation.style[Animation.opacity 0.0]
      , style2 = Animation.style[Animation.opacity 0.0]
      }
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.batch
    [ Animation.subscription Animate2 [m.style2]
    , Animation.subscription Animate [m.style]
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

--------------------------------------------------------------------------------
-- MESSAGES
--------------------------------------------------------------------------------


type Msg
    = NoOp
    | Incr Int
    | Decr Int
    | Add
    | Input String
    | Animate Animation.Msg
    | Animate2 Animation.Msg
    | FadeIn
    | FadeOut
    | FadeIn2
    | FadeOut2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
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
            ( { m | input = st } , Cmd.none)


        FadeIn2 ->
            ( { m 
                | style2 = 
                    Animation.interrupt
                        [ Animation.to
                            [Animation.opacity 1
                            ]
                        ]
                    m.style2
                }
                , Cmd.none
            )


        FadeIn ->
            ( { m 
                | style = 
                    Animation.interrupt
                        [ Animation.to
                            [Animation.opacity 1
                            ]
                        ]
                    m.style
                }
                , Cmd.none
            )

        FadeOut2 ->
            ( { m 
                | style2 = 
                    Animation.interrupt
                        [ Animation.to
                            [Animation.opacity 0
                            ]
                        ]
                    m.style2
                }
                , Cmd.none
            )

        FadeOut ->
            ( { m 
                | style = 
                    Animation.interrupt
                        [ Animation.to
                            [Animation.opacity 0
                            ]
                        ]
                    m.style
                }
                , Cmd.none
            )
        Animate animMsg ->
            ( { m
                | style = Animation.update animMsg m.style
              }
            , Cmd.none
            )

        Animate2 animMsg ->
            ( { m
                | style2 = Animation.update animMsg m.style2
              }
            , Cmd.none
            )

        _ ->
            ( m, Cmd.none)



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
        [ div ( Animation.render m.style2 ++ [] ) [ viewBoard "Backlog" (board Backlog)]
        , button [onClick FadeIn2, style "align-self" "flex-start", style "cursor" "pointer"] [text "Backlog"]
        , viewBoard "To-do" (board Todo)
        , viewBoard "Doing" (board Doing)
        , viewBoard "Done" (board Done)
        , button [onClick FadeIn, style "align-self" "flex-start", style "cursor" "pointer"] [text "Archived"]
        , div ( Animation.render m.style ++ [] ) [ viewBoard "Archived" (board Archived)]
        ]
        , Html.form [ onSubmit Add, style "margin-top" "5rem" ]
            [ input [ placeholder "New issue", value m.input, onInput Input ] []
            , input [ type_ "submit" ] []
            ]
        ]

viewBoard : String -> Html Msg -> Html Msg
viewBoard title issues =
    div [ style "border-radius" "2px"
        , style "border-style" "solid"
        , style "min-width" "15rem"
        , style "min-height" "25rem"
        ]
        [ 
        div [ style "display" "inline-flex"
            , style "align-items" "center"
            ]
            [ h2 [style "margin" "1rem"] [ text title ]
            , p [style "cursor" "pointer", style "border-style" "solid", style "border-width" "1px", style "width" "20px", onClick FadeOut, classList [("hidden", title == "To-do"), ("hidden", title == "Doing"), ("hidden", title == "Done"), ("hidden", title == "Backlog")]] [text "X"]
            , p [style "cursor" "pointer", style "border-style" "solid", style "border-width" "1px", style "width" "20px", onClick FadeOut2, classList [("hidden", title == "To-do"), ("hidden", title == "Doing"), ("hidden", title == "Done"), ("hidden", title == "Archived")]] [text "X"]
            ]
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