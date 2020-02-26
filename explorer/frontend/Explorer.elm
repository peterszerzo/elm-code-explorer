port module Explorer exposing (main)

import Arborist.Tree as Tree
import Browser
import Browser.Dom
import Browser.Events
import Elm.Parser
import Elm.Processing
import Elm.Syntax.File
import Explorer.Ast as Ast
import Explorer.Pos as Pos
import Explorer.Range as Range
import Explorer.Ui as Ui
import Explorer.Utils as Utils
import Html
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Parser
import Task


port scrollIntoView : String -> Cmd msg



-- Entry point


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { elmFile : Maybe (Result Http.Error String)
    , windowSize :
        Maybe
            { width : Int
            , height : Int
            }
    , selectedNode : Maybe Ast.Node
    }


type alias Flags =
    Encode.Value


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { elmFile = Nothing
      , windowSize = Nothing
      , selectedNode = Nothing
      }
    , Cmd.batch
        [ fetchFile ReceiveFile
        , Browser.Dom.getViewport
            |> Task.map
                (\viewport ->
                    { width = floor viewport.scene.width
                    , height = floor viewport.scene.height
                    }
                )
            |> Task.perform WindowSize
        ]
    )



-- Msg


type Msg
    = ReceiveFile (Result Http.Error String)
    | WindowSize
        { width : Int
        , height : Int
        }
    | SelectNode Bool (Maybe Ast.Node)
    | ArrowPress (Tree.Tree Ast.Node) ArrowDirection



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    model.elmFile
        |> Maybe.andThen Result.toMaybe
        |> Maybe.andThen (process >> Result.toMaybe)
        |> Maybe.map
            (\tree ->
                Browser.Events.onKeyDown keyDecoder
                    |> Sub.map (ArrowPress (toTree tree))
            )
        |> Maybe.withDefault Sub.none


type ArrowDirection
    = ArrowDown
    | ArrowUp
    | ArrowLeft
    | ArrowRight


keyDecoder : Decode.Decoder ArrowDirection
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowDown" ->
                        Decode.succeed ArrowDown

                    "ArrowUp" ->
                        Decode.succeed ArrowUp

                    "ArrowLeft" ->
                        Decode.succeed ArrowLeft

                    "ArrowRight" ->
                        Decode.succeed ArrowRight

                    _ ->
                        Decode.fail "No arrow key detected. This is fine"
            )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveFile res ->
            ( { model
                | elmFile = Just res
              }
            , Cmd.none
            )

        WindowSize windowSize ->
            ( { model
                | windowSize = Just windowSize
              }
            , Cmd.none
            )

        SelectNode shouldScrollIntoView node ->
            ( { model
                | selectedNode = node
              }
            , case ( shouldScrollIntoView, Maybe.andThen .range node ) of
                ( True, Just range ) ->
                    scrollIntoView <| nodeId range

                _ ->
                    Cmd.none
            )

        ArrowPress tree arrow ->
            case arrow of
                ArrowUp ->
                    ( { model
                        | selectedNode =
                            model.selectedNode
                                |> Maybe.andThen
                                    (\selectedNode ->
                                        Utils.moveUpFrom selectedNode tree
                                    )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Html.Html Msg
view model =
    case model.elmFile of
        Nothing ->
            Html.div [ HA.class "container" ]
                [ Html.text "Loading..."
                ]

        Just (Ok file) ->
            let
                processedResult =
                    process file
            in
            Html.div
                [ HA.class "container"
                ]
                [ Html.header [ HA.class "header" ]
                    [ Ui.logo 24
                    , Html.p
                        [ HA.class "header-title"
                        ]
                        [ Html.text "Elm Code Explorer"
                        ]
                    ]
                , case processedResult of
                    Err _ ->
                        Html.text "Could not parse"

                    Ok processed ->
                        let
                            tree =
                                toTree processed
                        in
                        Html.div
                            [ HA.class "content"
                            ]
                            [ Html.section
                                [ HA.class "pad"
                                ]
                                [ Html.div [ HA.class "label" ]
                                    [ Html.text "Main.elm"
                                    ]
                                , viewCode
                                    { file = file
                                    , selectedNode = model.selectedNode
                                    , tree = tree
                                    , onClick =
                                        \pos ->
                                            tree
                                                |> findRange pos
                                                |> SelectNode True
                                    }
                                ]
                            , Html.section
                                [ HA.class "pad"
                                ]
                                [ Html.div [ HA.class "label" ]
                                    [ Html.text "AST"
                                    ]
                                , viewTree
                                    { selectedNode = model.selectedNode
                                    , onSelectNode = SelectNode False
                                    }
                                    tree
                                ]
                            ]
                ]

        Just (Err _) ->
            Html.text "Something went wrong"


nodeId : Range.Range -> String
nodeId range =
    "nd-"
        ++ String.fromInt range.start.column
        ++ "-"
        ++ String.fromInt range.start.row
        ++ "-"
        ++ String.fromInt range.end.column
        ++ "-"
        ++ String.fromInt range.end.row


colorIndex : Ast.NodeCategory -> Int
colorIndex category =
    case category of
        Ast.Literal ->
            2

        Ast.Definition ->
            3

        Ast.Declaration ->
            4

        Ast.Annotation ->
            5

        Ast.Action ->
            1

        Ast.Comment ->
            0


viewTree :
    { selectedNode : Maybe Ast.Node
    , onSelectNode : Maybe Ast.Node -> msg
    }
    -> Tree.Tree Ast.Node
    -> Html.Html msg
viewTree config tree =
    case tree of
        Tree.Empty ->
            Html.text ""

        Tree.Node node children ->
            let
                colorIndex_ =
                    colorIndex node.content.category
            in
            Html.div
                [ HA.class "node-container"
                ]
                [ Html.div
                    ([ [ HA.classList
                            [ ( "node", True )
                            , ( "node--selectable", node.range /= Nothing )
                            , ( "type-" ++ String.fromInt (modBy 6 colorIndex_), True )
                            , ( "node--selected"
                              , config.selectedNode
                                    /= Nothing
                                    && Maybe.andThen .range config.selectedNode
                                    == node.range
                              )
                            ]
                       ]
                     , case node.range of
                        Nothing ->
                            []

                        Just range ->
                            [ HE.onClick (config.onSelectNode (Just node))
                            , HA.id (nodeId range)
                            ]
                     ]
                        |> List.foldr (++) []
                    )
                    [ [ Html.text node.content.name |> Just
                      , Maybe.map
                            (\specifier ->
                                Html.em []
                                    [ Html.text <| " [" ++ specifier ++ "]"
                                    ]
                            )
                            node.content.specifier
                      ]
                        |> List.filterMap identity
                        |> Html.h3
                            [ HA.class "node-title"
                            ]
                    , node.range
                        |> Maybe.map viewRange
                        |> Maybe.withDefault (Html.text "")
                    ]
                , Html.div
                    [ HA.style "padding-left" "30px"
                    ]
                    (List.map (viewTree config) children)
                ]


findRange :
    Pos.Pos
    -> Tree.Tree Ast.Node
    -> Maybe Ast.Node
findRange pos tree =
    case tree of
        Tree.Empty ->
            Nothing

        Tree.Node node children ->
            let
                nodeFind =
                    Maybe.andThen
                        (\range ->
                            if Range.inside pos range then
                                Just node

                            else
                                Nothing
                        )
                        node.range

                childrenFind =
                    children
                        |> List.map (findRange pos)
                        |> List.filterMap identity
                        |> List.head
            in
            case ( childrenFind, nodeFind ) of
                ( Just foundNode, _ ) ->
                    Just foundNode

                ( _, Just foundNode ) ->
                    Just foundNode

                _ ->
                    Nothing



-- General helpers


fetchFile : (Result Http.Error String -> msg) -> Cmd msg
fetchFile onResponse =
    Http.get
        { url = "/Main.elm"
        , expect = Http.expectString onResponse
        }


process : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
process =
    Elm.Parser.parse
        >> Result.map
            (Elm.Processing.process Elm.Processing.init)


toTree : Elm.Syntax.File.File -> Tree.Tree Ast.Node
toTree processed =
    Tree.Node
        { content = Ast.NodeContent "File" Nothing Ast.Definition
        , range = Nothing
        }
        [ Ast.moduleDefinitionTree processed.moduleDefinition
        , Ast.importsDeclarationTree processed.imports
        , Tree.Node
            { content = Ast.NodeContent "Declarations" Nothing Ast.Declaration
            , range = Nothing
            }
            (List.map Ast.declarationTree processed.declarations)
        ]
        |> Ast.fillOutRanges



-- View helpers


viewCode :
    { file : String
    , selectedNode : Maybe Ast.Node
    , tree : Tree.Tree Ast.Node
    , onClick : Pos.Pos -> msg
    }
    -> Html.Html msg
viewCode config =
    Html.div
        [ HA.class "code"
        ]
        [ Html.div []
            (config.file
                |> String.split "\n"
                |> List.indexedMap
                    (\rowIndex line ->
                        Html.pre
                            [ HA.class "code-line"
                            ]
                            (line
                                |> String.toList
                                |> List.indexedMap
                                    (\columnIndex char ->
                                        let
                                            currentPos =
                                                { row = rowIndex + 1
                                                , column = columnIndex + 1
                                                }
                                        in
                                        Html.span
                                            [ [ [ ( "code-char", True ) ]
                                              , config.selectedNode
                                                    |> Maybe.andThen
                                                        (\node ->
                                                            Maybe.andThen
                                                                (\range ->
                                                                    if Range.inside currentPos range then
                                                                        Just node

                                                                    else
                                                                        Nothing
                                                                )
                                                                node.range
                                                        )
                                                    |> Maybe.map
                                                        (\selectedNode ->
                                                            [ ( "code-char--highlighted", True )
                                                            , ( "type-"
                                                                    ++ String.fromInt
                                                                        (modBy 6 (colorIndex selectedNode.content.category))
                                                              , True
                                                              )
                                                            ]
                                                        )
                                                    |> Maybe.withDefault []
                                              ]
                                                |> List.foldl (++) []
                                                |> HA.classList
                                            , HE.onClick (config.onClick currentPos)
                                            ]
                                            [ Html.text <| String.fromChar char
                                            ]
                                    )
                                |> (\els ->
                                        Html.span
                                            [ HA.class "code-empty-char"
                                            ]
                                            [ Html.text (toStringWithPadding rowIndex)
                                            ]
                                            :: els
                                   )
                            )
                    )
            )
        ]


toStringWithPadding : Int -> String
toStringWithPadding val =
    (if val < 10 then
        " "

     else
        ""
    )
        ++ String.fromInt val
        ++ " "


viewRange : Range.Range -> Html.Html msg
viewRange range =
    Html.small
        [ HA.class "range"
        ]
        [ String.fromInt range.start.row
            ++ ":"
            ++ String.fromInt range.start.column
            ++ " - "
            ++ String.fromInt range.end.row
            ++ ":"
            ++ String.fromInt range.end.column
            |> Html.text
        ]
