module Dashboard exposing (main)

import Arborist.Tree as Tree
import Browser
import Browser.Dom
import Elm.Parser
import Elm.Processing
import Elm.Syntax.Declaration
import Elm.Syntax.File
import Elm.Syntax.Node
import Html
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Encode as Encode
import Parser
import Task



-- Entry point


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { elmFile : Maybe (Result Http.Error String)
    , windowSize :
        Maybe
            { width : Int
            , height : Int
            }
    , selectedRange : Maybe Range
    }


type alias Pos =
    { row : Int
    , column : Int
    }


type alias Range =
    { start : Pos
    , end : Pos
    }


insideRange : Range -> Pos -> Bool
insideRange range pos =
    pos.row
        <= range.end.row
        && pos.row
        >= range.start.row
        && (if pos.row == range.start.row then
                pos.column >= range.start.column

            else
                True
           )
        && (if pos.row == range.end.row then
                pos.column <= range.end.column

            else
                True
           )


type alias Flags =
    Encode.Value


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { elmFile = Nothing
      , windowSize = Nothing
      , selectedRange = Nothing
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
    | WindowSize { width : Int, height : Int }
    | SelectRange Range
    | NoOp



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

        SelectRange range ->
            ( { model | selectedRange = Just range }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    case model.elmFile of
        Nothing ->
            Html.text "Loading..."

        Just (Ok file) ->
            let
                processedResult =
                    process file
            in
            Html.div
                [ HA.class "container"
                ]
                [ viewCode
                    { file = file
                    , selectedRange = model.selectedRange
                    , onClick =
                        \pos ->
                            processedResult
                                |> Result.map
                                    (\processed ->
                                        Tree.Node
                                            { content = "Declarations"
                                            , range = Nothing
                                            }
                                            (List.map declarationTree processed.declarations)
                                    )
                                |> Result.withDefault Tree.Empty
                                |> findRange pos
                                |> Debug.log "rindrange"
                                |> Maybe.map SelectRange
                                |> Maybe.withDefault NoOp
                    }
                , Html.section
                    [ HA.class "pad"
                    ]
                    (case processedResult of
                        Ok processed ->
                            [ viewTree
                                { selectedRange = model.selectedRange
                                , onSelectRange = SelectRange
                                }
                                (Tree.Node
                                    { content = "Declarations"
                                    , range = Nothing
                                    }
                                    (List.map declarationTree processed.declarations)
                                )
                            ]

                        Err _ ->
                            [ Html.text "Could not parse"
                            ]
                    )
                ]

        Just (Err _) ->
            Html.text "Something went wrong"


viewTree :
    { selectedRange : Maybe Range
    , onSelectRange : Range -> msg
    }
    ->
        Tree.Tree
            { range : Maybe Range
            , content : String
            }
    -> Html.Html msg
viewTree config tree =
    case tree of
        Tree.Empty ->
            Html.text ""

        Tree.Node node children ->
            Html.div
                [ HA.style "margin" "10px 0"
                ]
                [ Html.div [ HA.style "display" "flex" ]
                    [ Html.h3
                        [ HA.style "margin" "0 20px 0 0"
                        , HA.style "font-size" "14px"
                        ]
                        [ Html.text node.content ]
                    , node.range
                        |> Maybe.map
                            (\range ->
                                viewRange
                                    { range = range
                                    , selected = config.selectedRange == Just range
                                    , onSelect = config.onSelectRange range
                                    }
                            )
                        |> Maybe.withDefault (Html.text "")
                    ]
                , Html.div
                    [ HA.style "padding-left" "20px"
                    ]
                    (List.map (viewTree config) children)
                ]


findRange : Pos -> Tree.Tree { a | range : Maybe Range } -> Maybe Range
findRange pos tree =
    case tree of
        Tree.Empty ->
            Nothing

        Tree.Node node children ->
            let
                nodeFind =
                    Maybe.andThen
                        (\range ->
                            if insideRange range pos then
                                Just range

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
                ( Just range, _ ) ->
                    Just range

                ( _, Just range ) ->
                    Just range

                _ ->
                    Nothing


declarationTree :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    ->
        Tree.Tree
            { range : Maybe Range
            , content : String
            }
declarationTree (Elm.Syntax.Node.Node range content) =
    case content of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            Tree.Node
                { range = Just range
                , content = "FunctionDeclaration"
                }
                ([ function.signature
                    |> Maybe.map
                        (\signatureNode ->
                            let
                                (Elm.Syntax.Node.Node signatureRange signature) =
                                    signatureNode

                                (Elm.Syntax.Node.Node nameRange name) =
                                    signature.name
                            in
                            Tree.Node
                                { range = Just signatureRange
                                , content = "Signature"
                                }
                                [ Tree.Node
                                    { range = Just nameRange
                                    , content = "FunctionName: " ++ "\"" ++ name ++ "\""
                                    }
                                    []

                                -- TODO: parse type signature
                                ]
                        )
                 , let
                    (Elm.Syntax.Node.Node implementationRange implementation) =
                        function.declaration
                   in
                   Tree.Node
                    { range = Just implementationRange
                    , content = "Implementation"
                    }
                    [ let
                        (Elm.Syntax.Node.Node expressionRange expression) =
                            implementation.expression
                      in
                      Tree.Node
                        { range = Just expressionRange
                        , content = "Expression"
                        }
                        []
                    ]
                    |> Just
                 ]
                    |> List.filterMap identity
                )

        Elm.Syntax.Declaration.AliasDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = "AliasDeclaration"
                }
                []

        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = "CustomTypeDeclaration"
                }
                []

        Elm.Syntax.Declaration.PortDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = "PortDeclaration"
                }
                []

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = "InfixDeclaration"
                }
                []

        Elm.Syntax.Declaration.Destructuring _ _ ->
            Tree.Node
                { range = Just range
                , content = "Destructuring"
                }
                []



-- General helpers


fetchFile : (Result Http.Error String -> msg) -> Cmd msg
fetchFile onResponse =
    Http.get
        { url = "/file"
        , expect = Http.expectString onResponse
        }


process : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
process =
    Elm.Parser.parse
        >> Result.map
            (Elm.Processing.process Elm.Processing.init)



-- View helpers


viewCode :
    { file : String
    , selectedRange : Maybe Range
    , onClick : Pos -> msg
    }
    -> Html.Html msg
viewCode config =
    Html.section
        [ HA.class "pad"
        ]
        [ Html.div
            [ HA.class "code"
            ]
            [ Html.div []
                (config.file
                    |> String.split "\n"
                    |> List.indexedMap
                        (\rowIndex line ->
                            if line == "" then
                                Html.pre
                                    [ HA.class "code-line"
                                    ]
                                    [ Html.text " "
                                    ]

                            else
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
                                                    [ HA.classList
                                                        [ ( "code-char", True )
                                                        , ( "code-char--highlighted"
                                                          , config.selectedRange
                                                                |> Maybe.map
                                                                    (\range ->
                                                                        insideRange range currentPos
                                                                    )
                                                                |> Maybe.withDefault False
                                                          )
                                                        ]
                                                    , HE.onClick (config.onClick currentPos)
                                                    ]
                                                    [ Html.text <| String.fromChar char
                                                    ]
                                            )
                                    )
                        )
                )
            ]
        ]


viewRange :
    { range : Range
    , selected : Bool
    , onSelect : msg
    }
    -> Html.Html msg
viewRange config =
    Html.small
        [ HA.classList [ ( "range", True ), ( "range--selected", config.selected ) ]
        , HE.onClick config.onSelect
        ]
        [ String.fromInt config.range.start.row
            ++ ":"
            ++ String.fromInt config.range.start.column
            ++ " - "
            ++ String.fromInt config.range.end.row
            ++ ":"
            ++ String.fromInt config.range.end.column
            |> Html.text
        ]
