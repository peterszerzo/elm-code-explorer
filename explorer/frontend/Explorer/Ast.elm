module Explorer.Ast exposing
    ( Node
    , NodeCategory(..)
    , NodeContent
    , declarationTree
    , fillOutRanges
    , importsDeclarationTree
    , moduleDefinitionTree
    )

import Arborist.Tree as Tree
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.Node
import Explorer.Range as Range


type NodeCategory
    = Literal
    | Definition
    | Declaration
    | Annotation
    | Action
    | Comment


type alias NodeContent =
    { name : String
    , specifier : Maybe String
    , category : NodeCategory
    }


type alias Node =
    { range : Maybe Range.Range
    , content : NodeContent
    }


moduleDefinitionTree :
    Elm.Syntax.Node.Node Elm.Syntax.Module.Module
    -> Tree.Tree Node
moduleDefinitionTree (Elm.Syntax.Node.Node range _) =
    Tree.Node
        { range = Just range
        , content = NodeContent "Module" Nothing Definition
        }
        []


fillOutRanges : Tree.Tree Node -> Tree.Tree Node
fillOutRanges tree =
    case tree of
        Tree.Empty ->
            tree

        Tree.Node node children ->
            let
                newChildren =
                    List.map fillOutRanges children

                newRange =
                    case node.range of
                        Just justRange ->
                            Just justRange

                        Nothing ->
                            newChildren
                                |> List.map
                                    (\subtree ->
                                        case subtree of
                                            Tree.Empty ->
                                                Nothing

                                            Tree.Node subtreeRoot _ ->
                                                subtreeRoot.range
                                    )
                                |> List.foldl Range.maybeUnion Nothing
            in
            Tree.Node { node | range = newRange } newChildren


importsDeclarationTree : List (Elm.Syntax.Node.Node Elm.Syntax.Import.Import) -> Tree.Tree Node
importsDeclarationTree imports =
    case imports of
        [] ->
            Tree.Empty

        (Elm.Syntax.Node.Node range _) :: [] ->
            Tree.Node
                { range = Just range
                , content = NodeContent "Import" Nothing Definition
                }
                []

        _ ->
            Tree.Node
                { content = NodeContent "Imports" Nothing Declaration
                , range = Nothing
                }
                (List.map
                    (\(Elm.Syntax.Node.Node range _) ->
                        Tree.Node
                            { range = Just range
                            , content = NodeContent "Import" Nothing Definition
                            }
                            []
                    )
                    imports
                )


expressionTree : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Tree.Tree Node
expressionTree (Elm.Syntax.Node.Node expressionRange expression) =
    case expression of
        Elm.Syntax.Expression.Application expressions ->
            Tree.Node
                { range = Just expressionRange
                , content = NodeContent "Application" Nothing Action
                }
                (List.map expressionTree expressions)

        Elm.Syntax.Expression.FunctionOrValue parts name ->
            Tree.Node
                { range = Just expressionRange
                , content =
                    NodeContent "Value"
                        (parts
                            ++ [ name ]
                            |> String.join "."
                            |> Just
                        )
                        Definition
                }
                []

        Elm.Syntax.Expression.Literal str ->
            Tree.Node
                { range = Just expressionRange
                , content =
                    NodeContent "String Literal"
                        (Just <| "\"" ++ str ++ "\"")
                        Literal
                }
                []

        _ ->
            Tree.Empty


declarationTree :
    Elm.Syntax.Node.Node Elm.Syntax.Declaration.Declaration
    -> Tree.Tree Node
declarationTree (Elm.Syntax.Node.Node range content) =
    case content of
        Elm.Syntax.Declaration.FunctionDeclaration function ->
            Tree.Node
                { range = Just range
                , content = NodeContent "FunctionDeclaration" Nothing Declaration
                }
                ([ function.documentation
                    |> Maybe.map
                        (\documentationNode ->
                            let
                                (Elm.Syntax.Node.Node currentRange _) =
                                    documentationNode
                            in
                            Tree.Node
                                { range = Just currentRange
                                , content = NodeContent "Documentation" Nothing Comment
                                }
                                []
                        )
                 , function.signature
                    |> Maybe.map
                        (\signatureNode ->
                            let
                                (Elm.Syntax.Node.Node signatureRange signature) =
                                    signatureNode

                                (Elm.Syntax.Node.Node nameRange name) =
                                    signature.name

                                (Elm.Syntax.Node.Node typeAnnotationRange _) =
                                    signature.typeAnnotation
                            in
                            Tree.Node
                                { range = Just signatureRange
                                , content = NodeContent "Signature" Nothing Annotation
                                }
                                [ Tree.Node
                                    { range = Just nameRange
                                    , content = NodeContent "FunctionName" (Just name) Definition
                                    }
                                    []
                                , Tree.Node
                                    { range = Just typeAnnotationRange
                                    , content = NodeContent "Type Annotation" Nothing Annotation
                                    }
                                    []
                                ]
                        )
                 , let
                    (Elm.Syntax.Node.Node implementationRange implementation) =
                        function.declaration
                   in
                   Tree.Node
                    { range = Just implementationRange
                    , content = NodeContent "Implementation" Nothing Definition
                    }
                    [ expressionTree implementation.expression
                    ]
                    |> Just
                 ]
                    |> List.filterMap identity
                )

        Elm.Syntax.Declaration.AliasDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "AliasDeclaration" Nothing Definition
                }
                []

        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "CustomTypeDeclaration" Nothing Declaration
                }
                []

        Elm.Syntax.Declaration.PortDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "PortDeclaration" Nothing Declaration
                }
                []

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "InfixDeclaration" Nothing Declaration
                }
                []

        Elm.Syntax.Declaration.Destructuring _ _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "Destructuring" Nothing Action
                }
                []
