module Explorer.Ast exposing
    ( Node
    , NodeContent
    , declarationTree
    , importDeclarationTree
    , moduleDefinitionTree
    )

import Arborist.Tree as Tree
import Elm.Syntax.Declaration
import Elm.Syntax.Expression
import Elm.Syntax.Import
import Elm.Syntax.Module
import Elm.Syntax.Node
import Explorer.Range as Range


type alias NodeContent =
    { name : String
    , specifier : Maybe String
    , colorIndex : Int
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
        , content = NodeContent "Module" Nothing 1
        }
        []


importDeclarationTree : Elm.Syntax.Node.Node Elm.Syntax.Import.Import -> Tree.Tree Node
importDeclarationTree (Elm.Syntax.Node.Node range _) =
    Tree.Node
        { range = Just range
        , content = NodeContent "Import" Nothing 1
        }
        []


expressionTree : Elm.Syntax.Node.Node Elm.Syntax.Expression.Expression -> Tree.Tree Node
expressionTree (Elm.Syntax.Node.Node expressionRange expression) =
    case expression of
        Elm.Syntax.Expression.Application expressions ->
            Tree.Node
                { range = Just expressionRange
                , content = NodeContent "Application" Nothing 3
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
                        3
                }
                []

        Elm.Syntax.Expression.Literal str ->
            Tree.Node
                { range = Just expressionRange
                , content =
                    NodeContent "String Literal"
                        (Just <| "\"" ++ str ++ "\"")
                        3
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
                , content = NodeContent "FunctionDeclaration" Nothing 1
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
                                , content = NodeContent "Documentation" Nothing 4
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
                                , content = NodeContent "Signature" Nothing 4
                                }
                                [ Tree.Node
                                    { range = Just nameRange
                                    , content = NodeContent "FunctionName" (Just name) 5
                                    }
                                    []
                                , Tree.Node
                                    { range = Just typeAnnotationRange
                                    , content = NodeContent "Type Annotation" Nothing 5
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
                    , content = NodeContent "Implementation" Nothing 3
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
                , content = NodeContent "AliasDeclaration" Nothing 0
                }
                []

        Elm.Syntax.Declaration.CustomTypeDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "CustomTypeDeclaration" Nothing 0
                }
                []

        Elm.Syntax.Declaration.PortDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "PortDeclaration" Nothing 0
                }
                []

        Elm.Syntax.Declaration.InfixDeclaration _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "InfixDeclaration" Nothing 0
                }
                []

        Elm.Syntax.Declaration.Destructuring _ _ ->
            Tree.Node
                { range = Just range
                , content = NodeContent "Destructuring" Nothing 0
                }
                []
