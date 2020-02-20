module Dashboard.Ast exposing (Node, NodeContent, declarationTree, moduleDeclarationTree)

import Arborist.Tree as Tree
import Dashboard.Range as Range
import Elm.Syntax.Declaration
import Elm.Syntax.Module
import Elm.Syntax.Node


type alias NodeContent =
    { name : String
    , specifier : Maybe String
    , colorIndex : Int
    }


type alias Node =
    { range : Maybe Range.Range
    , content : NodeContent
    }


moduleDeclarationTree :
    Elm.Syntax.Node.Node Elm.Syntax.Module.Module
    -> Tree.Tree Node
moduleDeclarationTree (Elm.Syntax.Node.Node range _) =
    Tree.Node
        { range = Just range
        , content = NodeContent "Module" Nothing 1
        }
        []


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
                                , content = NodeContent "Signature" Nothing 4
                                }
                                [ Tree.Node
                                    { range = Just nameRange
                                    , content = NodeContent "FunctionName" (Just name) 5
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
                    , content = NodeContent "Implementation" Nothing 3
                    }
                    [ let
                        (Elm.Syntax.Node.Node expressionRange expression) =
                            implementation.expression
                      in
                      Tree.Node
                        { range = Just expressionRange
                        , content = NodeContent "Expression" Nothing 2
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
