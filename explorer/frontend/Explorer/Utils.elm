module Explorer.Utils exposing
    ( moveDownFrom
    , moveUpFrom
    )

import Arborist.Tree as Tree


moveUpFrom : node -> Tree.Tree node -> Maybe node
moveUpFrom baseNode tree =
    case tree of
        Tree.Empty ->
            Nothing

        Tree.Node node children ->
            if baseNode == node then
                Just baseNode

            else if
                List.any
                    (\childTree ->
                        case childTree of
                            Tree.Node childNode _ ->
                                childNode == baseNode

                            _ ->
                                False
                    )
                    children
            then
                Just node

            else
                children
                    |> List.filterMap (moveUpFrom baseNode)
                    |> List.head


moveDownFrom : node -> Tree.Tree node -> Maybe node
moveDownFrom baseNode tree =
    case tree of
        Tree.Empty ->
            Nothing

        Tree.Node node children ->
            if baseNode == node then
                children
                    |> List.filterMap
                        (\subtree ->
                            case subtree of
                                Tree.Empty ->
                                    Nothing

                                Tree.Node currentRoot _ ->
                                    Just currentRoot
                        )
                    |> List.head

            else
                children
                    |> List.filterMap (moveDownFrom baseNode)
                    |> List.head
