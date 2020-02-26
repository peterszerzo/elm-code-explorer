module Explorer.Utils exposing (moveUpFrom)

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
