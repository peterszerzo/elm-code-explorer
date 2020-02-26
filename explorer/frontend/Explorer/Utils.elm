module Explorer.Utils exposing (moveUpFrom)

import Arborist.Tree as Tree


moveUpFrom : node -> Tree.Tree node -> Maybe node
moveUpFrom baseNode tree =
    case tree of
        Tree.Empty ->
            Nothing

        Tree.Node node children ->
            if baseNode == node then
                Nothing

            else if List.any (\_ -> False) children then
                Just node

            else
                Nothing
