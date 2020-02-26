module Explorer.Pos exposing (Pos, compare, max, min)


type alias Pos =
    { row : Int
    , column : Int
    }


compare : Pos -> Pos -> Order
compare pos1 pos2 =
    if pos1.row < pos2.row then
        LT

    else if pos1.row > pos2.row then
        GT

    else if pos1.column < pos2.column then
        LT

    else if pos1.column > pos2.column then
        GT

    else
        EQ


min : Pos -> Pos -> Pos
min pos1 pos2 =
    case compare pos1 pos2 of
        LT ->
            pos1

        _ ->
            pos2


max : Pos -> Pos -> Pos
max pos1 pos2 =
    case compare pos1 pos2 of
        GT ->
            pos1

        _ ->
            pos2
