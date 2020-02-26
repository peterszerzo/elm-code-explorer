module Explorer.Range exposing (Range, inside, maybeUnion, union)

import Explorer.Pos as Pos


type alias Range =
    { start : Pos.Pos
    , end : Pos.Pos
    }


union : Range -> Range -> Range
union range1 range2 =
    { start = Pos.min range1.start range2.start
    , end = Pos.max range1.end range2.end
    }


maybeUnion : Maybe Range -> Maybe Range -> Maybe Range
maybeUnion maybeRange1 maybeRange2 =
    case ( maybeRange1, maybeRange2 ) of
        ( Just range1, Just range2 ) ->
            union range1 range2 |> Just

        ( Just range1, Nothing ) ->
            range1 |> Just

        ( Nothing, Just range2 ) ->
            range2 |> Just

        _ ->
            Nothing


inside : Pos.Pos -> Range -> Bool
inside pos range =
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
