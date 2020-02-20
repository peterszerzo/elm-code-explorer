module Dashboard.Range exposing (Range, inside)

import Dashboard.Pos as Pos


type alias Range =
    { start : Pos.Pos
    , end : Pos.Pos
    }


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
