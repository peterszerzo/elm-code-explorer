module Main exposing (main)

import Html


name : String
name =
    "Paul"


{-| Format greeting
-}
greeting : String
greeting =
    "Hello, " ++ name


main : Html.Html msg
main =
    Html.text greeting
