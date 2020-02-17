module Main exposing (main)

import Html


name : String
name =
    "Paul"


greeting : String
greeting =
    "Hello, " ++ name


main : Html.Html msg
main =
    Html.text greeting
