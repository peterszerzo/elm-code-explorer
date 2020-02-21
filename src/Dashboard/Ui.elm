module Dashboard.Ui exposing (logo)

import Html
import Svg
import Svg.Attributes as SA


logo : Int -> Html.Html msg
logo n =
    Svg.svg
        [ SA.height (String.fromInt n)
        , SA.viewBox "0 0 600 600"
        ]
        [ shape "0,20 280,300 0,580"
        , shape "20,600 300,320 580,600"
        , shape "320,0 600,0 600,280"
        , shape "20,0 280,0 402,122 142,122"
        , shape "170,150 430,150 300,280"
        , shape "320,300 450,170 580,300 450,430"
        , shape "470,450 600,320 600,580"
        ]


shape : String -> Svg.Svg msg
shape coordinates =
    Svg.polygon [ SA.fill "currentColor", SA.points coordinates ] []
