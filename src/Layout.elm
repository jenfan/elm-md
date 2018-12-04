module Layout exposing (viewHeader)

import Browser
import Html exposing (Html, a, header, button, input, text, div)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- VIEW

viewHeader : Html msg
viewHeader =
    header [ class "header-wrap" ]
        [ div [ class "header" ]
            [ div [ class "header-left" ]
                [ a [ class "header__logo", href "/" ] []
            ]
        ]
    ]
