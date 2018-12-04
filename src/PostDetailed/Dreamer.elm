module PostDetailed.Dreamer exposing (Dreamer, dreamerDecoder)

import Browser
import Html exposing (Html, a, header, button, input, text, div, section, ul, li, p, img, article, span)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)

-- POST

type alias Dreamer =
    { full_name : String
    , avatar : String
    }

-- VIEW

userInfo : Dreamer -> String -> Html msg
userInfo user date =
    div [ class "user-post-info user-post-info_dream-head" ]
        [ div [ class "user-post-info__wrap" ]
            [ div [ class "user-post-info__avatar" ]
                []
            , div [ class "user-post-info__text" ]
                [ p [ class "user-post-info__name" ]
                    [ text user.full_name ]
                , p [ class "user-post-info__time" ]
                    [ text date ]
                ]
            ]
        ]



dreamerDecoder : Decoder Dreamer
dreamerDecoder =
    Decode.succeed Dreamer
        |> required "full_name" Decode.string
        |> required "avatar" (Decode.field "web_card" Decode.string)


