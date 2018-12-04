module PostDetailed.Comment exposing (Comment, postComment, commentDecoder, commentsDecoder)

import Html exposing (Html, text, article, p, div, li, img)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import PostDetailed.Dreamer exposing (Dreamer, dreamerDecoder)

type alias Comment =
    { dreamer : Dreamer
    , body : String
    , timestamp : String
    }

postComment : Comment -> Html msg
postComment comment =
    li []
        [ div [ class "user-post-info" ]
            [ div [ class "user-post-info__avatar" ]
                [ img [ src comment.dreamer.avatar ] [] ]
            , div [ class "user-post-info__text" ]
                [ p [ class "user-post-info__name" ]
                    [ text comment.dreamer.full_name ]
                , p [ class "user-post-info__time" ]
                    [ text comment.timestamp ]
                , article []
                    [ text comment.body ]
                ]
            ]
        ]


commentDecoder : Decoder Comment
commentDecoder =
    Decode.succeed Comment
        |> required "dreamer" dreamerDecoder
        |> required "body" Decode.string
        |> required "created_at" Decode.string

commentsDecoder : Decoder (List Comment)
commentsDecoder =
    Decode.field "comments" (Decode.list commentDecoder)
