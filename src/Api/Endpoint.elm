module Api.Endpoint exposing (Endpoint, detailed_post_url, comments_url, PostId, DreamerId, unwrap)

import Http
import Url.Builder exposing (QueryParameter)
import Json.Decode as Decode exposing (Decoder)

type alias Id = Int
type alias PostId = Id
type alias DreamerId = Id

type Endpoint = Endpoint String

unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


host_url =
    "http://localhost:3000"


detailed_post_url : Id -> Endpoint
detailed_post_url post_id =
    url [ "elm/posts", String.fromInt post_id ] []


comments_url : Id -> Endpoint
comments_url post_id =
    url [ "elm/posts", String.fromInt post_id, "comments" ] []


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin host_url
        ("api" :: paths)
        queryParams
        |> Endpoint
