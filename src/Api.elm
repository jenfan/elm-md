module Api exposing (load_post, load_comments)

import Http
import Url.Builder exposing (QueryParameter)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode

import Api.Endpoint as Endpoint exposing (Endpoint, PostId, DreamerId, detailed_post_url, comments_url, unwrap)

type alias Body =
  String

host_url =
  "http://localhost:3000/"

load_post : PostId -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
load_post post_id cmd decoder =
    let
        endpoint = detailed_post_url post_id
    in
        get endpoint cmd decoder

load_comments : PostId -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
load_comments post_id cmd decoder =
    let
        endpoint = comments_url post_id
    in
        get endpoint cmd decoder


get : Endpoint -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
get endpoint cmd decoder =
    Http.get
      { url = unwrap endpoint
      , expect = Http.expectJson cmd decoder
      }

