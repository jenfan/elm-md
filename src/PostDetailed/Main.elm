module PostDetailed.Main exposing (..)

import Browser
import Html exposing (Html, a, header, button, input, text, div, section, ul, li, p, img, article, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http

import Layout exposing (viewHeader)
import PostDetailed.View.Icons exposing (likeIcon, commentIcon)
import PostDetailed.Post
import PostDetailed.Comment exposing (Comment, postComment)
import Json.Decode


main =
    Browser.document
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = \model -> { title = "My Dreams", body = view model }
        }

init : PostDetailed.Post.PostId -> (Model, Cmd Msg)
init post_id =
    let
        (initialPost, cmdPost) = PostDetailed.Post.initialModel post_id

    in
        ({ post = initialPost}, Cmd.map PostMsg cmdPost)

-- MODEL

type alias Model =
    { post : PostDetailed.Post.Model
    }


-- Msg

type Msg
    = PostMsg PostDetailed.Post.Msg

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PostMsg subMsg ->
            let
                ( updatedWidgetModel, postCmd ) =
                    PostDetailed.Post.update subMsg model.post
            in
                ( { model | post = updatedWidgetModel }, Cmd.map PostMsg postCmd )



-- VIEW

view : Model -> List (Html Msg)
view model =
    let
        post =
            Html.map PostMsg (PostDetailed.Post.view model.post)
    in
        [ viewHeader
        , section [ class "content-wrap" ] [ post ]
        ]



