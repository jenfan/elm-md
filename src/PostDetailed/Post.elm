module PostDetailed.Post exposing (Post, view, Msg, Model, PostId, initialModel, update)

import Browser
import Html exposing (Html, a, header, button, input, text, div, section, ul, li, p, img, article, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Api

import Layout exposing (viewHeader)
import PostDetailed.View.Icons exposing (likeIcon, commentIcon)
import PostDetailed.Dreamer exposing (Dreamer, dreamerDecoder)
import PostDetailed.Comment exposing (Comment, postComment, commentDecoder, commentsDecoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)

-- POST

type alias Image =
    { url : String
    }

type alias PostId = Int

type alias Post =
    { id : PostId
    , images : List Image
    , description : String
    , likesCount : Int
    --, liked_by_me : Bool
    , commentsCount : Int
    , dreamer : Dreamer
    , created_at : String
    , comments : List Comment
    }

type alias Page =
    { post : Post
    , commentsToggle : CommentsToggle
    , commentsLoaded : Bool
    , error : String
    }

loadedPost : Post -> Page
loadedPost post =
    Page post False False ""

type alias CommentsToggle = Bool

-- Model

type Model
    = Failure String
    | NotFound
    | Loading
    | Success Page


initialModel : PostId -> (Model, Cmd Msg)
initialModel post_id =
    ( Loading , Api.load_post post_id PostLoading postDecoder)


-- Msg

type Msg
    = PostLoading (Result Http.Error Post)
    | AllCommentsLoading (Result Http.Error (List Comment))
    | ToggleComments


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PostLoading result ->
        case result of
            Ok post ->
                (Success (loadedPost post), Cmd.none)
            Err err ->
                case err of
                    Http.BadBody error ->
                        Debug.log error
                        (Failure "bad json", Cmd.none)
                    Http.BadStatus status ->
                        case status of
                            404 ->
                                (NotFound, Cmd.none)
                            500 ->
                                (Failure "Sorry, internal error", Cmd.none)
                            _ ->
                                (Failure "Post was deleted or blocked", Cmd.none)
                    Http.NetworkError ->
                        (Failure "no internet", Cmd.none)
                    _ ->
                        (Failure "other error", Cmd.none)

    ToggleComments ->
        case model of
            Success page ->
                case page.commentsLoaded of
                    False ->
                        let
                            cmd = Api.load_comments page.post.id AllCommentsLoading commentsDecoder
                            updated_page = { page | commentsLoaded = True }
                        in
                            ( Success updated_page, cmd )
                    True ->
                        (Success { page | commentsToggle = (not page.commentsToggle) }, Cmd.none)
            _ ->
                (model, Cmd.none)

    AllCommentsLoading result ->
        case model of
            Success page ->
                case result of
                    Ok listComments ->
                        let
                            post = page.post
                            updated_post = { post | comments = listComments }
                        in
                            ( Success { page | post = updated_post, commentsToggle = True }, Cmd.none )
                    Err err ->
                        case err of
                            Http.BadBody error ->
                                Debug.log error
                                ( model, Cmd.none )
                            Http.BadStatus status ->
                                Debug.log ("BadStatus" ++ (String.fromInt status))
                                ( Success { page | error = "BadStatus" }, Cmd.none )
                            Http.NetworkError ->
                                ( Success { page | error = "no internet" }, Cmd.none )
                            _ ->
                                Debug.log "other error"
                                ( Success { page | error = "other error" }, Cmd.none )
            _ ->
                ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    case model of
        NotFound ->
            text "Post not found"

        Failure err ->
            text err

        Loading ->
            text "Loading..."

        Success page ->
            postView page.post page.commentsToggle


postView : Post -> Bool -> Html Msg
postView post commentsToggle =
    ul [ class "dreams-list" ]
        [ li [ class "dream" ]
            [ postHeader post
            , postImg (List.head post.images)
            , postInfo post
            , postComments post.comments commentsToggle
            ]
        ]


postHeader : Post -> Html msg
postHeader post =
    div [ class "user-post-info user-post-info_dream-head" ]
        [ div [ class "user-post-info__wrap" ]
            [ div [ class "user-post-info__avatar" ]
                [ img [ src post.dreamer.avatar ] [] ]
            , div [ class "user-post-info__text" ]
                [ p [ class "user-post-info__name" ]
                    [ text post.dreamer.full_name ]
                , p [ class "user-post-info__time" ]
                    [ text post.created_at ]
                ]
            ]
        ]

postImg : Maybe Image -> Html msg
postImg maybeImage =
    case maybeImage of
        Just image ->
            div [ class "gallery gallery_dream-detail" ]
                [ div [ class "gallery-photos" ]
                    [ img [ src image.url ] []
                    , text "            "
                    ]
                ]
        Nothing ->
            div [] []

postInfo post =
    div [ class "item__info" ]
        [ postDescription post
        , postDetailedInfo post
        ]

postDescription post =
    article []
            [ text post.description ]

postDetailedInfo post =
    div [ class "item-activities" ]
        [ postLikesCount post
        , postCommentsCount post
        ]

postCommentsCount post =
    let
        title =
            (String.fromInt post.commentsCount) ++ " комментария"
    in
        div [ class "item-activities__comments" ]
            [ span [ class "ico" ] [ commentIcon ]
            , span [ class "text" ] [ text title ]
            ]

postLikesCount post =
    let
        title = (String.fromInt post.likesCount) ++ " нравится"
    in
        div [ class "item-activities__likes" ]
            [ span [ class "ico" ] [ likeIcon ]
            , span [ class "text" ] [ text title ]
            ]

postComments : List Comment -> Bool -> Html Msg
postComments comments commentsToggle =
    let
        ( cmntBtn, comments_to_view ) =
            if commentsToggle then
                ( "скрыть комментарии", comments )
            else
                ( "показать все комментарии", List.take 3 comments )
    in

        div [ class "comments-list lot-of-comments" ]
            [ a [ class "lnk-bg", onClick ToggleComments ] [ text cmntBtn ]
            , ul [] (List.map postComment comments_to_view)
            --, form [ class "one-input-form" ]
            --    [ div [ class "author-avatar" ]
            --        []
            --    , div [ class "one-input-form__container" ]
            --        [ input [ name "", placeholder "Написать комментарий" ]
            --            []
            --        , button [ class "btn btn_small btn_inverse", type_ "submit" ]
            --            [ text "Отправить" ]
            --        ]
            --    ]
            ]

postDecoder : Decoder Post
postDecoder =
    let
        postDecode =
            Decode.succeed Post
                |> required "id" Decode.int
                |> required "photos" (Decode.list imageDecoder)
                |> required "content" Decode.string
                |> required "likes_count" Decode.int
                --|> required "liked_by_me" Decode.bool
                |> required "comments_count" Decode.int
                |> required "dreamer" dreamerDecoder
                |> required "created_at" Decode.string
                |> required "last_comments" (Decode.list commentDecoder)


    in
        Decode.field "post" postDecode


imageDecoder : Decoder Image
imageDecoder =
    Decode.succeed Image
        |> required "web_full_retina" Decode.string


--toggleLike : Post -> Post
--toggleLike post =
--    { post | liked_by_me = not post.liked_by_me}
