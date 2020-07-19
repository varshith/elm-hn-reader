--
-- A sample project that fetches top HN stories
--


module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, required)
import Platform.Cmd
import Process
import RemoteData exposing (RemoteData, WebData)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { ids : WebData (List Int)
    , story : WebData (List Story)
    }


initialModel : Model
initialModel =
    { ids = RemoteData.NotAsked
    , story = RemoteData.NotAsked
    }


type alias Story =
    { title : String
    , author : String
    , score : Int
    , comments : List Int
    , url : String
    , id : Int
    }


init : () -> ( Model, Cmd Msg )
init model =
    ( initialModel, getRecentStoryIds )



-- UPDATE


type Msg
    = MorePlease
    | GotStoryIds (WebData (List Int))
    | GotStory (WebData Story)
    | ChangeTitle Int String


pageSize =
    5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStoryIds result ->
            let
                ids =
                    fromRemoteList result

                takeIds =
                    List.take pageSize ids

                dropIds =
                    List.drop pageSize ids

                updatedList =
                    RemoteData.succeed dropIds
            in
            ( { model | ids = updatedList }, Platform.Cmd.batch (getStoryFromIds takeIds) )

        GotStory result ->
            let
                resultStory =
                    RemoteData.fromList [ result ]

                relist =
                    if model.story == RemoteData.NotAsked then
                        resultStory

                    else
                        mergeStories resultStory model.story
            in
            ( { model | story = relist }, Cmd.none )

        MorePlease ->
            let
                ids =
                    fromRemoteList model.ids

                takeIds =
                    List.take pageSize ids

                dropIds =
                    List.drop pageSize ids

                updatedList =
                    RemoteData.succeed dropIds
            in
            ( { model | ids = updatedList }, Platform.Cmd.batch (getStoryFromIds takeIds) )

        ChangeTitle id title ->
            let
                storiesList =
                    fromRemoteList model.story

                titleCheck story =
                    if story.id == id then
                        { story | title = title }

                    else
                        story

                stories =
                    List.map titleCheck storiesList

                storiesWebData =
                    RemoteData.succeed stories
            in
            ( { model | story = storiesWebData }, Cmd.none )


mergeStories : WebData (List Story) -> WebData (List Story) -> WebData (List Story)
mergeStories a b =
    RemoteData.map List.append a
        |> RemoteData.andMap b


fromRemoteList : WebData (List a) -> List a
fromRemoteList webData =
    case webData of
        RemoteData.NotAsked ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Failure e ->
            []

        RemoteData.Success data ->
            data



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [ class "header" ] [ text "HN Top Stories" ]
        , viewStories model
        , viewMore model
        ]


viewStories : Model -> Html Msg
viewStories model =
    case model.story of
        RemoteData.NotAsked ->
            h3 [ class "wait" ] [ text "Waiting..." ]

        RemoteData.Loading ->
            h3 [ class "wait" ] [ text "Loading..." ]

        RemoteData.Success stories ->
            let
                storyList =
                    fromRemoteList model.story
            in
            div []
                (List.map storyItem (List.reverse storyList))

        RemoteData.Failure httpError ->
            div []
                [ text "I could not load posts for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]


viewMore : Model -> Html Msg
viewMore model =
    case model.ids of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            div [] []

        RemoteData.Success ids ->
            if List.length ids > 0 then
                button
                    [ onClick MorePlease
                    , class "load-more"
                    ]
                    [ text "Give me more" ]

            else
                div [] []

        RemoteData.Failure _ ->
            div [] []


storyItem : Story -> Html Msg
storyItem story =
    if story.url /= "" then
        let
            commentCount =
                List.length story.comments
        in
        article
            [ class "card"
            ]
            [ a
                [ href story.url
                , class "card-title"
                ]
                [ h3 [] [ text story.title ] ]
            , p [ class "card-details" ]
                [ span [] [ text "Authored by " ]
                , span [ class "card-author" ] [ text story.author ]
                , span [] [ text (" (" ++ String.fromInt story.score ++ " points)") ]
                , span [] [ text (" | " ++ String.fromInt commentCount ++ " comments") ]
                ]
            , div []
                [ span [] [ text "Edit title: " ]
                , input
                    [ placeholder "Edit Title"
                    , Html.Attributes.value story.title
                    , onInput (ChangeTitle story.id)
                    , class "card-title-textbox"
                    ]
                    []
                ]
            ]

    else
        div [] []



-- HTTP


serverAddr =
    "https://hacker-news.firebaseio.com/v0/"


getStoryFromIds : List Int -> List (Cmd Msg)
getStoryFromIds ids =
    List.map getStoryFromId ids


getStoryFromId : Int -> Cmd Msg
getStoryFromId id =
    Http.get
        { url = serverAddr ++ "item/" ++ String.fromInt id ++ ".json"
        , expect = Http.expectJson (RemoteData.fromResult >> GotStory) storyDecoder
        }


getRecentStoryIds : Cmd Msg
getRecentStoryIds =
    Http.get
        { url = serverAddr ++ "topstories.json"
        , expect = Http.expectJson (RemoteData.fromResult >> GotStoryIds) storyIdsDecoder
        }


storyIdsDecoder : Decoder (List Int)
storyIdsDecoder =
    Json.Decode.list Json.Decode.int


storyDecoder : Decoder Story
storyDecoder =
    Json.Decode.succeed Story
        |> Json.Decode.Pipeline.required "title" string
        |> Json.Decode.Pipeline.required "by" string
        |> Json.Decode.Pipeline.required "score" int
        |> Json.Decode.Pipeline.optional "kids" (Json.Decode.list Json.Decode.int) []
        |> Json.Decode.Pipeline.required "url" string
        |> Json.Decode.Pipeline.required "id" int
