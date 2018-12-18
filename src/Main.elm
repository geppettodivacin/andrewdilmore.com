module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Debug
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import RemoteData exposing (RemoteData(..), WebData)
import SelectList exposing (SelectList)
import Task
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Url exposing (..)
import Url.Parser.Query as Query


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- ROUTES


route : Model -> Parser (Page -> a) a
route model =
    oneOf
        [ Url.map (toFullSizePage model) (s "full" <?> Query.string "image")
        , Url.map Thumbnails top
        ]


toPage : Model -> Url -> Page
toPage model url =
    Maybe.withDefault NotFound (Url.parse (route model) url)


toFullSizePage : Model -> Maybe String -> Page
toFullSizePage model imageSrc =
    case RemoteData.toMaybe model.dirListing of
        Just dirListing ->
            case Maybe.andThen (makeFullSize dirListing model.filterPath) imageSrc of
                Nothing ->
                    NotFound

                Just data ->
                    FullSize data

        Nothing ->
            case imageSrc of
                Nothing ->
                    NotFound

                Just existingSrc ->
                    FullSize (SelectList.singleton existingSrc)


homeUrl : String
homeUrl =
    thumbnailsUrl


thumbnailsUrl : String
thumbnailsUrl =
    Builder.absolute [] []


fullSizeUrl : String -> String
fullSizeUrl imageSrc =
    Builder.absolute [ "full" ] [ Builder.string "image" imageSrc ]


assetUrl : String -> String
assetUrl src =
    Builder.absolute [ "assets", src ] []


queryUrl : String
queryUrl =
    Builder.crossOrigin "http://andrewdilmore.com" [ "query" ] []



-- MODEL


type alias Model =
    { key : Navigation.Key
    , viewport : Viewport
    , page : Page
    , dirListing : WebData DirListing
    , filterPath : FilterPath
    }


type alias Viewport =
    { width : Int
    , height : Int
    }


type Page
    = Thumbnails
    | FullSize FullSizeData
    | NotFound


type alias FullSizeData =
    SelectList String


type alias DirListing =
    Dict String DirData


type alias DirData =
    { files : List String
    , subDirs : List String
    }


type FilterPath
    = FilterPath String (List String)


init : { viewport : Viewport } -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialModel =
            { key = key
            , viewport = flags.viewport
            , page = Thumbnails
            , dirListing = NotAsked
            , filterPath = FilterPath "images/" []
            }
    in
    ( { initialModel | page = toPage initialModel url }
    , requestDirListing
    )



-- FUNCTIONS FOR DIRECTORY AND FILTER


filesInDir : String -> DirListing -> List String
filesInDir dir listing =
    let
        currentDir =
            listing
                |> Dict.get dir
                |> Maybe.withDefault (DirData [] [])

        subFiles =
            currentDir.subDirs
                |> List.map (\subDir -> filesInDir subDir listing)
                |> List.concat
    in
    currentDir.files ++ subFiles


currentFilter : FilterPath -> String
currentFilter filterPath =
    case filterPath of
        FilterPath filter [] ->
            filter

        FilterPath _ filterList ->
            filterList
                |> List.head
                |> Maybe.withDefault ""


possibleFilters : DirListing -> FilterPath -> List String
possibleFilters dirListing filterPath =
    case Dict.get (currentFilter filterPath) dirListing of
        Nothing ->
            []

        Just dirData ->
            dirData.subDirs


addFilter : String -> FilterPath -> FilterPath
addFilter newFilter filterPath =
    case filterPath of
        FilterPath root filterList ->
            FilterPath root (newFilter :: filterList)


removeFilter : Int -> FilterPath -> FilterPath
removeFilter count filterPath =
    case filterPath of
        FilterPath root filterList ->
            FilterPath root (List.drop count filterList)


makeFullSize : DirListing -> FilterPath -> String -> Maybe FullSizeData
makeFullSize listing filter current =
    let
        doThing ( start, end ) =
            case end of
                [] ->
                    Nothing

                head :: tail ->
                    Just (SelectList.fromLists start head tail)
    in
    listing
        |> filesInDir (currentFilter filter)
        |> List.splitWhen (\file -> file == current)
        |> Maybe.andThen doThing


nextImage : Navigation.Key -> FullSizeData -> ( FullSizeData, Cmd Msg )
nextImage key data =
    let
        newData =
            SelectList.attempt (SelectList.selectBy 1) data

        newUrl =
            newData
                |> SelectList.selected
                |> fullSizeUrl
    in
    ( newData, Navigation.pushUrl key newUrl )


prevImage : Navigation.Key -> FullSizeData -> ( FullSizeData, Cmd Msg )
prevImage key data =
    let
        newData =
            SelectList.attempt (SelectList.selectBy -1) data

        newUrl =
            newData
                |> SelectList.selected
                |> fullSizeUrl
    in
    ( newData, Navigation.pushUrl key newUrl )



-- UPDATE


type Msg
    = NoOp
    | WindowResize Int Int
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotDirListing (Result Http.Error DirListing)
    | AddFilter String
    | RemoveFilters Int
    | OpenImage String
    | NextImage
    | PrevImage
    | CloseImage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowResize width height ->
            ( { model | viewport = { width = width, height = height } }, Cmd.none )

        ClickedLink request ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ChangedUrl url ->
            let
                page =
                    toPage model url
            in
            ( { model | page = page }, Cmd.none )

        GotDirListing result ->
            let
                newDirListing =
                    RemoteData.fromResult (Debug.log "Result" result)

                newModel =
                    { model | dirListing = newDirListing }

                newPage =
                    case newModel.page of
                        FullSize dirListing ->
                            toFullSizePage
                                newModel
                                (Just (SelectList.selected dirListing))

                        _ ->
                            newModel.page
            in
            ( { newModel | page = newPage }, Cmd.none )

        AddFilter newFilter ->
            ( model, Cmd.none )

        RemoveFilters count ->
            ( model, Cmd.none )

        OpenImage img ->
            let
                newPage =
                    toFullSizePage model (Just img)
            in
            ( { model | page = newPage }, Cmd.none )

        NextImage ->
            case model.page of
                FullSize data ->
                    let
                        ( newData, cmd ) =
                            nextImage model.key data

                        newPage =
                            FullSize newData
                    in
                    ( { model | page = newPage }, cmd )

                _ ->
                    ( model, Cmd.none )

        PrevImage ->
            case model.page of
                FullSize data ->
                    let
                        ( newData, cmd ) =
                            prevImage model.key data

                        newPage =
                            FullSize newData
                    in
                    ( { model | page = newPage }, cmd )

                _ ->
                    ( model, Cmd.none )

        CloseImage ->
            ( { model | page = Thumbnails }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowResize



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Andrew Dilmore"
    , body =
        [ column
            [ width fill, height fill, spacing 10 ]
            [ siteHeader
            , pageContent model
            , siteFooter
            ]
            |> el [ width fill, height fill, withBackground ]
            |> Element.layout []
        ]
    }


withBackground : Attribute Msg
withBackground =
    let
        leftImage =
            image [ alignLeft, moveLeft 260 ]
                { description = "", src = assetUrl "backgroundSide.png" }

        rightImage =
            image [ alignRight, moveRight 260 ]
                { description = "", src = assetUrl "backgroundSide.png" }
    in
    row
        [ height fill, width fill, clip ]
        [ leftImage
        , rightImage
        ]
        |> behindContent


pageContent : Model -> Element Msg
pageContent model =
    case model.page of
        Thumbnails ->
            thumbnailListElement model

        FullSize fullSizeData ->
            fullSizeElement model.viewport fullSizeData

        NotFound ->
            el [ centerX ] (text "Not found, buddy")


thumbnailListElement : Model -> Element Msg
thumbnailListElement model =
    let
        localLoaderElement =
            loaderElement
                |> el [ centerX, centerY ]
                |> el [ width fill, height fill ]
    in
    case model.dirListing of
        Success dirListing ->
            dirListing
                |> filesInDir (currentFilter model.filterPath)
                |> chunksOf rowLength
                |> thumbnailColumn model.viewport

        NotAsked ->
            localLoaderElement

        Loading ->
            localLoaderElement

        Failure error ->
            errorElement error
                |> el [ centerX, centerY ]


thumbnailColumn : Viewport -> List (List String) -> Element Msg
thumbnailColumn viewport rows =
    rows
        |> List.map (thumbnailRow viewport)
        |> column [ centerX, spacing 20 ]


thumbnailRow : Viewport -> List String -> Element Msg
thumbnailRow viewport srcs =
    srcs
        |> List.map (thumbnailElement viewport)
        |> row [ spacing 20 ]


thumbnailElement : Viewport -> String -> Element Msg
thumbnailElement viewport src =
    let
        sideLength =
            (viewport.width - 700) // rowLength
    in
    image
        [ width (shrink |> maximum sideLength)
        , centerY
        , centerX
        , Border.solid
        ]
        { src = src, description = "" }
        |> (\img -> link [ centerY ] { label = img, url = fullSizeUrl src })
        |> el
            [ width (px sideLength)
            , height (px sideLength)
            , centerY
            , clip
            ]


fullSizeElement : Viewport -> FullSizeData -> Element Msg
fullSizeElement viewport data =
    let
        leftArrow =
            text "<"
                |> el [ centerY, centerX ]
                |> el
                    [ Font.size (scaled 5)
                    , Events.onClick PrevImage
                    , alignLeft
                    , height fill
                    , width (px 50)
                    , pointer
                    , alpha 0.5
                    , mouseOver [ alpha 0.7 ]
                    ]

        rightArrow =
            text ">"
                |> el [ centerY, centerX ]
                |> el
                    [ Font.size (scaled 5)
                    , Events.onClick NextImage
                    , alignRight
                    , height fill
                    , width (px 50)
                    , pointer
                    , alpha 0.5
                    , mouseOver [ alpha 0.7 ]
                    ]
    in
    row
        [ centerX
        , width fill
        , height fill
        ]
        [ if not (SelectList.isHead data) then
            leftArrow

          else
            none
        , fullSizeImageElement viewport data
        , if not (SelectList.isLast data) then
            rightArrow

          else
            none
        ]
        |> el [ width fill, height fill ]


fullSizeImageElement : Viewport -> FullSizeData -> Element Msg
fullSizeImageElement viewport data =
    image
        [ width (shrink |> maximum (viewport.width - 614))
        , height (shrink |> maximum (viewport.height - 125))
        , centerX
        ]
        { src = SelectList.selected data
        , description = ""
        }
        |> (\img -> link [ centerX ] { label = img, url = thumbnailsUrl })


siteHeader : Element Msg
siteHeader =
    row
        [ alignTop
        , width fill
        , spacing 10
        , paddingXY 2 10
        , Background.color (rgb255 200 200 200)
        ]
        [ nameElement "Andrew Dilmore"
        ]


siteFooter : Element Msg
siteFooter =
    el
        [ alignBottom
        , width fill
        , spacing 10
        , paddingXY 2 10
        , Background.color (rgb255 200 200 200)
        , height (px 30)
        ]
        Element.none


nameElement : String -> Element msg
nameElement name =
    link
        [ Font.color (rgb255 0 0 0)
        , Font.size (scaled 3)
        , padding 10
        ]
        { label = text name
        , url = homeUrl
        }


headerButtonElement : { title : String, url : String } -> Element msg
headerButtonElement content =
    link
        [ Background.color (rgb255 100 100 100)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , Font.size (scaled 2)
        , padding 10
        ]
        { label = text content.title
        , url = content.url
        }


scaled : Int -> Int
scaled =
    round << modular 16 1.25



-- UGLY VIEW HELPERS


loaderElement : Element msg
loaderElement =
    Html.div [ Html.Attributes.class "loader loader-bouncing is-active" ] []
        |> Element.html



-- REQUESTS


requestDirListing : Cmd Msg
requestDirListing =
    Http.get { url = queryUrl, expect = Http.expectJson GotDirListing decodeDirListing }


decodeDirListing : Decode.Decoder DirListing
decodeDirListing =
    Decode.dict decodeDirData


decodeDirData : Decode.Decoder DirData
decodeDirData =
    let
        decodeFiles =
            Decode.field "files" (Decode.list Decode.string)

        decodeSubDirs =
            Decode.field "subdirs" (Decode.list Decode.string)
    in
    Decode.map2 DirData decodeFiles decodeSubDirs



-- CONSTANTS


rowLength =
    3



-- PURE UTILITY


colors =
    { lightGrey = rgb 0.8 0.8 0.8
    }


debugColors =
    {}


chunksOf : Int -> List a -> List (List a)
chunksOf n list =
    case List.drop n list of
        [] ->
            [ List.take n list ]

        rest ->
            List.take n list :: chunksOf n rest


errorElement : Http.Error -> Element msg
errorElement error =
    textColumn [ spacing 10, width (px 600) ]
        [ paragraph []
            [ text
                """
                I got an error trying to get the list of images. You should
                try to reload the page first. If you still have problems, you
                can email the website developer, Eric Dilmore, at
                ericdilmore@gmail.com. Please include the error message below:
                """
            ]
        , text (httpErrorToString error)
        ]


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "I tried to send an HTTP request to a bad url: " ++ url ++ "."

        Http.Timeout ->
            "My request for information took too long."

        Http.NetworkError ->
            "I couldn't communicate with the network properly."

        Http.BadStatus status ->
            "The server I requested data from rejected us with this status code: " ++ String.fromInt status ++ "."

        Http.BadBody jsonError ->
            "I didn't understand the response I got from the server. My JSON decoder says:\n" ++ jsonError
