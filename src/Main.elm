module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Navigation
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
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import SelectList exposing (SelectList)
import Task


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { page : Page
    , dirListing : DirListing
    , filterPath : FilterPath
    }


type Page
    = Thumbnails
    | FullSize FullSizeData


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


init : Model
init =
    { page = Thumbnails
    , dirListing = defaultListing
    , filterPath = FilterPath "images" []
    }


defaultListing : DirListing
defaultListing =
    Dict.fromList
        [ ( "images", DirData [] [ "images/Logos", "images/Adverts", "images/Composition" ] )
        , ( "images/Logos", DirData [] [ "images/Logos/Brass Hats", "images/Logos/Orchid" ] )
        , ( "images/Logos/Brass Hats", DirData [ "images/Logos/Brass Hats/Brass Hats Logo.png" ] [] )
        , ( "images/Logos/Orchid", DirData [ "images/Logos/Orchid/Orchid Resort Branding.png" ] [] )
        , ( "images/Adverts", DirData [ "images/Adverts/Mustache Factory Labels.png", "images/Adverts/Hot Sauce Ad.png", "images/Adverts/Coke ad mockup.png" ] [] )
        , ( "images/Composition", DirData [ "images/Composition/Freedom is not Free.jpg", "images/Composition/Triad House.png", "images/Composition/AzlynProject 4.jpg" ] [] )
        ]



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


makeFullSize : DirListing -> String -> String -> FullSizeData
makeFullSize listing dir current =
    let
        doThing ( start, end ) =
            case end of
                [] ->
                    SelectList.fromList start
                        |> Maybe.withDefault (SelectList.singleton "")

                elements ->
                    List.tail elements
                        |> Maybe.withDefault []
                        |> (\rest -> SelectList.fromLists start current rest)
    in
    listing
        |> filesInDir dir
        |> List.splitWhen (\file -> file == current)
        |> Maybe.withDefault ( [], [] )
        |> doThing


nextImage : FullSizeData -> FullSizeData
nextImage =
    SelectList.attempt (SelectList.selectBy 1)


prevImage : FullSizeData -> FullSizeData
prevImage =
    SelectList.attempt (SelectList.selectBy -1)



-- UPDATE


type Msg
    = NoOp
    | OpenImage String
    | NextImage
    | PrevImage
    | CloseImage


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        OpenImage img ->
            let
                newPage =
                    FullSize (makeFullSize model.dirListing (currentFilter model.filterPath) img)
            in
            { model | page = newPage }

        NextImage ->
            case model.page of
                FullSize data ->
                    { model | page = FullSize (nextImage data) }

                _ ->
                    model

        PrevImage ->
            case model.page of
                FullSize data ->
                    { model | page = FullSize (prevImage data) }

                _ ->
                    model

        CloseImage ->
            { model | page = Thumbnails }



-- VIEW


view : Model -> Html Msg
view model =
    column
        [ width fill, height fill ]
        [ siteHeader
        , pageContent model
        , siteFooter
        ]
        |> Element.layout []


pageContent : Model -> Element Msg
pageContent model =
    case model.page of
        Thumbnails ->
            thumbnailListElement model

        FullSize fullSizeData ->
            fullSizeElement fullSizeData


thumbnailListElement : Model -> Element Msg
thumbnailListElement model =
    model.dirListing
        |> filesInDir (currentFilter model.filterPath)
        |> chunksOf 3
        |> thumbnailColumn


thumbnailColumn : List (List String) -> Element Msg
thumbnailColumn rows =
    rows
        |> List.map thumbnailRow
        |> column [ spacing 20, Background.color debugColors.column ]


thumbnailRow : List String -> Element Msg
thumbnailRow srcs =
    srcs
        |> List.map thumbnailElement
        |> row [ spacing 20, Background.color debugColors.row ]


thumbnailElement : String -> Element Msg
thumbnailElement src =
    image
        [ width (shrink |> maximum 200)
        , centerY
        , centerX
        , Border.solid
        , Border.color debugColors.thumbnail
        , Events.onClick (OpenImage src)
        ]
        { src = src, description = "" }
        |> el
            [ width (px 200)
            , height (px 200)
            , centerY
            , clip
            , Background.color debugColors.el
            ]


fullSizeElement : FullSizeData -> Element Msg
fullSizeElement data =
    let
        leftArrow =
            text "<"
                |> el [ centerY, alignLeft ]
                |> el
                    [ Font.size (scaled 5)
                    , Events.onClick PrevImage
                    , alignLeft
                    , height fill
                    , width (px 30)
                    , Background.color debugColors.el
                    , Background.color colors.lightGrey
                    ]

        rightArrow =
            text ">"
                |> el [ centerY, alignRight ]
                |> el
                    [ Font.size (scaled 5)
                    , Events.onClick NextImage
                    , alignRight
                    , height fill
                    , width (px 30)
                    , Background.color colors.lightGrey
                    ]
    in
    row
        [ centerX
        , width fill
        , height fill
        ]
        [ leftArrow
        , fullSizeImageElement data
        , rightArrow
        ]
        |> el [ width fill, height fill ]


fullSizeImageElement : FullSizeData -> Element Msg
fullSizeImageElement data =
    image
        [ width (shrink |> maximum 1200)
        , height (shrink |> maximum 550)
        , centerX
        , Events.onClick CloseImage
        ]
        { src = SelectList.selected data
        , description = ""
        }


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
        , url = "/"
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



-- PURE UTILITY


colors =
    { lightGrey = rgb 0.8 0.8 0.8
    }


debugColors =
    { row = rgb 1 0 0
    , el = rgb 0 1 0
    , thumbnail = rgb 0 0 1
    , column = rgb 0 0 0
    }


chunksOf : Int -> List a -> List (List a)
chunksOf n list =
    case List.drop n list of
        [] ->
            [ List.take n list ]

        rest ->
            List.take n list :: chunksOf n rest
