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



-- ROUTES ######################################################################


route : Model -> Parser (Page -> a) a
route model =
    oneOf
        [ Url.map (toFullSizePage model) (s "portfolio" </> s "full" <?> Query.string "image")
        , Url.map Thumbnails (s "portfolio" </> top)
        , Url.map (Home Nothing) top
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
    Builder.absolute [] []


thumbnailsUrl : String
thumbnailsUrl =
    Builder.absolute [ "portfolio" ] []


fullSizeUrl : String -> String
fullSizeUrl imageSrc =
    Builder.absolute [ "portfolio", "full" ] [ Builder.string "image" imageSrc ]


aboutUrl : String
aboutUrl =
    Builder.absolute [ "about" ] []


resumeUrl : String
resumeUrl =
    Builder.absolute [ "resume" ] []


assetUrl : String -> String
assetUrl src =
    Builder.absolute [ "assets", src ] []


queryUrl : String
queryUrl =
    Builder.crossOrigin "http://andrewdilmore.com" [ "query" ] []



-- MODEL #######################################################################


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
    , device : Device
    }


type Page
    = Home (Maybe String)
    | Thumbnails
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


init : { viewport : { width : Int, height : Int } } -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialModel =
            { key = key
            , viewport = toViewport flags.viewport
            , page = Home Nothing
            , dirListing = NotAsked
            , filterPath = FilterPath "images/" []
            }
    in
    ( { initialModel | page = toPage initialModel url }
    , requestDirListing
    )


toViewport dimensions =
    { width = dimensions.width
    , height = dimensions.height
    , device = classifyDevice dimensions
    }



-- FUNCTIONS FOR DIRECTORY AND FILTER ##########################################


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



-- UPDATE ######################################################################


type Msg
    = NoOp
    | WindowResize Int Int
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | MouseOverLink String
    | MouseLeaveLink
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
            let
                newViewport =
                    { width = width
                    , height = height
                    , device = classifyDevice { width = width, height = height }
                    }
            in
            ( { model | viewport = newViewport }, Cmd.none )

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

        MouseOverLink x ->
            let
                newPage =
                    case model.page of
                        Home _ ->
                            Home (Just x)

                        _ ->
                            model.page
            in
            ( { model | page = newPage }, Cmd.none )

        MouseLeaveLink ->
            let
                newPage =
                    case model.page of
                        Home _ ->
                            Home Nothing

                        _ ->
                            model.page
            in
            ( { model | page = newPage }, Cmd.none )

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



-- SUBSCRIPTIONS ###############################################################


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowResize



-- VIEW #######################################################################
-- GENERAL


view : Model -> Browser.Document Msg
view model =
    { title = "Andrew Dilmore"
    , body =
        [ bodyElement model
            |> Element.layout []
        ]
    }


bodyElement : Model -> Element Msg
bodyElement model =
    case model.page of
        Home selected ->
            homeElement model.viewport selected

        Thumbnails ->
            usualBody (thumbnailListElement model)

        FullSize data ->
            usualBody (fullSizeElement model.viewport data)

        NotFound ->
            text "Not found"


usualBody : Element Msg -> Element Msg
usualBody content =
    column
        [ width fill, height fill, spacing 10 ]
        [ siteHeader
        , content
        , siteFooter
        ]
        |> el [ width fill, height fill, withBackground ]


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
        [ height (fill |> maximum 600), width fill, clip ]
        [ leftImage
        , rightImage
        ]
        |> behindContent


siteHeader : Element Msg
siteHeader =
    row
        [ alignTop
        , width fill
        , spacing 10
        , paddingXY 2 10
        , Background.color (rgb255 200 200 200)
        ]
        [ nameElement "ANDREW DILMORE"
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
        ([ Font.color (rgb255 0 0 0)
         , Font.size (scaled 4)
         , padding 10
         ]
            ++ futuraBold
        )
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



-- HOME


homeElement : Viewport -> Maybe String -> Element Msg
homeElement viewport selected =
    case classifySimpleDevice viewport.device of
        FullDesktop ->
            homeDesktopElement viewport selected

        Mobile ->
            homeMobileElement viewport selected



-- Desktop


homeDesktopElement : Viewport -> Maybe String -> Element Msg
homeDesktopElement viewport selected =
    row [ height fill, width fill ]
        [ homeLinkListElement viewport selected
        , homeDividerElement
        , homePageGraphicElement viewport
        ]


homeLinkListElement : Viewport -> Maybe String -> Element Msg
homeLinkListElement viewport selected =
    let
        fontSize =
            case viewport.device.class of
                BigDesktop ->
                    6

                _ ->
                    5
    in
    column
        ([ width (fillPortion 1)
         , centerY
         , centerX
         , spacing 40
         , Font.size (scaled fontSize)
         ]
            ++ futuraMedium
        )
        [ homeLinkElement viewport "Portfolio" thumbnailsUrl selected
        , homeLinkElement viewport "About" aboutUrl selected
        , homeLinkElement viewport "Resume" resumeUrl selected
        ]


homeLinkElement : Viewport -> String -> String -> Maybe String -> Element Msg
homeLinkElement viewport linkName url selected =
    let
        isSelected =
            Just linkName == selected

        fontAttributes =
            if isSelected then
                futuraBold

            else
                futuraMedium
    in
    link ([ centerX ] ++ fontAttributes ++ withHomeLinkArrows viewport isSelected)
        { url = url
        , label = text linkName
        }
        |> el
            [ centerX
            , Events.onMouseEnter (MouseOverLink linkName)
            , Events.onMouseLeave MouseLeaveLink
            , width (px 200)
            ]


withHomeLinkArrows : Viewport -> Bool -> List (Attribute Msg)
withHomeLinkArrows viewport isSelected =
    let
        verticalOffset =
            case viewport.device.class of
                BigDesktop ->
                    7

                _ ->
                    19

        correctScale =
            case viewport.device.class of
                BigDesktop ->
                    0.7

                _ ->
                    0.5

        withLeftArrow =
            image
                [ rotate 3.14159
                , moveUp verticalOffset
                , scale correctScale
                , centerY
                ]
                { src = assetUrl "Rollover_button_1.png", description = "" }
                |> onLeft

        withRightArrow =
            image
                [ moveUp verticalOffset
                , scale correctScale
                , centerY
                ]
                { src = assetUrl "Rollover_button_2.png", description = "" }
                |> onRight
    in
    if isSelected then
        [ withLeftArrow, withRightArrow ]

    else
        []


withHomeUnderline : Viewport -> Bool -> Attribute Msg
withHomeUnderline viewport isSelected =
    let
        correctScale =
            case viewport.device.class of
                BigDesktop ->
                    1

                _ ->
                    0.65

        underlineElement =
            if isSelected then
                image
                    [ centerX
                    , scale correctScale
                    ]
                    { src = assetUrl "Underline.png", description = "" }

            else
                none
    in
    underlineElement
        |> below


homeDividerElement : Element msg
homeDividerElement =
    el [ height fill, width (px 5), Background.color colors.black ] none


homePageGraphicElement : Viewport -> Element msg
homePageGraphicElement viewport =
    image
        [ centerY
        , centerX
        , height (fill |> maximum viewport.height)
        , width shrink
        ]
        { src = assetUrl "HomePageGraphic.png"
        , description = ""
        }
        |> el
            [ width (fillPortion 2 |> maximum viewport.width)
            , height (fill |> maximum viewport.height)
            , clipX
            , withHomeName viewport
            ]


withHomeName : Viewport -> Attribute msg
withHomeName viewport =
    homeNameElement viewport
        |> inFront


homeNameElement : Viewport -> Element msg
homeNameElement viewport =
    let
        fontSize =
            break { break = 775, low = 6, high = 7 } viewport.width

        lineSpacing =
            break { break = 7, low = -15, high = -25 } fontSize
    in
    [ paragraph [ centerX, Font.center ] [ text "ANDREW" ]
    , paragraph [ centerX, Font.center ] [ text "DILMORE" ]
    ]
        |> textColumn
            ([ alignBottom
             , centerX
             , moveUp 20
             , Font.size (scaled fontSize)
             , Font.letterSpacing 10
             , spacing lineSpacing
             ]
                ++ futuraBold
            )



-- Mobile


homeMobileElement : Viewport -> Maybe String -> Element Msg
homeMobileElement viewport selected =
    column [ height fill, width fill, withHomeBackground viewport ]
        [ homeNameMobileElement viewport
        , homeLinkListMobileElement viewport selected
        ]


withHomeBackground : Viewport -> Attribute msg
withHomeBackground viewport =
    image
        [ centerY
        , centerX
        , height (fill |> maximum viewport.height)
        ]
        { src = assetUrl "HomePageGraphic.png"
        , description = ""
        }
        |> el
            [ width fill
            , moveLeft 10
            , height (fill |> maximum viewport.height)
            , clipX
            ]
        |> behindContent


homeNameMobileElement : Viewport -> Element msg
homeNameMobileElement viewport =
    let
        fontSize =
            break { break = 500, low = 6, high = 7 } viewport.width

        lineSpacing =
            break { break = 7, low = -10, high = -25 } fontSize
    in
    [ paragraph [ centerX, Font.center ] [ text "ANDREW" ]
    , paragraph [ centerX, Font.center ] [ text "DILMORE" ]
    ]
        |> textColumn
            ([ alignBottom
             , width (px viewport.width)
             , clip
             , centerX
             , moveUp (toFloat ((3 * viewport.height // 20) - 55))
             , Font.size (scaled fontSize)
             , Font.letterSpacing 10
             , spacing lineSpacing
             ]
                ++ futuraBold
            )


homeLinkListMobileElement : Viewport -> Maybe String -> Element Msg
homeLinkListMobileElement viewport selected =
    column
        ([ width (fillPortion 1)
         , centerY
         , centerX
         , spacing ((3 * viewport.height // 100) - 11)
         , Font.size
            (scaled
                (break
                    { break = 700
                    , low = 5
                    , high = 6
                    }
                    viewport.height
                )
            )
         , moveUp (toFloat ((3 * viewport.height // 50) - 2))
         ]
            ++ futuraMedium
        )
        [ homeLinkMobileElement "Portfolio" thumbnailsUrl selected
        , homeLinkMobileElement "About" aboutUrl selected
        , homeLinkMobileElement "Resume" resumeUrl selected
        ]


homeLinkMobileElement : String -> String -> Maybe String -> Element Msg
homeLinkMobileElement linkName url selected =
    let
        fontAttributes =
            if Just linkName == selected then
                futuraBold

            else
                futuraMedium
    in
    link ([] ++ fontAttributes)
        { url = url
        , label = text linkName
        }
        |> el
            [ centerX
            , Events.onMouseEnter (MouseOverLink linkName)
            , Events.onMouseLeave MouseLeaveLink
            ]



-- PORTFOLIO
-- Thumbnail List


thumbnailListElement : Model -> Element Msg
thumbnailListElement model =
    case classifySimpleDevice model.viewport.device of
        FullDesktop ->
            thumbnailListDesktopElement model

        Mobile ->
            thumbnailListDesktopElement model


thumbnailListDesktopElement : Model -> Element Msg
thumbnailListDesktopElement model =
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



-- Full size image slideshow


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



-- VIEW HELPERS


scaled : Int -> Int
scaled =
    round << modular 5.5 1.62


break : { break : number, high : a, low : a } -> number -> a
break spec value =
    if value < spec.break then
        spec.low

    else
        spec.high


loaderElement : Element msg
loaderElement =
    Html.div [ Html.Attributes.class "loader loader-bouncing is-active" ] []
        |> Element.html


type SimpleDeviceClass
    = FullDesktop
    | Mobile


classifySimpleDevice : Device -> SimpleDeviceClass
classifySimpleDevice device =
    case device.orientation of
        Landscape ->
            FullDesktop

        Portrait ->
            case device.class of
                Phone ->
                    Mobile

                Tablet ->
                    Mobile

                Desktop ->
                    FullDesktop

                BigDesktop ->
                    FullDesktop



-- FONTS


futura : Maybe String -> Int -> List (Attribute msg)
futura postfix weightNumber =
    let
        typefaceName =
            postfix
                |> Maybe.map (\postfix_ -> "-" ++ postfix_)
                |> Maybe.withDefault ""
                |> (\postfix_ -> "futura-pt" ++ postfix_)

        fontWeight =
            case weightNumber of
                100 ->
                    Font.hairline

                200 ->
                    Font.extraLight

                300 ->
                    Font.light

                400 ->
                    Font.regular

                500 ->
                    Font.medium

                600 ->
                    Font.semiBold

                700 ->
                    Font.bold

                800 ->
                    Font.extraBold

                900 ->
                    Font.heavy

                _ ->
                    Font.medium
    in
    [ Font.family
        [ Font.typeface typefaceName
        , Font.sansSerif
        ]
    , fontWeight
    ]


futuraBook =
    futura Nothing 400


futuraMedium =
    futura Nothing 500


futuraHeavy =
    futura Nothing 700


futuraCondensedBook =
    futura (Just "condensed") 400


futuraCondensedBold =
    futura (Just "condensed") 700


futuraBold =
    futura (Just "bold") 700



-- REQUESTS ####################################################################


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
            Decode.field "files" (Decode.map (List.map (\f -> "/" ++ f)) (Decode.list Decode.string))

        decodeSubDirs =
            Decode.field "subdirs" (Decode.list Decode.string)
    in
    Decode.map2 DirData decodeFiles decodeSubDirs



-- UTILITY #####################################################################
-- CONSTANTS


rowLength =
    3



-- PURE UTILITY


colors =
    { lightGrey = rgb 0.8 0.8 0.8
    , black = rgb 0 0 0
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
