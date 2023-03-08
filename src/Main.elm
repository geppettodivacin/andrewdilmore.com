module Main exposing (main)

import Aliases exposing (Album, Collection)
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
import Http exposing (Error(..))
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import RemoteData exposing (RemoteData(..), WebData)
import Sanity
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
        [ Url.map makeFullSize (s "portfolio" </> string </> s "full" </> string)
        , Url.map makeAbout (s "about" </> top)
        , Url.map makeResume (s "resume" </> top)
        , Url.map (makeThumbnails model.pulledCollections) (s "portfolio" </> string)
        , Url.map (Home Nothing) top
        ]


isImageUrl : Url -> Bool
isImageUrl url =
    String.startsWith "/images" url.path


toPage : Model -> Url -> Page
toPage model url =
    Maybe.withDefault NotFound (Url.parse (route model) url)


makeHeaderState selected =
    { selected = selected, hovered = Nothing, displayContact = False }


makeAbout : Page
makeAbout =
    About (makeHeaderState headerTitle.about)


makeResume : Page
makeResume =
    Resume (makeHeaderState headerTitle.resume)


makeThumbnails : WebData (List Collection) -> String -> Page
makeThumbnails collections currentCategory =
    Thumbnails (makeHeaderState headerTitle.portfolio) (thumbnailDataFromSanityCache currentCategory collections)


makeThumbnailData : List CategoryInfo -> String -> ThumbnailData
makeThumbnailData categories currentCategory =
    { currentCategory = currentCategory
    , listing = NotAsked
    , categories = categories
    }


makeFullSize : String -> String -> Page
makeFullSize category resource =
    FullSize (makeHeaderState headerTitle.portfolio)
        { category = category
        , resource = resource
        , contents = NotAsked
        }


homeUrl : String
homeUrl =
    Builder.absolute [] []


thumbnailsUrl : String -> String
thumbnailsUrl category =
    Builder.absolute [ "portfolio", category ] []


fullSizeUrl : String -> String -> String
fullSizeUrl category resource =
    Builder.absolute [ "portfolio", category, "full", resource ] []


aboutUrl : String
aboutUrl =
    Builder.absolute [ "about" ] []


resumeUrl : String
resumeUrl =
    Builder.absolute [ "resume" ] []


assetUrl : String -> String
assetUrl src =
    Builder.absolute [ "assets", src ] []


thumbnailQueryUrl : String -> String
thumbnailQueryUrl category =
    Builder.crossOrigin "https://andrewdilmore.com" [ "query", "thumbnails", category ] []


fullQueryUrl : String -> String -> String
fullQueryUrl category resource =
    Builder.crossOrigin "https://andrewdilmore.com" [ "query", "full", category, resource ] []


imagePrefix : String
imagePrefix =
    "images/"


imageDirectory : String -> String
imageDirectory category =
    imagePrefix ++ category



-- MODEL #######################################################################


type alias Model =
    { key : Navigation.Key
    , viewport : Viewport
    , page : Page
    , pulledCollections : WebData (List Collection)
    }


type alias Viewport =
    { width : Int
    , height : Int
    , sceneWidth : Int
    , sceneHeight : Int
    , device : Device
    }


type Page
    = Home (Maybe String)
    | About HeaderState
    | Resume HeaderState
    | Thumbnails HeaderState ThumbnailData
    | FullSize HeaderState FullSizeData
    | NotFound


type alias HeaderState =
    { selected : String
    , hovered : Maybe String
    , displayContact : Bool
    }


type alias FullSizeData =
    { category : String
    , resource : String
    , contents : WebData PortfolioFolder
    }


type alias PortfolioFolder =
    List PortfolioFile


type PortfolioFile
    = PortfolioImage String
    | PortfolioText String


type alias ThumbnailData =
    { currentCategory : String
    , categories : List CategoryInfo
    , listing : WebData CategoryListing
    }


type alias CategoryListing =
    List ThumbnailInfo


type alias CategoryInfo =
    { category : String
    , label : String
    }


type alias ThumbnailInfo =
    { thumbnail : String
    , resourceQuery : String
    }


init : { viewport : { width : Int, height : Int } } -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialModelWithoutPage : Model
        initialModelWithoutPage =
            { key = key
            , viewport = toViewport flags.viewport
            , page = NotFound
            , pulledCollections = NotAsked
            }

        initialPage =
            toPage initialModelWithoutPage url

        initialModel =
            { initialModelWithoutPage | page = initialPage }
    in
    ( initialModel, Cmd.batch [ requestScene, modelRequests initialModel] )


toViewport dimensions =
    { width = dimensions.width
    , height = dimensions.height
    , sceneWidth = dimensions.width -- initial guess, awaiting response
    , sceneHeight = dimensions.height -- initial guess, awaiting response
    , device = classifyDevice dimensions
    }



-- UPDATE ######################################################################


type Msg
    = NoOp
    | WindowResize Int Int
    | SceneResize Int Int
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | MouseOverLink String
    | MouseLeaveLink
    | EnterContact
    | LeaveContact
    | GotFullResponse (Result Http.Error FullDataResponse)
    | StartSanityRequest
    | GotSanityResponse (Result Http.Error (List Collection))


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
                    , sceneWidth = model.viewport.sceneWidth
                    , sceneHeight = model.viewport.sceneHeight
                    , device = classifyDevice { width = width, height = height }
                    }
            in
            ( { model | viewport = newViewport }, requestScene )

        SceneResize width height ->
            let
                newViewport =
                    { width = model.viewport.width
                    , height = model.viewport.height
                    , sceneWidth = width
                    , sceneHeight = height
                    , device = model.viewport.device
                    }
            in
            ( { model | viewport = newViewport }, Cmd.none )

        ClickedLink request ->
            case request of
                Browser.Internal url ->
                    if isImageUrl url then
                        ( model, Navigation.load (Url.toString url) )

                    else
                        ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ChangedUrl url ->
            let
                page =
                    toPage model url
            in
            ( { model | page = page }, Cmd.batch [ requestScene, pageRequests page ] )

        MouseOverLink newHovered ->
            let
                newPage =
                    updateHovered (Just newHovered) model.page
            in
            ( { model | page = newPage }, Cmd.none )

        MouseLeaveLink ->
            let
                newPage =
                    updateHovered Nothing model.page
            in
            ( { model | page = newPage }, Cmd.none )

        EnterContact ->
            let
                newPage =
                    updateDisplayContact True model.page
            in
            ( { model | page = newPage }, Cmd.none )

        LeaveContact ->
            let
                newPage =
                    updateDisplayContact False model.page
            in
            ( { model | page = newPage }, Cmd.none )

        GotFullResponse result ->
            case model.page of
                FullSize _ data ->
                    let
                        newPage =
                            fullPageFromResponse data.category data.resource result
                    in
                    ( { model | page = newPage }, requestScene )

                _ ->
                    ( model, requestScene )

        StartSanityRequest ->
            let
                newPage =
                    updatePageWithSanityCache Loading model.page
            in
            ( { model | pulledCollections = Loading, page = newPage }, Cmd.none )

        GotSanityResponse result ->
            let
                newPulledCollections =
                    case result of
                        Ok data ->
                            Success data

                        Err error ->
                            Failure error

                newPage =
                    updatePageWithSanityCache newPulledCollections model.page
            in
            ( { model | pulledCollections = newPulledCollections, page = newPage }, Cmd.none )


updateHovered : Maybe String -> Page -> Page
updateHovered hovered page =
    case page of
        Home _ ->
            Home hovered

        About headerState ->
            About { headerState | hovered = hovered }

        Resume headerState ->
            Resume { headerState | hovered = hovered }

        Thumbnails headerState data ->
            Thumbnails { headerState | hovered = hovered } data

        FullSize headerState data ->
            FullSize { headerState | hovered = hovered } data

        NotFound ->
            page


updateDisplayContact : Bool -> Page -> Page
updateDisplayContact displayContact page =
    case page of
        Home _ ->
            page

        About headerState ->
            About { headerState | displayContact = displayContact }

        Resume headerState ->
            Resume { headerState | displayContact = displayContact }

        Thumbnails headerState data ->
            Thumbnails { headerState | displayContact = displayContact } data

        FullSize headerState data ->
            FullSize { headerState | displayContact = displayContact } data

        NotFound ->
            page


updatePageWithSanityCache : WebData (List Collection) -> Page -> Page
updatePageWithSanityCache sanityCache page =
    case page of
        Thumbnails headerState thumbnailData ->
            Thumbnails headerState (thumbnailDataFromSanityCache thumbnailData.currentCategory sanityCache)

        _ ->
            page


thumbnailDataFromSanityCache : String -> WebData (List Collection) -> ThumbnailData
thumbnailDataFromSanityCache currentCategory sanityCache =
    let
        toCategoryInfo : Collection -> CategoryInfo
        toCategoryInfo collection =
            { category = collection.slug
            , label = collection.title
            }
        
        newCategories =
            sanityCache
                |> RemoteData.map (List.map toCategoryInfo)
                |> RemoteData.withDefault []

        newCurrentCategory : String
        newCurrentCategory =
            if currentCategory == "" then
                sanityCache
                    |> RemoteData.map List.head
                    |> RemoteData.andThen (RemoteData.fromMaybe NetworkError)
                    |> RemoteData.map (\collection -> collection.slug)
                    |> RemoteData.withDefault ""
            else
                currentCategory

        toThumbnailInfo : Album -> ThumbnailInfo
        toThumbnailInfo album =
            { thumbnail = album.thumbnailUrl, resourceQuery = album.slug }

        currentCollection : List Collection -> Maybe Collection
        currentCollection sanityCacheData =
            sanityCacheData
                |> List.find (\collection -> collection.slug == newCurrentCategory)

        newListing =
            sanityCache
                |> RemoteData.map currentCollection
                |> RemoteData.andThen (RemoteData.fromMaybe NetworkError)
                |> RemoteData.map (\collection -> List.map toThumbnailInfo collection.albums)
    in
    { categories = newCategories, currentCategory = newCurrentCategory, listing = newListing }

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
    case classifySimpleDevice model.viewport.device of
        FullDesktop ->
            bodyElementDesktop model

        Mobile ->
            bodyElementMobile model


bodyElementDesktop : Model -> Element Msg
bodyElementDesktop model =
    case model.page of
        Home selected ->
            homeDesktopElement model.viewport selected

        About headerState ->
            usualBodyDesktop model.viewport headerState (aboutElement model.viewport)

        Resume headerState ->
            usualBodyDesktop model.viewport headerState (resumeElement model.viewport)

        Thumbnails headerState data ->
            usualBodyDesktop model.viewport headerState (thumbnailListElement model.viewport data)

        FullSize headerState data ->
            usualBodyDesktop
                model.viewport
                headerState
                (fullSizeElement model.viewport data)

        NotFound ->
            notFoundElement


bodyElementMobile : Model -> Element Msg
bodyElementMobile model =
    case model.page of
        Home selected ->
            homeMobileElement model.viewport selected

        About headerState ->
            usualBodyMobile model.viewport headerState (aboutElement model.viewport)

        Resume headerState ->
            usualBodyMobile model.viewport headerState (resumeElement model.viewport)

        Thumbnails headerState data ->
            usualBodyMobile model.viewport headerState (thumbnailListElement model.viewport data)

        FullSize headerState data ->
            usualBodyMobile model.viewport headerState (fullSizeElement model.viewport data)

        NotFound ->
            notFoundElement


usualBodyDesktop viewport headerState content =
    let
        withSidebars mid =
            row [ width fill, height fill ]
                [ sidebarLeft viewport
                , mid
                , sidebarRight viewport
                ]
    in
    column
        [ width (fillPortion 2), height fill, spacing 10 ]
        [ siteHeader headerState
        , content
        , siteFooter
        ]
        |> withSidebars


sidebarLeft : Viewport -> Element msg
sidebarLeft viewport =
    let
        topImage =
            image [ alignLeft, alignTop, width (fill |> maximum 280) ]
                { description = "", src = assetUrl "backgroundSide1.png" }

        bottomImage =
            if viewport.sceneHeight >= 1400 then
                image [ alignLeft, alignBottom, scale -1, width (fill |> maximum 280) ]
                    { description = "", src = assetUrl "backgroundSide0.png" }

            else
                none
    in
    column [ height fill, width fill ]
        [ topImage
        , bottomImage
        ]


sidebarRight : Viewport -> Element msg
sidebarRight viewport =
    let
        topImage =
            image [ alignRight, alignTop, width (fill |> maximum 280) ]
                { description = "", src = assetUrl "backgroundSide0.png" }

        bottomImage =
            if viewport.sceneHeight >= 1400 then
                image [ alignRight, alignBottom, scale -1, width (fill |> maximum 280) ]
                    { description = "", src = assetUrl "backgroundSide1.png" }

            else
                none
    in
    column [ height fill, width fill ]
        [ topImage
        , bottomImage
        ]


usualBodyMobile : Viewport -> HeaderState -> Element Msg -> Element Msg
usualBodyMobile viewport headerState content =
    column
        [ width fill, height fill, spacing 10 ]
        [ siteHeaderMobile viewport headerState
        , content
        , siteFooter
        ]


headerLinkInfo =
    [ { title = "Portfolio", url = thumbnailsUrl "design" }
    , { title = "About", url = aboutUrl }
    , { title = "Resume", url = resumeUrl }
    ]


siteHeader : HeaderState -> Element Msg
siteHeader headerState =
    column
        [ centerX, padding 20, width fill ]
        [ nameElement "ANDREW DILMORE" headerState.displayContact
        , headerLinkInfo
            |> List.map (headerLinkElement headerState)
            |> row [ centerX, spacing 10 ]
        , headerDividerElement
        ]


siteHeaderMobile : Viewport -> HeaderState -> Element Msg
siteHeaderMobile viewport headerState =
    let
        spaceWidth =
            break { break = 890, high = 70, low = 108 } viewport.width
    in
    column
        [ centerX, padding 20 ]
        [ nameElementMobile viewport "ANDREW DILMORE"
        , contactMobileElement viewport
        , headerLinkInfo
            |> List.map (headerLinkElementMobile viewport headerState)
            |> row [ centerX, paddingXY 0 10, spacing spaceWidth ]
        , headerDividerElementMobile
        ]


contactMobileElement : Viewport -> Element Msg
contactMobileElement viewport =
    let
        emailElement =
            row [ alignLeft ]
                [ emailIcon
                , el [ width (px 12) ] none
                , link []
                    { url = "mailto:andrewdilmore@gmail.com"
                    , label = text "andrewdilmore@gmail.com"
                    }
                ]

        phoneElement =
            row [ alignRight ]
                [ phoneIcon
                , el [ width (px 12) ] none
                , link []
                    { url = "tel:1-337-936-2652"
                    , label = text "337-936-2652"
                    }
                ]

        fontSize =
            break { break = 890, low = 4, high = 5 } viewport.width
    in
    row
        [ width fill
        , spacing 30
        , Font.size (scaled fontSize)
        ]
        [ emailElement
        , phoneElement
        ]


headerDividerElement : Element Msg
headerDividerElement =
    let
        dividerHeight =
            5
    in
    image
        [ paddingXY 20 3
        , height (px dividerHeight)
        , width fill
        , centerX
        ]
        { src = assetUrl "gradient.svg", description = "" }


headerDividerElementMobile : Element Msg
headerDividerElementMobile =
    image
        [ padding 3
        , width fill
        , centerX
        ]
        { src = assetUrl "gradient.svg", description = "" }


siteFooter : Element Msg
siteFooter =
    el
        [ alignBottom
        , width fill
        , height (px 30)
        ]
        Element.none


nameElement : String -> Bool -> Element Msg
nameElement name displayContact =
    link
        ([ Font.color colors.black
         , Font.size (scaled 5)
         ]
            ++ futuraBold
        )
        { label = text name
        , url = homeUrl
        }
        |> el
            [ centerX
            , contactInfoElement displayContact |> onRight
            ]


nameElementMobile : Viewport -> String -> Element Msg
nameElementMobile viewport name =
    let
        fontSize =
            break { break = 890, low = 6, high = 7 } viewport.width
    in
    link
        ([ Font.color colors.black
         , Font.size (scaled fontSize)
         ]
            ++ futuraBold
        )
        { label = text name
        , url = homeUrl
        }
        |> el [ centerX ]


contactInfoElement : Bool -> Element Msg
contactInfoElement displayContact =
    let
        padding =
            { top = 5
            , left = 10
            , right = 10
            , bottom = 8
            }

        corners =
            { topLeft = 15
            , topRight = 15
            , bottomLeft = 0
            , bottomRight = 0
            }

        sides =
            { top = 3
            , left = 3
            , right = 3
            , bottom = 0
            }

        email =
            link []
                { url = "mailto:andrewdilmore@gmail.com"
                , label = text "andrewdilmore@gmail.com"
                }

        phone =
            link []
                { url = "tel:1-337-936-2652"
                , label = text "337-936-2652"
                }

        contactElement =
            if displayContact then
                [ email
                , phone
                ]
                    |> column
                        [ paddingEach padding
                        , Font.size (scaled 3)
                        , Font.color colors.white
                        , Border.color colors.black
                        , Background.color (colors.transparentBlack 0.7)
                        , Border.widthEach sides
                        , Border.roundEach corners
                        , backgroundPad |> behindContent
                        ]

            else
                none

        backgroundPad =
            el
                [ width (px 250)
                , height (px 100)
                , moveLeft 35
                , moveUp 30
                ]
                none
    in
    text "Contact"
        |> el
            ([ Font.size (scaled 4)
             , moveRight 19
             , alignBottom
             , Events.onMouseEnter EnterContact
             , Events.onMouseLeave LeaveContact
             , contactElement |> below
             ]
                ++ futuraMedium
            )


headerLinkElement : HeaderState -> { title : String, url : String } -> Element Msg
headerLinkElement headerState content =
    let
        isSelected =
            content.title == headerState.selected

        isHovered =
            Just content.title == headerState.hovered

        fontColor =
            if isHovered then
                colors.black

            else if isSelected then
                colors.paintPurple

            else
                colors.darkGray
    in
    link
        [ Font.color fontColor
        , padding 10
        , Events.onMouseEnter (MouseOverLink content.title)
        , Events.onMouseLeave MouseLeaveLink
        ]
        { label = text content.title
        , url = content.url
        }


headerLinkElementMobile : Viewport -> HeaderState -> { title : String, url : String } -> Element Msg
headerLinkElementMobile viewport headerState content =
    let
        isSelected =
            content.title == headerState.selected

        fontColor =
            if isSelected then
                colors.paintPurple

            else
                colors.darkGray

        fontSize =
            break { break = 890, high = 6, low = 5 } viewport.width
    in
    link
        [ Font.color fontColor
        , Font.size (scaled fontSize)
        ]
        { label = text content.title
        , url = content.url
        }


notFoundElement : Element Msg
notFoundElement =
    text "Not found"



-- HOME
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
        [ homeLinkElement viewport "Portfolio" (thumbnailsUrl "design") selected
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

        correctWidth =
            case viewport.device.class of
                BigDesktop ->
                    55

                _ ->
                    39

        withLeftArrow =
            image
                [ rotate pi
                , moveLeft 10
                , width (px correctWidth)
                , centerY
                ]
                { src = assetUrl "Rollover.png", description = "" }
                |> onLeft

        withRightArrow =
            image
                [ moveRight 10
                , width (px correctWidth)
                , centerY
                ]
                { src = assetUrl "Rollover.png", description = "" }
                |> onRight
    in
    if isSelected then
        [ withLeftArrow, withRightArrow ]

    else
        []


withHomeUnderline : Bool -> Attribute Msg
withHomeUnderline isSelected =
    let
        underlineElement =
            if isSelected then
                image
                    [ centerX
                    , width (px 150)
                    , moveUp 10
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
        [ homeLinkMobileElement "Portfolio" (thumbnailsUrl "design") selected
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



-- ABOUT


aboutElement : Viewport -> Element Msg
aboutElement viewport =
    aboutParagraphElements
        |> textColumn
            ([ centerX
             , paddingXY 20 0
             , width fill
             , spacing 30
             , Font.size (scaled 4)
             ]
                ++ futuraBook
            )


aboutParagraphElements =
    let
        firstSentence =
            [ Font.size (scaled 4 + 3) ] ++ futuraHeavy
    in
    [ [ "I started learning creativity at an early age." |> text |> el firstSentence
      , " Most people do, somewhere between the sticky glue covered hands of the craft table to snacking on Play-Doh in the middle of your next Michelangelo masterpiece. From this creative, expressive little toddler came the much larger, creative, expressive individual making his way into the working world, both as a photographer, designer, and artist." |> text
      ]
        |> paragraph []
    , [ "Most of my creative experience came through my love of Photoshop." |> text |> el firstSentence
      , " I fell in love with the program partway into my college career, prompting me to abandon my old industrial tech degree and pursue Graphic Design. I thrived under my new degree, earning my Bachelor’s and taking my experience into a new professional career. My current position at Decor Steals has me using Photoshop extensively, and my primary job in the company is editing products into various rooms." |> text
      ]
        |> paragraph []
    , [ "Please feel free to take a look at my portfolio." |> text |> el firstSentence
      , " If you feel I would be a good fit for your company, my contact info is listed at the top of my website. Give me a call or shoot me an email! I look forward to hearing from you." |> text
      ]
        |> paragraph []
    , [ "\"Your work is going to fill a large part of your life, and the only way to be truly satisfied is to do what you believe is great work. And the only way to do great work is to love what you do.\"" |> text |> el ([ Font.italic ] ++ firstSentence)
      , " - Steve Jobs" |> text |> el []
      ]
        |> paragraph []
    ]



-- RESUME


resumeElement : Viewport -> Element Msg
resumeElement viewport =
    [ image
        [ width fill
        , paddingXY 0 0
        , centerX
        ]
        { src = assetUrl "ADilmore_Resume.png"
        , description = "Andrew Dilmore's Creative Resume"
        }
    , downloadAs [ centerX ]
        { filename = "ADilmore_Resume.pdf"
        , url = assetUrl "ADilmore_Resume.pdf"
        , label = text "Click to download full-size pdf"
        }
    ]
        |> column [ centerX, spacing 20 ]



-- PORTFOLIO
-- Thumbnail List


thumbnailListElement : Viewport -> ThumbnailData -> Element Msg
thumbnailListElement viewport data =
    let
        localLoaderElement =
            loaderElement
                |> el [ centerX, centerY ]
                |> el [ width fill, height fill ]
    in
    case data.listing of
        Success listing ->
            thumbnailSuccess viewport data.categories data.currentCategory listing

        NotAsked ->
            localLoaderElement

        Loading ->
            localLoaderElement

        Failure error ->
            directoryErrorElement error
                |> el [ centerX, centerY ]


thumbnailSuccess : Viewport -> List CategoryInfo -> String -> CategoryListing -> Element Msg
thumbnailSuccess viewport categories category listing =
    let
        thumbnails =
            listing
                |> chunksOf (rowLength viewport)
                |> thumbnailColumn viewport categories category
    in
    thumbnails


thumbnailColumn : Viewport -> List CategoryInfo -> String -> List (List ThumbnailInfo) -> Element Msg
thumbnailColumn viewport categories currentCategory rows =
    let
        baseWidth =
            1918

        heightAdjustment =
            case currentCategory of
                "design" ->
                    50

                "photography" ->
                    98

                _ ->
                    98

        deviceClass =
            classifySimpleDevice viewport.device

        clickMeLabel =
            image
                [ alignRight
                , moveDown (heightAdjustment + (toFloat viewport.sceneWidth - baseWidth) / 18)
                , moveRight (120 + (toFloat viewport.sceneWidth - baseWidth) / 14)
                , width (197 + ((viewport.sceneWidth - baseWidth) // 9) |> px)
                ]
                { src = assetUrl "clickMe.png", description = "Click me!" }

        insertClickMeLabel =
            List.indexedMap
                (\i row ->
                    if i /= 0 then
                        row

                    else
                        el [ above clickMeLabel ] row
                )

        insertClickMeLabelForDesktop =
            case classifySimpleDevice viewport.device of
                FullDesktop ->
                    insertClickMeLabel

                Mobile ->
                    identity

        insertHeader list =
            portfolioCategoryListElement deviceClass categories currentCategory :: list
    in
    rows
        |> List.map (thumbnailRow currentCategory viewport)
        |> insertClickMeLabelForDesktop
        |> insertHeader
        |> column [ centerX, spacing 20 ]


portfolioCategoryListElement : SimpleDeviceClass -> List CategoryInfo -> String -> Element Msg
portfolioCategoryListElement deviceClass categories currentCategory =
    let
        elementSpacing =
            case deviceClass of
                Mobile ->
                    35

                FullDesktop ->
                    20
    in
    categories
        |> List.map (portfolioCategoryElement deviceClass currentCategory)
        |> row
            [ spacing elementSpacing
            , centerX
            ]


portfolioCategoryElement : SimpleDeviceClass -> String -> CategoryInfo -> Element Msg
portfolioCategoryElement deviceClass currentCategory info =
    let
        isSelected =
            info.category == currentCategory

        fontAttributes =
            if isSelected then
                futuraBold ++ [ Font.color colors.black ]

            else
                futuraMedium ++ [ Font.color colors.darkGray ]

        fontSize =
            case deviceClass of
                Mobile ->
                    scaled 5

                FullDesktop ->
                    scaled 4
    in
    link
        ([ centerX
         , centerY
         , Font.size fontSize
         , mouseOver [ Font.color colors.black ]
         ]
            ++ fontAttributes
        )
        { url = thumbnailsUrl info.category
        , label = text info.label
        }


thumbnailRow : String -> Viewport -> List ThumbnailInfo -> Element Msg
thumbnailRow category viewport thumbnailInfos =
    thumbnailInfos
        |> List.map (thumbnailElement category viewport)
        |> row [ centerX, spacing 20 ]


thumbnailElement : String -> Viewport -> ThumbnailInfo -> Element Msg
thumbnailElement category viewport thumbnailInfo =
    let
        currentRowLength =
            rowLength viewport

        margin =
            case classifySimpleDevice viewport.device of
                Mobile ->
                    viewport.width // 4

                FullDesktop ->
                    100 + (viewport.width // 2)

        sideLength =
            (viewport.width - margin) // rowLength viewport

        imageWidth =
            if currentRowLength > 1 then
                shrink |> maximum sideLength

            else
                px sideLength

        frameHeight =
            if currentRowLength > 1 then
                px sideLength

            else
                shrink
    in
    image
        [ width imageWidth
        , centerY
        , centerX
        ]
        { src = thumbnailInfo.thumbnail, description = "" }
        |> (\img -> link [ centerX, centerY ] { label = img, url = fullSizeUrl category thumbnailInfo.resourceQuery })
        |> el
            [ width (px sideLength)
            , height frameHeight
            , centerY
            , centerX
            , clip
            ]



-- Full size image slideshow


fullSizeElement : Viewport -> FullSizeData -> Element Msg
fullSizeElement viewport data =
    let
        deviceClass =
            classifySimpleDevice viewport.device

        arrowWidth =
            case deviceClass of
                Mobile ->
                    80

                FullDesktop ->
                    50

        arrowImage =
            image [ width (px arrowWidth) ]
                { src = assetUrl "Arrow.png", description = "" }

        leftArrow =
            arrowImage
                |> el
                    [ centerY
                    , centerX
                    , alignLeft
                    , width (px arrowWidth)
                    , height shrink
                    , rotate pi
                    ]
                |> el [ width (px arrowWidth), height fill ]

        backSize =
            case deviceClass of
                Mobile ->
                    scaled 5

                FullDesktop ->
                    scaled 4

        backLink elements =
            link
                ([ alpha 0.6
                 , mouseOver [ alpha 1 ]
                 , Font.size backSize
                 ]
                    ++ futuraBold
                )
                { url = thumbnailsUrl data.category
                , label = row [ spacing 10 ] elements
                }

        backButton =
            [ leftArrow
            , text "Thumbnails"
            ]
                |> backLink
    in
    [ backButton
    , data.contents
        |> RemoteData.map
            (List.map (fullSizeFileElement viewport data.category)
                >> column [ spacing 10 ]
            )
        |> RemoteData.withDefault Element.none
    ]
        |> column
            [ centerX
            , spacing 10
            ]


fullSizeFileElement : Viewport -> String -> PortfolioFile -> Element Msg
fullSizeFileElement viewport category file =
    case file of
        PortfolioImage imageSrc ->
            fullSizeImageElement viewport { category = category, imageSrc = imageSrc }

        PortfolioText _ ->
            Element.none


fullSizeImageElement : Viewport -> { category : String, imageSrc : String } -> Element Msg
fullSizeImageElement viewport data =
    let
        maxWidth =
            case classifySimpleDevice viewport.device of
                FullDesktop ->
                    max
                        (viewport.width - 650)
                        ((viewport.width // 2) - 120)

                Mobile ->
                    viewport.width

        maxHeight =
            viewport.height - 200
    in
    image
        [ width (shrink |> maximum maxWidth)
        , height (shrink |> maximum maxHeight)
        ]
        { src = data.imageSrc
        , description = ""
        }
        |> (\img -> link [ centerX, centerY ] { label = img, url = data.imageSrc })
        |> el [ centerX, centerY, width (px maxWidth) ]



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


emailIcon : Element msg
emailIcon =
    Html.i [ Html.Attributes.class "fas fa-envelope" ] []
        |> Element.html


phoneIcon =
    Html.i [ Html.Attributes.class "fas fa-phone" ] []
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



-- REQUESTS #####################################################################


modelRequests : Model -> Cmd Msg
modelRequests model =
    let
        requestsFromPage =
            pageRequests model.page

        requestsFromSanityCache =
            sanityRequests model.pulledCollections
    in
    Cmd.batch [requestsFromPage, requestsFromSanityCache]


pageRequests : Page -> Cmd Msg
pageRequests page =
    case page of
        FullSize _ data ->
            requestFullData { category = data.category, resource = data.resource }

        _ ->
            Cmd.none


sanityRequests : WebData a -> Cmd Msg
sanityRequests sanityWebData =
    case sanityWebData of
        NotAsked ->
            let
                fetchCollections =
                    Sanity.fetchCollections GotSanityResponse

                startRequest =
                    Task.succeed ()
                        |> Task.perform (always StartSanityRequest)
            in
            Cmd.batch [startRequest, fetchCollections]

        _ ->
            Cmd.none


type alias ThumbnailDataResponse =
    { category : String
    , listing : CategoryListing
    }


type alias FullDataResponse =
    { category : String
    , resource : String
    , contents : PortfolioFolder
    }


requestFullData : { category : String, resource : String } -> Cmd Msg
requestFullData request =
    Http.get
        { url = fullQueryUrl request.category request.resource
        , expect = Http.expectJson GotFullResponse decodeFullData
        }


decodeThumbnailData : Decode.Decoder ThumbnailDataResponse
decodeThumbnailData =
    Decode.map2 ThumbnailDataResponse
        (Decode.field "category" Decode.string)
        (Decode.field "listing" decodeCategoryListing)


decodeCategoryListing : Decode.Decoder CategoryListing
decodeCategoryListing =
    Decode.list decodeThumbnailInfo


decodeThumbnailInfo : Decode.Decoder ThumbnailInfo
decodeThumbnailInfo =
    Decode.map2 ThumbnailInfo
        (Decode.field "thumbnail" Decode.string)
        (Decode.field "resourceQuery" Decode.string)


decodeFullData : Decode.Decoder FullDataResponse
decodeFullData =
    Decode.map3 FullDataResponse
        (Decode.field "category" Decode.string)
        (Decode.field "resource" Decode.string)
        (Decode.field "contents" decodePortfolioFolder)


decodePortfolioFolder : Decode.Decoder PortfolioFolder
decodePortfolioFolder =
    Decode.list decodePortfolioFile


decodePortfolioFile : Decode.Decoder PortfolioFile
decodePortfolioFile =
    let
        decodePortfolioImage =
            Decode.field "type" (decodeSpecific "image")
                |> Decode.andThen
                    (always <|
                        Decode.map PortfolioImage <|
                            Decode.field "src" Decode.string
                    )

        decodePortfolioText =
            Decode.field "type" (decodeSpecific "text")
                |> Decode.andThen
                    (always <|
                        Decode.map PortfolioText <|
                            Decode.field "text" Decode.string
                    )
    in
    Decode.oneOf
        [ decodePortfolioImage
        , decodePortfolioText
        ]


requestScene : Cmd Msg
requestScene =
    Task.perform
        (\viewport ->
            SceneResize
                (round viewport.scene.width)
                (round viewport.scene.height)
        )
        Dom.getViewport


fullPageFromResponse : String -> String -> Result Http.Error FullDataResponse -> Page
fullPageFromResponse category resource response =
    let
        data =
            { category = category
            , resource = resource
            , contents = RemoteData.fromResult (Result.map .contents response)
            }
    in
    FullSize (makeHeaderState headerTitle.portfolio) data



-- UTILITY ######################################################################
-- CONSTANTS


pi =
    3.14159


rowLength : Viewport -> number
rowLength viewport =
    case classifySimpleDevice viewport.device of
        FullDesktop ->
            3

        Mobile ->
            break { break = 890, high = 2, low = 1 } viewport.width


headerTitle =
    { portfolio = "Portfolio"
    , about = "About"
    , resume = "Resume"
    }


colors =
    { lightGray = rgb 0.8 0.8 0.8
    , darkGray = rgb 0.5 0.5 0.5
    , paintPurple = rgb255 136 49 227
    , black = rgb 0 0 0
    , transparentBlack = \a -> rgba 0 0 0 a
    , white = rgb 1 1 1
    }



-- FUNCTIONS


chunksOf : Int -> List a -> List (List a)
chunksOf n list =
    case List.drop n list of
        [] ->
            [ List.take n list ]

        rest ->
            List.take n list :: chunksOf n rest


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


decodeSpecific : String -> Decode.Decoder ()
decodeSpecific expected =
    let
        verify actual =
            if expected == actual then
                Decode.succeed ()

            else
                Decode.fail
                    ("Expected \""
                        ++ expected
                        ++ "\" but got \""
                        ++ actual
                        ++ "\""
                    )
    in
    Decode.string
        |> Decode.andThen verify


directoryErrorElement : Http.Error -> Element msg
directoryErrorElement error =
    textColumn [ spacing 10, width (px 600) ]
        [ paragraph []
            [ text
                """
                I got an error trying to get the list of images. If reloading
                the page does not fix this error, please contact me and let me
                know! A more specific error message can be found below:
                """
            ]
        , text (httpErrorToString error)
        ]
