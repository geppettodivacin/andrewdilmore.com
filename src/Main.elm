module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
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
        , Url.map makeAbout (s "about" </> top)
        , Url.map makeResume (s "resume" </> top)
        , Url.map makeThumbnails (s "portfolio" </> top)
        , Url.map (Home Nothing) top
        ]


toPage : Model -> Url -> Page
toPage model url =
    Maybe.withDefault NotFound (Url.parse (route model) url)


toFullSizePage : Model -> Maybe String -> Page
toFullSizePage model imageSrc =
    case RemoteData.toMaybe model.dirListing of
        Just dirListing ->
            case Maybe.andThen (makeFullSizeData dirListing model.filterPath) imageSrc of
                Nothing ->
                    NotFound

                Just data ->
                    makeFullSize data

        Nothing ->
            case imageSrc of
                Nothing ->
                    NotFound

                Just existingSrc ->
                    makeFullSize (SelectList.singleton existingSrc)


makeHeaderState selected =
    { selected = selected, hovered = Nothing, displayContact = False }


makeAbout =
    About (makeHeaderState "About")


makeResume =
    Resume (makeHeaderState "Resume")


makeThumbnails =
    Thumbnails (makeHeaderState "Portfolio")


makeFullSize =
    FullSize (makeHeaderState "Portfolio")


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
    Builder.crossOrigin "https://andrewdilmore.com" [ "query" ] []



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
    , sceneWidth : Int
    , sceneHeight : Int
    , device : Device
    }


type Page
    = Home (Maybe String)
    | About HeaderState
    | Resume HeaderState
    | Thumbnails HeaderState
    | FullSize HeaderState FullSizeData
    | NotFound


type alias HeaderState =
    { selected : String
    , hovered : Maybe String
    , displayContact : Bool
    }


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
    , Cmd.batch
        [ requestDirListing
        , requestScene
        ]
    )


toViewport dimensions =
    { width = dimensions.width
    , height = dimensions.height
    , sceneWidth = dimensions.width
    , sceneHeight = dimensions.height
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


makeFullSizeData : DirListing -> FilterPath -> String -> Maybe FullSizeData
makeFullSizeData listing filter current =
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
    | SceneResize Int Int
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | MouseOverLink String
    | MouseLeaveLink
    | EnterContact
    | LeaveContact
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
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ChangedUrl url ->
            let
                page =
                    toPage model url

                oldViewport =
                    model.viewport

                newViewport =
                    { oldViewport
                        | sceneWidth = oldViewport.width
                        , sceneHeight = oldViewport.height
                    }
            in
            ( { model | page = page }, requestScene )

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

        GotDirListing result ->
            let
                newDirListing =
                    RemoteData.fromResult result

                newModel =
                    { model | dirListing = newDirListing }

                newPage =
                    case newModel.page of
                        FullSize _ dirListing ->
                            toFullSizePage
                                newModel
                                (Just (SelectList.selected dirListing))

                        _ ->
                            newModel.page
            in
            ( { newModel | page = newPage }, requestScene )

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
                FullSize _ data ->
                    let
                        ( newData, cmd ) =
                            nextImage model.key data

                        newPage =
                            makeFullSize newData
                    in
                    ( { model | page = newPage }, cmd )

                _ ->
                    ( model, Cmd.none )

        PrevImage ->
            case model.page of
                FullSize _ data ->
                    let
                        ( newData, cmd ) =
                            prevImage model.key data

                        newPage =
                            makeFullSize newData
                    in
                    ( { model | page = newPage }, cmd )

                _ ->
                    ( model, Cmd.none )

        CloseImage ->
            ( model, Navigation.pushUrl model.key thumbnailsUrl )


updateHovered : Maybe String -> Page -> Page
updateHovered hovered page =
    case page of
        Home _ ->
            Home hovered

        About headerState ->
            About { headerState | hovered = hovered }

        Resume headerState ->
            Resume { headerState | hovered = hovered }

        Thumbnails headerState ->
            Thumbnails { headerState | hovered = hovered }

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

        Thumbnails headerState ->
            Thumbnails { headerState | displayContact = displayContact }

        FullSize headerState data ->
            FullSize { headerState | displayContact = displayContact } data

        NotFound ->
            page



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

        Thumbnails headerState ->
            usualBodyDesktop model.viewport headerState (thumbnailListDesktopElement model)

        FullSize headerState data ->
            usualBodyDesktop
                model.viewport
                headerState
                (fullSizeElement model.viewport data)

        NotFound ->
            text "Not found"


bodyElementMobile : Model -> Element Msg
bodyElementMobile model =
    case model.page of
        Home selected ->
            homeMobileElement model.viewport selected

        About headerState ->
            usualBodyMobile model.viewport headerState (aboutElement model.viewport)

        Resume headerState ->
            usualBodyMobile model.viewport headerState (resumeElement model.viewport)

        Thumbnails headerState ->
            usualBodyMobile model.viewport headerState (thumbnailListMobileElement model)

        FullSize headerState data ->
            usualBodyMobile model.viewport headerState (thumbnailListMobileElement model)

        NotFound ->
            text "Not found"


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
    [ { title = "Portfolio", url = thumbnailsUrl }
    , { title = "About", url = aboutUrl }
    , { title = "Resume", url = resumeUrl }
    ]


siteHeader : HeaderState -> Element Msg
siteHeader headerState =
    column
        [ centerX, padding 20 ]
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
        toParagraph attributes paragraphText =
            paragraphText
                |> text
                |> List.singleton
                |> paragraph attributes
    in
    [ [ """
        I started learning creativity at an early age. Most people do,
        somewhere between the sticky glue-covered hands and eating Play-Doh.
        """
            |> text
      , """
        “Music, writing, and art have inspired me greatly throughout my life
        and influence who I am as a person,”
        """
            |> text
            |> el [ Font.italic ]
      , """ is what everyone writes on their cover letters, and I'm no exception
        to the rule. But we're not here to talk about what makes me similar to
        your other applicants.
        """
            |> text
      ]
        |> paragraph [ width fill ]
        |> List.singleton
        |> textColumn [ width fill ]
    , [ "We want to know"
            |> toParagraph []
      , "WHAT MAKES ME DIFFERENT FROM EVERYONE ELSE?"
            |> toParagraph [ Font.size (scaled 5) ]
      ]
        |> textColumn [ width fill ]
    , """
    After high school, I started working on a degree for Industrial Technology.
    I spent four years expecting to join an industrial firm and work on
    blueprints for a living. During that time, I went through multiple
    depressive states, and I learned more about myself than I ever would have
    learned about welding and OSHA. I would never be able to put my best foot
    forward in a career path I consider to be just “okay.”
    """
        |> toParagraph []
    , """
    You cannot truly be satisfied with the work you accomplish if you don't
    truly love the work you do.
    """
        |> toParagraph ([ Font.size (scaled 5) ] ++ futuraHeavy)
    , [ """
        After a year of soul searching, I dedicated myself to a career path in art
        and graphic design. My GPA and mental state instantly improved, and I've
        now graduated with a BA in Art. I've spent the last year focusing on
        typography, layout, photography, and photo composition. My images have
        appeared in official McNeese State University publications as well as in
        """
            |> text
      , "American Press" |> text |> el [ Font.italic ]
      , "," |> text
      , "Thrive" |> text |> el [ Font.italic ]
      , " magazine,"
            |> text
      , """
        and the Lake Charles Southwest Louisiana Convention and Visitors Bureau
        website.
        """
            |> text
      ]
        |> paragraph []
        |> List.singleton
        |> textColumn [ width fill ]
    , """
    Please take a look at my portfolio. If you feel I would be a good fit for
    your company, my contact info is listed in the header. Give me a call —
    shoot me an email! I look forward to hearing from you.
    """
        |> toParagraph []
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
            ((viewport.width // 2) - 100) // rowLength
    in
    image
        [ width (shrink |> maximum sideLength)
        , centerY
        , centerX
        ]
        { src = src, description = "" }
        |> (\img -> link [ centerY ] { label = img, url = fullSizeUrl src })
        |> el
            [ width (px sideLength)
            , height (px sideLength)
            , centerY
            , clip
            ]



-- Mobile Thumbnail List


thumbnailListMobileElement : Model -> Element Msg
thumbnailListMobileElement model =
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
                |> List.map thumbnailElementMobile
                |> column
                    [ paddingXY 20 0
                    , width fill
                    , spacing 30
                    ]

        NotAsked ->
            localLoaderElement

        Loading ->
            localLoaderElement

        Failure error ->
            errorElement error
                |> el [ centerX, centerY ]


thumbnailElementMobile : String -> Element Msg
thumbnailElementMobile src =
    image
        [ width fill
        , height shrink
        , centerY
        , centerX
        ]
        { src = src, description = "" }



-- Full size image slideshow


fullSizeElement : Viewport -> FullSizeData -> Element Msg
fullSizeElement viewport data =
    let
        arrowWidth =
            80

        arrowImage =
            image [ width (px arrowWidth) ]
                { src = assetUrl "Arrow.png", description = "" }

        leftArrow =
            if not (SelectList.isHead data) then
                arrowImage
                    |> el
                        [ centerY
                        , centerX
                        , Events.onClick PrevImage
                        , alignLeft
                        , width (px arrowWidth)
                        , height shrink
                        , pointer
                        , rotate pi
                        , moveUp 10
                        , alpha 0.6
                        , mouseOver [ alpha 1 ]
                        ]
                    |> el [ width (px arrowWidth), height fill ]

            else
                el [ width (px arrowWidth) ] none

        rightArrow =
            if not (SelectList.isLast data) then
                arrowImage
                    |> el
                        [ centerY
                        , centerX
                        , Events.onClick NextImage
                        , alignRight
                        , height shrink
                        , width (px arrowWidth)
                        , pointer
                        , moveUp 10
                        , alpha 0.6
                        , mouseOver [ alpha 1 ]
                        ]
                    |> el [ width (px arrowWidth), height fill ]

            else
                el [ width (px arrowWidth) ] none
    in
    [ leftArrow
    , fullSizeImageElement viewport data
    , rightArrow
    ]
        |> row
            [ centerX
            , height fill
            , spacing 20
            ]


fullSizeImageElement : Viewport -> FullSizeData -> Element Msg
fullSizeImageElement viewport data =
    let
        maxWidth =
            max
                (viewport.width - 650)
                ((viewport.width // 2) - 120)

        maxHeight =
            viewport.height - 200
    in
    image
        [ width (shrink |> maximum maxWidth)
        , height (shrink |> maximum maxHeight)
        ]
        { src = SelectList.selected data
        , description = ""
        }
        |> (\img -> link [ centerX, centerY ] { label = img, url = thumbnailsUrl })
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


requestScene : Cmd Msg
requestScene =
    Task.perform
        (\viewport ->
            SceneResize
                (round viewport.scene.width)
                (round viewport.scene.height)
        )
        Dom.getViewport



-- UTILITY #####################################################################
-- CONSTANTS


pi =
    3.14159


rowLength =
    3



-- PURE UTILITY


colors =
    { lightGray = rgb 0.8 0.8 0.8
    , darkGray = rgb 0.5 0.5 0.5
    , paintPurple = rgb255 136 49 227
    , black = rgb 0 0 0
    , transparentBlack = \a -> rgba 0 0 0 a
    , white = rgb 1 1 1
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
