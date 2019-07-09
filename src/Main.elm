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


route : Parser (Page -> a) a
route =
    oneOf
        [ Url.map makeFullSize (s "portfolio" </> string </> s "full" </> string)
        , Url.map makeAbout (s "about" </> top)
        , Url.map makeResume (s "resume" </> top)
        , Url.map makeThumbnails (s "portfolio" </> string)
        , Url.map (Home Nothing) top
        ]


toPage : Url -> Page
toPage url =
    Maybe.withDefault NotFound (Url.parse route url)


makeHeaderState selected =
    { selected = selected, hovered = Nothing, displayContact = False }


makeAbout : Page
makeAbout =
    About (makeHeaderState headerTitle.about)


makeResume : Page
makeResume =
    Resume (makeHeaderState headerTitle.resume)


makeThumbnails : String -> Page
makeThumbnails category =
    Thumbnails (makeHeaderState headerTitle.portfolio) (makeThumbnailData category)


makeThumbnailData : String -> ThumbnailData
makeThumbnailData category =
    { category = category
    , listing = NotAsked
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
fullSizeUrl category imageSrc =
    Builder.absolute [ "portfolio", category, "full" ] [ Builder.string "image" imageSrc ]


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
    { category : String
    , listing : WebData CategoryListing
    }


type alias CategoryListing =
    Dict String (List ThumbnailInfo)


type alias ThumbnailInfo =
    { thumbnail : String
    , resourceQuery : String
    }


init : { viewport : { width : Int, height : Int } } -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialPage =
            toPage url

        initialModel =
            { key = key
            , viewport = toViewport flags.viewport
            , page = initialPage
            }
    in
    ( initialModel, Cmd.batch [ requestScene, pageRequests initialPage ] )


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
    | GotThumbnailResponse (Result Http.Error ThumbnailDataResponse)
    | GotFullResponse (Result Http.Error FullDataResponse)


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
                    toPage url

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

        GotThumbnailResponse result ->
            case model.page of
                Thumbnails _ data ->
                    let
                        newPage =
                            thumbnailPageFromResponse data.category result
                    in
                    ( { model | page = newPage }, requestScene )

                _ ->
                    ( model, requestScene )

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

        Thumbnails headerState thumbnailData ->
            usualBodyDesktop model.viewport headerState (thumbnailListDesktopElement model)

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

        Thumbnails headerState thumbnailData ->
            usualBodyMobile model.viewport headerState (thumbnailListMobileElement model)

        FullSize headerState data ->
            usualBodyMobile model.viewport headerState (thumbnailListMobileElement model)

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
    case model.page of
        Thumbnails _ thumbnailData ->
            case thumbnailData.listing of
                Success listing ->
                    listing
                        |> Dict.get thumbnailData.category
                        |> Maybe.map (thumbnailColumn model.viewport thumbnailData.category << chunksOf rowLength)
                        |> Maybe.withDefault notFoundElement

                NotAsked ->
                    localLoaderElement

                Loading ->
                    localLoaderElement

                Failure error ->
                    directoryErrorElement error
                        |> el [ centerX, centerY ]

        _ ->
            unknownPageErrorElement


thumbnailColumn : Viewport -> String -> List (List ThumbnailInfo) -> Element Msg
thumbnailColumn viewport currentCategory rows =
    let
        insertHeader list =
            portfolioCategoryListElement currentCategory :: list
    in
    rows
        |> List.map (thumbnailRow currentCategory viewport)
        |> insertHeader
        |> column [ centerX, spacing 20 ]


portfolioCategoryListElement : String -> Element Msg
portfolioCategoryListElement currentCategory =
    [ { category = "design", label = "Graphic Design" }
    , { category = "photography", label = "Photography" }
    ]
        |> List.map (portfolioCategoryElement currentCategory)
        |> row
            [ spacing 20
            , centerX
            ]


portfolioCategoryElement : String -> { category : String, label : String } -> Element Msg
portfolioCategoryElement currentCategory info =
    let
        isSelected =
            Debug.log "Category" info.category == Debug.log "Current" currentCategory

        fontAttributes =
            if isSelected then
                futuraBold ++ [ Font.color colors.black ]

            else
                futuraMedium ++ [ Font.color colors.darkGray ]
    in
    link
        ([ centerX
         , centerY
         , Font.size (scaled 4)
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
        |> row [ spacing 20 ]


thumbnailElement : String -> Viewport -> ThumbnailInfo -> Element Msg
thumbnailElement category viewport thumbnailInfo =
    let
        sideLength =
            ((viewport.width // 2) - 100) // rowLength
    in
    image
        [ width (shrink |> maximum sideLength)
        , centerY
        , centerX
        ]
        { src = thumbnailInfo.thumbnail, description = "" }
        |> (\img -> link [ centerY ] { label = img, url = fullSizeUrl category thumbnailInfo.resourceQuery })
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

        insertHeader category list =
            portfolioCategoryListMobileElement category :: list
    in
    case model.page of
        Thumbnails _ thumbnailData ->
            case thumbnailData.listing of
                Success listing ->
                    listing
                        |> Dict.get thumbnailData.category
                        |> Maybe.map (List.map thumbnailElementMobile)
                        |> Maybe.withDefault [ notFoundElement ]
                        |> insertHeader thumbnailData.category
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
                    directoryErrorElement error

        _ ->
            unknownPageErrorElement
                |> el [ centerX, centerY ]


thumbnailElementMobile : ThumbnailInfo -> Element Msg
thumbnailElementMobile thumbnailInfo =
    image
        [ width fill
        , height shrink
        , centerY
        , centerX
        ]
        { src = thumbnailInfo.thumbnail, description = "" }


portfolioCategoryListMobileElement : String -> Element Msg
portfolioCategoryListMobileElement currentCategory =
    [ { category = "design", label = "Graphic Design" }
    , { category = "photography", label = "Photography" }
    ]
        |> List.map (portfolioCategoryMobileElement currentCategory)
        |> row
            [ spacing 35
            , centerX
            ]


portfolioCategoryMobileElement : String -> { category : String, label : String } -> Element Msg
portfolioCategoryMobileElement currentCategory info =
    let
        isSelected =
            Debug.log "Category" info.category == Debug.log "Current" currentCategory

        fontAttributes =
            if isSelected then
                futuraBold ++ [ Font.color colors.black ]

            else
                futuraMedium ++ [ Font.color colors.darkGray ]
    in
    link
        ([ centerX
         , centerY
         , Font.size (scaled 5)
         ]
            ++ fontAttributes
        )
        { url = thumbnailsUrl info.category
        , label = text info.label
        }



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
            el [ width (px arrowWidth) ] none

        rightArrow =
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
        { src = ""
        , description = ""
        }
        |> (\img -> link [ centerX, centerY ] { label = img, url = thumbnailsUrl data.category })
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


pageRequests : Page -> Cmd Msg
pageRequests page =
    case page of
        Thumbnails _ data ->
            requestThumbnailData { category = data.category }

        FullSize _ data ->
            requestFullData { category = data.category, resource = data.resource }

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


requestThumbnailData : { category : String } -> Cmd Msg
requestThumbnailData request =
    Http.get
        { url = thumbnailQueryUrl request.category
        , expect = Http.expectJson GotThumbnailResponse decodeThumbnailData
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
    Decode.dict (Decode.list decodeThumbnailInfo)


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


thumbnailPageFromResponse : String -> Result Http.Error ThumbnailDataResponse -> Page
thumbnailPageFromResponse category response =
    let
        data =
            { category = category
            , listing = RemoteData.fromResult (Result.map .listing response)
            }
    in
    Thumbnails (makeHeaderState headerTitle.portfolio) data


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


rowLength =
    3


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


unknownPageErrorElement : Element msg
unknownPageErrorElement =
    textColumn [ spacing 10, width (px 600) ]
        [ paragraph []
            [ text
                """
                Congratulations on finding the super-secret page of secrets!
                Send Andrew Dilmore a message at the contact info listed above.
                """
            ]
        ]
