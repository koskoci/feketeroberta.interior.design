module Main exposing (..)

import Bootstrap.Carousel as Carousel
import Bootstrap.Carousel.Slide as Slide
import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Transitions exposing (transition)
import Html.Attributes
import Html.Styled exposing (Html, a, div, fromUnstyled, img, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (attribute, class, css, src)
import Html.Styled.Events exposing (onClick)
import Images
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Carousel.subscriptions model.carouselState CarouselMsg
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { tab : Tab
    , carouselState : Carousel.State
    , carouselVisible : Maybe Tab
    , key : Nav.Key
    , url : Url.Url
    }


type Tab
    = Home
    | Enteriors
    | Moodboards
    | About
    | Contact


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    { tab = Home
    , carouselState = Carousel.initialState
    , carouselVisible = Nothing
    , key = key
    , url = url
    }
        |> cmdNone


type Msg
    = HomeClicked
    | EnteriorsClicked
    | MoodboardsClicked
    | AboutClicked
    | ContactClicked
    | EnteriorsImageClicked
    | MoodboardsImageClicked
    | CarouselMsg Carousel.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        HomeClicked ->
            { model | tab = Home } |> cmdNone

        EnteriorsClicked ->
            { model | tab = Enteriors } |> cmdNone

        MoodboardsClicked ->
            { model | tab = Moodboards } |> cmdNone

        AboutClicked ->
            { model | tab = About } |> cmdNone

        ContactClicked ->
            { model | tab = Contact } |> cmdNone

        EnteriorsImageClicked ->
            { model | carouselVisible = Just Enteriors } |> cmdNone

        MoodboardsImageClicked ->
            { model | carouselVisible = Just Moodboards } |> cmdNone

        CarouselMsg subMsg ->
            { model | carouselState = Carousel.update subMsg model.carouselState } |> cmdNone

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , url |> Url.toString |> Nav.pushUrl model.key
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


cmdNone : Model -> ( Model, Cmd msg )
cmdNone model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Fekete Roberta lakberendező"
    , body = [ model |> body |> toUnstyled ]
    }


body : Model -> Html Msg
body model =
    div
        [ class "app"
        ]
        [ case model.carouselVisible of
            Just tab ->
                viewCarousel tab model

            Nothing ->
                viewPage model
        ]


viewPage : Model -> Html Msg
viewPage model =
    div
        [ css
            [ width (px standardWidth)
            , margin auto
            , color theme.primary
            , letterSpacing (px 1.5)
            ]
        ]
        [ div
            [ css [ width (pct 100) ] ]
            (header model :: content model :: [ footer ])
        ]


viewCarousel : Tab -> Model -> Html Msg
viewCarousel tab model =
    let
        images =
            case tab of
                Enteriors ->
                    Images.enteriorok

                Moodboards ->
                    Images.latvanytervek

                _ ->
                    []

        slides =
            images
                |> List.map (Slide.image [ Html.Attributes.class "slide" ])
                |> List.map (Slide.config [])
    in
    Carousel.config CarouselMsg [ Html.Attributes.class "carousel" ]
        |> Carousel.withControls
        |> Carousel.withIndicators
        |> Carousel.slides slides
        |> Carousel.view model.carouselState
        >> fromUnstyled


header : Model -> Html Msg
header model =
    let
        logo =
            img
                [ src "assets/FR-no-margin.png"
                , onClick HomeClicked
                , css
                    [ height (px 80)
                    , marginRight (px 30)
                    ]
                ]
                []

        headerItem msg label =
            div
                [ onClick msg
                , class "headerContainer"
                , css
                    [ flexGrow (num 1)
                    , displayFlex
                    , alignItems flexEnd
                    ]
                ]
                [ div
                    [ class "headerText" ]
                    [ text label ]
                ]

        passiveHeaderItem msg label =
            div
                [ onClick msg
                , css
                    [ flexGrow (num 1)
                    , displayFlex
                    , alignItems flexEnd
                    ]
                ]
                [ div
                    [ class "headerText" ]
                    [ text label ]
                ]
    in
    div
        [ css
            [ displayFlex
            , flexDirection row
            , justifyContent spaceBetween
            , alignItems stretch
            , fontSize (px 20)
            , padding3 (px 20) zero (px 20)
            , cursor pointer
            , position sticky
            , top (px 0)
            , backgroundColor theme.secondary
            ]
        ]
        [ logo
        , passiveHeaderItem HomeClicked "| FEKETE ROBERTA lakberendező"
        , headerItem EnteriorsClicked " | ENTERIŐR"
        , headerItem MoodboardsClicked " | LÁTVÁNYTERV"
        , headerItem AboutClicked " | RÓLAM"
        , headerItem ContactClicked " | ELÉRHETŐSÉG"
        ]


footer : Html msg
footer =
    div
        [ css
            [ displayFlex
            , justifyContent center
            ]
        ]
        [ Html.Styled.footer
            [ css
                [ padding3 (px 40) zero (px 20) ]
            ]
            [ text "© 2021 Fekete Roberta" ]
        ]


content : Model -> Html Msg
content model =
    div
        [ css
            [ minHeight (px 500)
            , fontSize (px 20)
            ]
        ]
        [ content_ model ]


content_ : Model -> Html Msg
content_ { tab } =
    case tab of
        Home ->
            home

        Enteriors ->
            enteriors

        Moodboards ->
            moodboards

        About ->
            about

        Contact ->
            contact


home : Html Msg
home =
    img
        [ Html.Styled.Attributes.width standardWidth
        , src "assets/enteriorok/3 PIX7983.jpg"
        ]
        []


enteriors : Html Msg
enteriors =
    let
        wrap image =
            img
                [ src image
                , onClick EnteriorsImageClicked
                , css
                    [ padding4 zero (px padding) (px padding) zero
                    , Css.width (px width)
                    , cursor pointer
                    ]
                ]
                []

        width =
            (standardWidth - 2 * padding) / 3 + 15

        padding =
            20
    in
    Images.enteriorok
        |> List.map wrap
        |> div [ css [ displayFlex, flexDirection column, flexWrap Css.wrap, height (px 7600) ] ]


moodboards : Html Msg
moodboards =
    let
        wrap image =
            img
                [ src image
                , onClick MoodboardsImageClicked
                , css
                    [ padding4 zero (px padding) (px padding) zero
                    , Css.width (px width)
                    , cursor pointer
                    ]
                ]
                []

        width =
            (standardWidth - padding) / 2 + 10

        padding =
            20
    in
    Images.latvanytervek
        |> List.map wrap
        |> div []


about : Html Msg
about =
    div
        [ css
            [ displayFlex
            ]
        ]
        [ img
            [ src "assets/portré2.jpg"
            , css [ height (px standardHeight) ]
            ]
            []
        , div
            [ css
                [ padding4 (px 200) (px 25) zero (px 90)
                , lineHeight (num 1.5)
                , fontStyle italic
                , textAlign justify
                ]
            ]
            [ text "Belső terek kialakítása magán és céges ügyfeleknek, egyedi igény szerint, megtalálva a megfelelő harmóniát, stílust, funkcionalitást. Segítek összhangot teremteni.\n" ]
        ]


contact : Html Msg
contact =
    let
        contactItem icon label =
            div
                [ css
                    [ display inlineFlex
                    , alignItems center
                    , justifyContent flexStart
                    , padding4 zero (px 50) (px 20) (px 50)
                    , width (pct 100)
                    ]
                ]
                [ img [ src icon, css [ paddingRight (px 25) ] ] [], text label ]
    in
    div
        [ css
            [ displayFlex
            , height (px standardHeight)
            ]
        ]
        [ img
            [ Html.Styled.Attributes.width (standardWidth // 2)
            , src "assets/portré1.jpg"
            ]
            []
        , div
            [ css
                [ displayFlex
                , flexDirection column
                , alignItems flexStart
                , padding4 (px 200) (px 50) zero (px 50)
                ]
            ]
            [ contactItem "assets/phone.svg" "+36 70 882 0477"
            , contactItem "assets/email.svg" "fekete.roberta@gmail.com"
            ]
        ]


theme : { secondary : Color, primary : Color }
theme =
    { primary = rgb 0 0 0
    , secondary = rgb 255 255 255
    }


standardWidth =
    980


standardHeight =
    640
