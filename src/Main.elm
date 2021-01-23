module Main exposing (..)

import Bootstrap.Carousel as Carousel
import Bootstrap.Carousel.Slide as Slide
import Browser
import Css exposing (..)
import Css.Transitions exposing (transition)
import Html.Attributes
import Html.Styled exposing (Html, a, div, fromUnstyled, img, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (attribute, class, css, src)
import Html.Styled.Events exposing (onClick)
import Images


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel |> cmdNone
        , subscriptions = \model -> Carousel.subscriptions model.carouselState CarouselMsg
        , update = update
        , view = view >> toUnstyled
        }


type alias Model =
    { tab : Tab
    , carouselState : Carousel.State
    , carouselVisible : Maybe Tab
    }


type Tab
    = Home
    | Enteriors
    | Moodboards
    | About
    | Contact


initialModel : Model
initialModel =
    { tab = Home
    , carouselState = Carousel.initialState
    , carouselVisible = Nothing
    }


type Msg
    = HomeClicked
    | EnteriorsClicked
    | MoodboardsClicked
    | AboutClicked
    | ContactClicked
    | EnteriorsImageClicked
    | MoodboardsImageClicked
    | CarouselMsg Carousel.Msg


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


cmdNone : Model -> ( Model, Cmd msg )
cmdNone model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "app" ]
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
        jumpOnHover =
            css
                [ position relative
                , top (px 0)
                , hover
                    [ top (px -10)
                    ]
                , transition
                    [ Css.Transitions.top3 200 20 Css.Transitions.ease
                    ]
                ]

        logo =
            img
                [ src "assets/FR-no-margin.png"
                , css
                    [ height (px 80)
                    , marginRight (px 30)
                    ]
                ]
                []

        headerItem msg label =
            div
                [ onClick msg
                , jumpOnHover
                , css
                    [ flexGrow (num 1)
                    ]
                ]
                [ text label ]
    in
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , alignItems flexEnd
            , fontSize (px 24)
            , padding3 (px 40) zero (px 20)
            ]
        ]
        [ logo
        , headerItem HomeClicked "| Fekete Roberta lakberendező"
        , headerItem EnteriorsClicked " | Enteriőr"
        , headerItem MoodboardsClicked " | Látványterv"
        , headerItem AboutClicked " | Rólam"
        , headerItem ContactClicked " | Elérhetőség"
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
            [ text "© 2020 Fekete Roberta" ]
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
        , src "assets/portré2.jpg"
        ]
        []


enteriors : Html Msg
enteriors =
    let
        wrap image =
            img
                [ Html.Styled.Attributes.width standardWidth
                , src image
                , onClick EnteriorsImageClicked
                ]
                []
    in
    Images.enteriorok
        |> List.map wrap
        |> div []


moodboards : Html Msg
moodboards =
    let
        wrap image =
            img
                [ Html.Styled.Attributes.width standardWidth
                , src image
                , onClick MoodboardsImageClicked
                ]
                []
    in
    Images.latvanytervek
        |> List.map wrap
        |> div []


about : Html Msg
about =
    div
        [ css
            [ displayFlex
            , alignItems center
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
                [ padding4 zero zero zero (px 30)
                , lineHeight (num 2.5)
                , fontStyle italic
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
                    , padding (px 20)
                    , width (pct 100)
                    ]
                ]
                [ img [ src icon, css [ paddingRight (px 40) ] ] [], text label ]
    in
    div
        [ css
            [ displayFlex
            , alignItems center
            , justifyContent center
            , height (px standardHeight)
            ]
        ]
        [ div
            [ css
                [ displayFlex
                , flexDirection column
                , alignItems flexStart
                , justifyContent center
                , height (px (standardHeight / 4))
                ]
            ]
            [ contactItem "assets/phone.svg" "+36 70 338 1317"
            , contactItem "assets/email.svg" "fekete.roberta@gmail.com"
            ]
        ]


theme : { secondary : Color, primary : Color }
theme =
    { primary = rgb 0 0 0
    , secondary = rgb 250 240 230
    }


standardWidth =
    960


standardHeight =
    640
