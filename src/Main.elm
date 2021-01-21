module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Transitions exposing (transition)
import Html.Styled exposing (Html, a, div, footer, img, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import Images


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


type Model
    = Home
    | Enteriors
    | Moodboards
    | About
    | Contact


init : Model
init =
    Home


type Msg
    = HomeClicked
    | EnteriorsClicked
    | MoodboardsClicked
    | AboutClicked
    | ContactClicked


update : Msg -> Model -> Model
update msg _ =
    case msg of
        HomeClicked ->
            Home

        EnteriorsClicked ->
            Enteriors

        MoodboardsClicked ->
            Moodboards

        AboutClicked ->
            About

        ContactClicked ->
            Contact


view : Model -> Html Msg
view model =
    div
        [ css
            [ width (px standardWidth)
            , margin auto
            , color theme.primary
            ]
        ]
        [ div
            [ css [ width (pct 100) ] ]
            (viewHeader model :: content model :: [ viewFooter ])
        ]


viewHeader : Model -> Html Msg
viewHeader model =
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
        , div
            [ onClick HomeClicked
            , jumpOnHover
            , css
                [ flexGrow (num 1)
                ]
            ]
            [ text "| Fekete Roberta belsőépítész" ]
        , div
            [ onClick EnteriorsClicked
            , jumpOnHover
            , css
                [ flexGrow (num 1) ]
            ]
            [ text " | Enteriőr" ]
        , div
            [ onClick MoodboardsClicked
            , jumpOnHover
            , css
                [ flexGrow (num 1) ]
            ]
            [ text " | Látványterv" ]
        , div
            [ onClick AboutClicked
            , jumpOnHover
            , css
                [ flexGrow (num 1) ]
            ]
            [ text " | Rólam" ]
        , div
            [ onClick ContactClicked
            , jumpOnHover
            , css
                [ flexGrow (num 1) ]
            ]
            [ text " | Elérhetőség" ]
        ]


viewFooter : Html msg
viewFooter =
    div
        [ css
            [ displayFlex
            , justifyContent center
            ]
        ]
        [ footer
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
            , fontStyle italic
            ]
        ]
        [ content_ model ]


content_ : Model -> Html Msg
content_ model =
    case model of
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
    wrap "assets/portré2.jpg"


enteriors : Html Msg
enteriors =
    Images.enteriorok
        |> List.map wrap
        |> div []


moodboards : Html Msg
moodboards =
    Images.latvanytervek
        |> List.map wrap
        |> div []


about : Html Msg
about =
    div
        [ css
            [ fontStyle italic
            , displayFlex
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
                ]
            ]
            [ text "Belső terek kialakítása magán és céges ügyfeleknek egyedi igény szerint, megtalálva a megfelelő harmóniát, stílust, funkcionalitást. Segítek összhangot teremteni.\n" ]
        ]


contact : Html Msg
contact =
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
            [ div
                [ css
                    [ display inlineFlex
                    , alignItems center
                    , justifyContent spaceBetween
                    , padding (px 10)
                    , width (pct 100)
                    ]
                ]
                [ img [ src "assets/phone.svg" ] [], text "+36 70 338 1317" ]
            , div
                [ css
                    [ display inlineFlex
                    , alignItems center
                    , justifyContent spaceBetween
                    , padding (px 10)
                    , width (pct 100)
                    ]
                ]
                [ img [ src "assets/email.svg" ] [], text "fekete.roberta@gmail.com" ]
            ]
        ]


theme : { secondary : Color, primary : Color }
theme =
    { primary = rgb 0 0 0
    , secondary = rgb 250 240 230
    }


wrap : String -> Html msg
wrap image =
    img
        [ Html.Styled.Attributes.width standardWidth
        , src image
        ]
        []


standardWidth =
    960


standardHeight =
    640
