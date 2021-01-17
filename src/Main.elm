module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Transitions exposing (transition)
import Html.Styled exposing (Html, a, div, footer, img, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view >> toUnstyled }


type Model
    = Enteriors
    | Moodboards
    | About
    | Contact


init : Model
init =
    Enteriors


type Msg
    = EnteriorsClicked
    | MoodboardsClicked
    | AboutClicked
    | ContactClicked


update : Msg -> Model -> Model
update msg _ =
    case msg of
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
            [ width (px 1000)
            , margin auto
            ]
        ]
        [ div
            [ css [ width (pct 100) ] ]
            (viewHeader model :: content model :: [ viewFooter ])
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        highlightOnHover =
            css
                [ hover
                    [ backgroundColor theme.primary
                    ]
                , transition
                    [ Css.Transitions.backgroundColor 500
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
            , padding2 (px 60) zero
            ]
        ]
        [ logo
        , div
            [ css
                [ flexGrow (num 1)
                ]
            ]
            [ text "| Fekete Roberta" ]
        , div
            [ onClick EnteriorsClicked
            , highlightOnHover
            , css
                [ flexGrow (num 1) ]
            ]
            [ text " | Enteriőr" ]
        , div
            [ onClick MoodboardsClicked
            , css
                [ flexGrow (num 1) ]
            ]
            [ text " | Moodboardok" ]
        , div
            [ onClick AboutClicked
            , css
                [ flexGrow (num 1) ]
            ]
            [ text " | Rólam" ]
        , div
            [ onClick ContactClicked
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
            []
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
        Enteriors ->
            enteriors

        Moodboards ->
            moodboards

        About ->
            about

        Contact ->
            contact


enteriors : Html Msg
enteriors =
    let
        wrap image =
            img
                [ Html.Styled.Attributes.width 960
                , src image
                ]
                []
    in
    div
        []
        [ wrap "assets/enteriorok/IMG_1407_edited.jpg"
        , wrap "assets/enteriorok/IMG_1441_edited.jpg"
        , wrap "assets/enteriorok/IMG_1450_edited.jpg"
        , wrap "assets/enteriorok/IMG_1476_edited.jpg"
        , wrap "assets/enteriorok/IMG_1489_edited.jpg"
        , wrap "assets/enteriorok/IMG_1495_edited.jpg"
        , wrap "assets/enteriorok/IMG_1501_edited_true-color.jpg"
        , wrap "assets/enteriorok/IMG_1513_edited.jpg"
        ]


moodboards : Html Msg
moodboards =
    div [] [ text "3D Moodboardok...\n" ]


about : Html Msg
about =
    div [] [ text "Belső terek kialakítása magán és céges ügyfeleknek egyedi igény szerint, megtalálva a megfelelő harmóniát, stílust, funkcionalitást. Segítek összhangot teremteni...\n" ]


contact : Html Msg
contact =
    div []
        [ ul
            []
            [ li [] [ text "Telefon: +36 70 338 1317" ]
            , li [] [ text "Email: fekete.roberta@gmail.com" ]
            ]
        ]


theme : { secondary : Color, primary : Color }
theme =
    { primary = hex "d1d1d1"
    , secondary = rgb 250 240 230
    }
