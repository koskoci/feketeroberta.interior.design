module Main exposing (..)

import Array
import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Transitions exposing (transition)
import Html.Attributes
import Html.Styled exposing (Html, a, div, fromUnstyled, img, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (attribute, class, css, src)
import Html.Styled.Events exposing (onClick)
import Images
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, map, oneOf, parse, s)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { route : Route
    , key : Nav.Key
    }


type Route
    = Home
    | Enteriors (Maybe Index)
    | Moodboards (Maybe Index)
    | About
    | Contact


type alias Index =
    Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home Parser.top

        -- if the index were kept in a query string ?q=42 rather than the path,
        -- the next two lines could be contracted down to this single one:
        -- map Enteriors (s "enterior" <?> Query.string "q")
        , map (Enteriors Nothing) (s "enterior")
        , map (Enteriors << Just) (s "enterior" </> Parser.int)
        , map (Moodboards Nothing) (s "latvanyterv")
        , map (Moodboards << Just) (s "latvanyterv" </> Parser.int)
        , map About (s "rolam")
        , map Contact (s "elerhetoseg")
        ]


parse : Url -> Maybe Route
parse url =
    url |> Parser.parse parser


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = parse url |> Maybe.withDefault Home
      , key = key
      }
    , Cmd.none
    )


type Msg
    = HomeClicked
    | EnteriorsClicked
    | MoodboardsClicked
    | AboutClicked
    | ContactClicked
    | EnteriorsImageClicked Index
    | MoodboardsImageClicked Index
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        HomeClicked ->
            ( model, "/" |> Nav.pushUrl model.key )

        EnteriorsClicked ->
            ( model, "/enterior" |> Nav.pushUrl model.key )

        MoodboardsClicked ->
            ( model, "/latvanyterv" |> Nav.pushUrl model.key )

        AboutClicked ->
            ( model, "/rolam" |> Nav.pushUrl model.key )

        ContactClicked ->
            ( model, "/elerhetoseg" |> Nav.pushUrl model.key )

        EnteriorsImageClicked index ->
            let
                path =
                    String.concat [ "/enterior/", String.fromInt index ]
            in
            ( model, path |> Nav.pushUrl model.key )

        MoodboardsImageClicked index ->
            let
                path =
                    String.concat [ "/latvanyterv/", String.fromInt index ]
            in
            ( model, path |> Nav.pushUrl model.key )

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
            ( { model
                | route = url |> parse |> Maybe.withDefault Home
              }
            , Cmd.none
            )


cmdNone : Model -> ( Model, Cmd msg )
cmdNone model =
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view { route } =
    { title = "Fekete Roberta lakberendező"
    , body = [ route |> body |> toUnstyled ]
    }


body : Route -> Html Msg
body route =
    div
        [ class "app"
        ]
        [ viewPage route
        ]


viewPage : Route -> Html Msg
viewPage route =
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
            (header :: content route :: [ footer ])
        ]


header : Html Msg
header =
    let
        logo =
            img
                [ src "assets/FR-no-margin.png"
                , onClick HomeClicked
                , css
                    [ height (px logoHeight)
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
            , fontSize (px 20)
            , padding3 (px standardPadding) zero (px standardPadding)
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
                [ padding3 (px (2 * standardPadding)) zero (px standardPadding) ]
            ]
            [ text "© 2021 Fekete Roberta" ]
        ]


content : Route -> Html Msg
content route =
    div
        [ css
            [ minHeight (px 500)
            , fontSize (px 20)
            ]
        ]
        [ content_ route ]


content_ : Route -> Html Msg
content_ route =
    case route of
        Home ->
            home

        Enteriors Nothing ->
            enteriors

        Enteriors (Just index) ->
            let
                fileName =
                    Images.enteriorok
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.withDefault ""
            in
            fileName |> enlargedImage

        Moodboards Nothing ->
            moodboards

        Moodboards (Just index) ->
            let
                fileName =
                    Images.latvanytervek
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.withDefault ""
            in
            fileName |> enlargedImage

        About ->
            about

        Contact ->
            contact


enlargedImage : String -> Html msg
enlargedImage url =
    img
        [ css
            [ maxWidth (pct 100)
            , maxHeight (calc (vh 100) minus (px (logoHeight + 3 * standardPadding)))
            , display block
            , margin auto
            ]
        , src url
        ]
        []


home : Html Msg
home =
    img
        [ Html.Styled.Attributes.width standardWidth
        , src "assets/enteriorok/03 PIX7983.jpg"
        ]
        []


enteriors : Html Msg
enteriors =
    let
        wrap index image =
            img
                [ src image
                , onClick (EnteriorsImageClicked index)
                , css
                    [ boxSizing borderBox
                    , padding4 zero (px standardPadding) (px standardPadding) zero
                    , Css.width (px width)
                    , cursor pointer
                    ]
                ]
                []

        width =
            (standardWidth - 2 * standardPadding) / 3 + 15
    in
    Images.enteriorok
        |> Array.fromList
        |> Array.indexedMap wrap
        |> Array.toList
        |> div
            [ css
                [ displayFlex
                , flexDirection column
                , flexWrap Css.wrap
                , height (px 7600)
                ]
            ]


moodboards : Html Msg
moodboards =
    let
        wrap index image =
            img
                [ src image
                , onClick (MoodboardsImageClicked index)
                , css
                    [ boxSizing borderBox
                    , padding4 zero (px standardPadding) (px standardPadding) zero
                    , Css.width (px width)
                    , cursor pointer
                    ]
                ]
                []

        width =
            (standardWidth - standardPadding) / 2 + 10
    in
    Images.latvanytervek
        |> Array.fromList
        |> Array.indexedMap wrap
        |> Array.toList
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


standardPadding =
    20


logoHeight =
    60
