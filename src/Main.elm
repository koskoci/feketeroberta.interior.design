module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Transitions exposing (transition)
import Html.Attributes
import Html.Styled as Html exposing (Html, a, div, fromUnstyled, img, li, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (attribute, class, css, id, src)
import Html.Styled.Events exposing (onClick)
import ImageViewer
import Images
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, int, map, oneOf, s)


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


parse : Url -> Route
parse url =
    url |> Parser.parse parser |> Maybe.withDefault Home


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { route = parse url
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
    | ImageRequested Album Index


type Album
    = Enterior
    | Moodboard


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
            ( { model | route = parse url }
            , Cmd.none
            )

        ImageRequested album index ->
            ( model, index |> toPath album |> Nav.pushUrl model.key )


toPath : Album -> Index -> String
toPath album index =
    let
        scope =
            case album of
                Moodboard ->
                    "/latvanyterv/"

                Enterior ->
                    "/enterior/"
    in
    String.concat [ scope, String.fromInt index ]


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
    let
        footer_ =
            if route == Home then
                []

            else
                [ footer ]
    in
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
            (header :: content route :: footer_)
        ]


header : Html Msg
header =
    let
        logo =
            img
                [ src "/assets/FR-no-margin.png"
                , onClick HomeClicked
                , css
                    [ Css.height (px logoHeight)
                    , marginRight (px 30)
                    , paddingBottom (px standardPadding)
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
                    [ class "headerText"
                    , css [ paddingBottom (px standardPadding) ]
                    ]
                    [ text label ]
                ]

        passiveHeaderItem msg label =
            div
                [ onClick msg
                , css
                    [ flexGrow (num 1)
                    , displayFlex
                    , alignItems flexEnd
                    , paddingBottom (px standardPadding)
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
            , marginTop (px standardPadding)
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
        [ Html.footer
            [ css
                [ padding3 (px standardPadding) zero (px standardPadding) ]
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
            ImageViewer.call (Images.enteriorok |> Array.fromList) height (ImageRequested Enterior) index

        Moodboards Nothing ->
            moodboards

        Moodboards (Just index) ->
            ImageViewer.call (Images.latvanytervek |> Array.fromList) height (ImageRequested Moodboard) index

        About ->
            about

        Contact ->
            contact


home : Html Msg
home =
    let
        wrap src_ =
            img
                [ Html.Styled.Attributes.width standardWidth
                , src src_
                ]
                []
    in
    div [ id "cf" ]
        [ wrap "/assets/enteriorok/01 PIX7986.jpg"
        , wrap "/assets/enteriorok/02 PIX7973.jpg"
        , wrap "/assets/enteriorok/03 PIX7983.jpg"
        , wrap "/assets/enteriorok/04 PIX7964.jpg"
        , wrap "/assets/enteriorok/05 PIX7988.jpg"
        , wrap "/assets/enteriorok/06 PIX7912.jpg"
        ]


imagesForColumn : Int -> List (Html Msg)
imagesForColumn column =
    enteriorImages
        |> Array.filter (\item -> (item |> Tuple.first) == column)
        |> Array.map Tuple.second
        |> Array.toList


enteriors : Html Msg
enteriors =
    div [ class "row" ]
        [ div [ class "column" ] (imagesForColumn 1)
        , div [ class "column" ] (imagesForColumn 2)
        , div [ class "column" ] (imagesForColumn 3)
        ]


enteriorImages : Array ( Int, Html Msg )
enteriorImages =
    let
        wrap index image =
            let
                column =
                    (index |> remainderBy 3) + 1
            in
            ( column
            , img
                [ src image
                , onClick (EnteriorsImageClicked index)
                , css
                    [ boxSizing borderBox
                    , padding4 zero (px standardPadding) (px standardPadding) zero
                    , Css.width (pct 100)
                    , cursor pointer
                    ]
                ]
                []
            )
    in
    Images.enteriorok
        |> Array.fromList
        |> Array.indexedMap wrap


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
            [ src "/assets/portre2.jpg"
            , css [ Css.height (px standardHeight) ]
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
            , Css.height (px standardHeight)
            ]
        ]
        [ img
            [ Html.Styled.Attributes.width (standardWidth // 2)
            , src "/assets/portre1.jpg"
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
            [ contactItem "/assets/phone.svg" "+36 70 882 0477"
            , contactItem "/assets/email.svg" "fekete.roberta@gmail.com"
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


homeImage =
    "/assets/enteriorok/03 PIX7983.jpg"


height =
    calc (vh 100) minus (px (logoHeight + 4 * standardPadding + 16))
