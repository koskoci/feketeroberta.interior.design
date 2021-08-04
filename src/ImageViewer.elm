module ImageViewer exposing (call)

import Array exposing (Array)
import Css exposing (..)
import Html.Styled exposing (Html, div, img)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import Images


type alias Index =
    Int


call : Array String -> Css.LengthOrAuto compatible -> (Index -> msg) -> Index -> Html msg
call images height viewMsg index =
    let
        prevIndex =
            (index - 1) |> modBy (Array.length images)

        nextIndex =
            (index + 1) |> modBy (Array.length images)

        url =
            images |> Array.get index |> Maybe.withDefault ""
    in
    view height viewMsg prevIndex nextIndex url


view : Css.LengthOrAuto compatible -> (Index -> msg) -> Index -> Index -> String -> Html msg
view height viewMsg prevIndex nextIndex url =
    let
        leftArrow =
            img
                [ onClick <| viewMsg prevIndex
                , src "/assets/navigate_before.svg"
                ]
                []

        rightArrow =
            img
                [ onClick <| viewMsg nextIndex
                , src "/assets/navigate_next.svg"
                ]
                []
    in
    div
        [ css
            [ displayFlex
            , flexDirection row
            , position relative
            , left (px -24)
            , width (calc (pct 100) plus (px 24))
            ]
        ]
        [ leftArrow
        , enlargedImage height url
        , rightArrow
        ]


enlargedImage : Css.LengthOrAuto compatible -> String -> Html msg
enlargedImage height url =
    div
        [ css
            [ Css.height height
            , width (pct 100)
            , displayFlex
            ]
        ]
        [ img
            [ css
                [ maxWidth (pct 100)
                , maxHeight (pct 100)
                , margin auto
                ]
            , src url
            ]
            []
        ]
