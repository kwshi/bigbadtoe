module Main exposing (..)

import Browser
import Css
import Game
import Game.Outcome
import Game.Player
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev
import Style


type alias Model =
    { game : Game.State
    , resetConfirm : Maybe Bool
    }


type Msg
    = Click ( Int, Int )
    | Reset
    | Nop


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.init 3 3
      , resetConfirm = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click path ->
            ( { model
                | game = Game.click path model.game
                , resetConfirm = Just False
              }
            , Cmd.none
            )

        Reset ->
            if model.resetConfirm == Just True then
                init ()

            else
                ( { model | resetConfirm = Just True }, Cmd.none )

        Nop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "bigbadtoe"
    , body = List.map Html.toUnstyled [ viewBody model ]
    }


viewBody : Model -> Html.Html Msg
viewBody model =
    let
        game =
            Game.render model.game
    in
    Html.main_
        [ Attr.css
            [ Css.backgroundColor <| Css.rgb 0x1D 0x20 0x21
            , Style.full
            ]
        ]
        [ Html.div
            [ Attr.css <|
                [ Css.backgroundColor
                    (Game.Outcome.getWinner game.outcome
                        |> Maybe.map (Game.Player.color <| 1 / 2)
                        |> Maybe.withDefault (Css.rgba 0 0 0 0)
                    )
                , Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , Style.full
                ]
            ]
            [ Html.div
                [ Attr.css
                    [ Css.paddingBottom <| Css.rem 8
                    , Css.fontFamily Css.monospace
                    , Css.property "display" "grid"
                    , Css.property "grid-template-areas" <|
                        String.join " " <|
                            List.map (\row -> "\"" ++ String.join " " row ++ "\"")
                                [ [ "title", "by", "links" ]
                                , [ "board", "board", "board" ]
                                , [ "control", "control", "message" ]
                                ]
                    , Css.property "column-gap" ".5rem"
                    , Css.property "row-gap" ".5rem"
                    , Css.property "grid-template-columns" "auto auto auto"
                    ]
                ]
                [ Html.h1
                    [ Attr.css
                        [ Css.margin2 (Css.rem 0) (Css.rem <| 1 / 4)
                        , Css.fontSize <| Css.rem 2
                        , Css.alignSelf Css.baseline
                        ]
                    ]
                  <|
                    List.map
                        (\( color, text ) ->
                            Html.span
                                [ Attr.css [ Css.color <| Css.hex color ] ]
                                [ Html.text text ]
                        )
                        [ ( "fb4934", "big" ), ( "83a598", "bad" ), ( "ebdbb2", "toe" ) ]
                , Html.h2
                    [ Attr.css
                        [ Css.property "grid-area" "by"
                        , Css.margin <| Css.rem 0
                        , Css.fontSize <| Css.rem 1
                        , Css.color <| Css.hex "ebdbb2"
                        , Css.fontWeight Css.normal
                        , Css.alignSelf Css.baseline
                        ]
                    ]
                    [ Html.text "by @kwshi" ]
                , Html.map
                    (if Game.Outcome.isComplete game.outcome then
                        always Nop

                     else
                        Click
                    )
                    game.view
                , Html.div
                    [ Attr.css
                        [ Css.property "grid-area" "control"
                        , Css.minHeight <| Css.rem 2
                        , Css.margin <| Css.rem <| 1 / 4
                        ]
                    ]
                    [ model.resetConfirm
                        |> Maybe.map
                            (\confirm ->
                                Html.button
                                    [ Ev.onClick Reset
                                    , Attr.css
                                        [ Css.backgroundColor <|
                                            if confirm then
                                                Css.rgb 0xD7 0x99 0x21

                                            else
                                                Css.rgb 0x68 0x9D 0x6A
                                        , Css.active
                                            [ Css.backgroundColor <|
                                                if confirm then
                                                    Css.rgb 0xFA 0xBD 0x2F

                                                else
                                                    Css.rgb 0xB8 0xBB 0x26
                                            ]
                                        , Css.cursor Css.pointer
                                        , Css.color <| Css.rgb 0 0 0
                                        , Css.fontFamily Css.inherit
                                        , Css.borderStyle Css.none
                                        , Css.padding2 (Css.rem <| 1 / 4) (Css.rem <| 1 / 2)
                                        , Css.borderRadius <| Css.px 2
                                        , Css.fontWeight Css.bold
                                        ]
                                    ]
                                    [ Html.text <|
                                        if confirm then
                                            "reset (confirm?)"

                                        else
                                            "reset"
                                    ]
                            )
                        |> Maybe.withDefault (Html.text "")
                    ]
                ]
            ]
        ]
