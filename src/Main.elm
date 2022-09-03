module Main exposing (..)

import Browser
import Css
import Game.Grid
import Game.Outcome
import Game.Player
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev
import Style


type alias Model =
    { grid : Game.Grid.Grid (Game.Grid.Grid (Maybe Game.Player.Player))
    , currentPlayer : Game.Player.Player
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
    ( { grid = Game.Grid.init 3 <| Game.Grid.init 3 Nothing
      , currentPlayer = Game.Player.X
      , resetConfirm = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ( i, j ) ->
            ( { model
                | grid =
                    Game.Grid.update i
                        (Game.Grid.set j <| Just model.currentPlayer)
                        model.grid
                , currentPlayer = Game.Player.toggle model.currentPlayer
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


viewHeader : Html.Html msg
viewHeader =
    Html.header
        [ Attr.css
            [ Css.displayFlex
            , Css.padding2 (Css.rem 0) (Css.rem 1)
            ]
        ]
        [ Html.h1
            [ Attr.css
                [ Css.margin <| Css.rem 0
                , Css.fontSize <| Css.rem 2
                , Css.alignSelf Css.baseline
                ]
            ]
          <|
            List.map
                (\( outcome, text ) ->
                    Html.span
                        [ Attr.css [ Css.color <| Game.Outcome.colorBright outcome ] ]
                        [ Html.text text ]
                )
                [ ( Game.Outcome.WonBy Game.Player.X, "big" )
                , ( Game.Outcome.WonBy Game.Player.O, "bad" )
                , ( Game.Outcome.Tie, "toe" )
                ]
        , Html.h2
            [ Attr.css
                [ Css.property "grid-area" "by"
                , Css.margin <| Css.rem 0
                , Css.marginLeft <| Css.rem 1
                , Css.fontSize <| Css.rem 1
                , Css.color <| Css.hex "ebdbb2"
                , Css.fontWeight Css.normal
                , Css.alignSelf Css.baseline
                ]
            ]
            [ Html.text "by "
            , Html.a
                [ Attr.href "https://kshi.xyz"
                , Attr.css
                    [ Css.fontWeight Css.bold
                    , Css.color <| Css.rgb 0x68 0x9D 0x6A
                    , Css.property "text-decoration-color" "#689d6a80"
                    , Css.hover
                        [ Css.color <| Css.rgb 0x8E 0xC0 0x7C ]
                    ]
                ]
                [ Html.text "kshi" ]
            ]
        ]


viewBody : Model -> Html.Html Msg
viewBody model =
    let
        renderSubgrid subgrid =
            let
                subOutcome =
                    Game.Grid.map Game.Outcome.fromSingleCell subgrid |> Game.Grid.getOutcome
            in
            { outcome = subOutcome
            , view =
                Html.div
                    [ Attr.css
                        [ Css.position Css.relative
                        , Css.overflow Css.hidden
                        , Css.borderRadius <| Css.rem 1
                        , Css.margin <| Css.rem <| 1 / 4
                        ]
                    ]
                    [ Html.map Tuple.first <|
                        Game.Grid.view Game.Player.viewCell subgrid
                    , Game.Outcome.viewSubgrid subOutcome
                    ]
            }

        subgrids =
            Game.Grid.map renderSubgrid model.grid

        outcome =
            Game.Grid.map .outcome subgrids |> Game.Grid.getOutcome
    in
    Html.main_
        [ Attr.css
            [ Css.backgroundColor <| Css.rgb 0x1D 0x20 0x21
            , Style.full
            ]
        ]
        [ Html.div
            [ Attr.css <|
                [ Css.displayFlex
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
                                [ [ "header", "header" ]
                                , [ "board", "board" ]
                                , [ "control", "status" ]
                                ]
                    , Css.property "column-gap" ".5rem"
                    , Css.property "row-gap" ".5rem"
                    , Css.property "grid-template-columns" "auto auto auto"
                    ]
                ]
                [ viewHeader
                , Html.div
                    [ Attr.css
                        [ Css.property "grid-area" "board"
                        , Css.property "user-select" "none"
                        , Css.backgroundColor <| Game.Outcome.color 1 outcome
                        , Css.padding <| Css.rem <| 3 / 4
                        , Css.borderRadius <| Css.rem <| 7 / 4
                        ]
                    ]
                    [ Game.Grid.view .view subgrids ]
                    |> Html.map
                        (if outcome == Game.Outcome.Incomplete then
                            Click

                         else
                            always Nop
                        )
                , Html.div
                    [ Attr.css
                        [ Css.property "grid-area" "control"
                        , Css.minHeight <| Css.rem 2
                        , Css.alignSelf Css.center
                        , Css.fontSize <| Css.rem 1
                        , Css.padding2 (Css.rem 0) (Css.rem 1)
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
                                                Css.rgb 0x98 0x97 0x1A
                                        , Css.hover
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
                                        , Css.borderRadius <| Css.rem <| 1 / 4
                                        , Css.fontWeight Css.bold
                                        , Css.fontSize Css.inherit
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
                , Html.div
                    [ Attr.css
                        [ Css.property "grid-area" "status"
                        , Css.padding2 (Css.rem 0) (Css.rem 1)
                        , Css.textAlign Css.right
                        , Css.color <| Css.rgb 0xEB 0xDB 0xB2
                        , Css.fontSize <| Css.rem 1
                        , Css.alignSelf Css.center
                        ]
                    ]
                    [ Game.Outcome.viewStatus model.currentPlayer outcome ]
                ]
            ]
        ]
