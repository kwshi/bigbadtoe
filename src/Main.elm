module Main exposing (..)

import Browser
import Css
import Dict
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev


type Mark
    = X
    | O


type alias Board =
    Dict.Dict ( Int, Int ) Mark


type alias Model =
    { board : Board
    , current : Mark
    }


type Msg
    = Click Int Int
    | Restart
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
    ( { board = Dict.empty, current = X }, Cmd.none )


toggle : Mark -> Mark
toggle mark =
    case mark of
        X ->
            O

        O ->
            X


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click row col ->
            ( { model
                | board = Dict.insert ( row, col ) model.current model.board
                , current = toggle model.current
              }
            , Cmd.none
            )

        Restart ->
            init ()

        Nop ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "hi"
    , body = [ Html.toUnstyled <| viewBody model ]
    }


showMark : Mark -> String
showMark mark =
    case mark of
        X ->
            "X"

        O ->
            "O"


getWinner : ( Int, Int ) -> Board -> Maybe Mark
getWinner ( rowOffset, colOffset ) board =
    let
        rows =
            [ rowOffset, rowOffset + 1, rowOffset + 2 ]

        cols =
            [ colOffset, colOffset + 1, colOffset + 2 ]

        lines =
            -- horizontals
            List.map (\r -> List.map (\c -> ( r, c )) cols) rows
                ++ -- verticals
                   List.map (\c -> List.map (\r -> ( r, c )) rows) cols
                ++ [ -- diagonal
                     List.map2 Tuple.pair rows cols
                   , --antidiagonal
                     List.map2 Tuple.pair (List.reverse rows) cols
                   ]

        getLine line =
            case List.filterMap (\cell -> Dict.get cell board) line of
                [ X, X, X ] ->
                    Just X

                [ O, O, O ] ->
                    Just O

                _ ->
                    Nothing
    in
    List.filterMap getLine lines |> List.head


makeMetaBoard : Board -> Board
makeMetaBoard board =
    List.concatMap (\r -> List.map (Tuple.pair r) [ 0, 1, 2 ]) [ 0, 1, 2 ]
        |> List.foldl
            (\( r, c ) ->
                case getWinner ( 3 * r, 3 * c ) board of
                    Nothing ->
                        identity

                    Just mark ->
                        Dict.insert ( r, c ) mark
            )
            Dict.empty


viewBody : Model -> Html.Html Msg
viewBody model =
    let
        metaBoard =
            makeMetaBoard model.board

        winner =
            getWinner ( 0, 0 ) metaBoard

        active =
            Maybe.map (always False) winner |> Maybe.withDefault True
    in
    Html.main_ []
        [ viewBoard active model.board metaBoard
        , Html.text <| "current turn: " ++ showMark model.current
        , Maybe.map viewWinner winner |> Maybe.withDefault (Html.text "")
        , Html.button [ Ev.onClick Restart ] [ Html.text "restart" ]
        ]


viewWinner : Mark -> Html.Html Msg
viewWinner winner =
    Html.div []
        [ Html.text <| "winner! " ++ showMark winner
        ]


viewBoard : Bool -> Board -> Board -> Html.Html Msg
viewBoard active board metaBoard =
    let
        inds =
            List.range 0 8

        templateSpec =
            "repeat(9, 1.5rem)"
    in
    Html.div
        [ Attr.css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" templateSpec
            , Css.property "grid-template-rows" templateSpec
            ]
        ]
    <|
        (inds
            |> List.concatMap
                (\row ->
                    inds
                        |> List.map
                            (\col ->
                                let
                                    mark =
                                        Dict.get ( row, col ) board
                                in
                                Html.div
                                    [ Attr.css
                                        [ Css.border3 (Css.px 1) Css.solid <| Css.rgb 0 0 0
                                        , Css.textAlign Css.center
                                        , Css.property "grid-row" <| String.fromInt <| row + 1
                                        , Css.property "grid-column" <| String.fromInt <| col + 1
                                        , Css.color <| Css.rgb 255 255 255
                                        , Css.backgroundColor <|
                                            case mark of
                                                Just X ->
                                                    Css.rgb 255 0 0

                                                Just O ->
                                                    Css.rgb 0 0 255

                                                Nothing ->
                                                    Css.rgba 0 0 0 0
                                        ]
                                    , Ev.onClick <|
                                        if active then
                                            Maybe.map (always Nop) mark
                                                |> Maybe.withDefault (Click row col)

                                        else
                                            Nop
                                    ]
                                    [ Maybe.map showMark mark
                                        |> Maybe.withDefault ""
                                        |> Html.text
                                    ]
                            )
                )
        )
            ++ (Dict.toList metaBoard
                    |> List.map
                        (\( ( metaRow, metaCol ), mark ) ->
                            Html.div
                                [ Attr.css
                                    [ Css.property "grid-row-start" <| String.fromInt <| 3 * metaRow + 1
                                    , Css.property "grid-column-start" <| String.fromInt <| 3 * metaCol + 1
                                    , Css.property "grid-row-end" "span 3"
                                    , Css.property "grid-column-end" "span 3"
                                    , Css.backgroundColor <|
                                        case mark of
                                            X ->
                                                Css.rgba 255 0 0 0.5

                                            O ->
                                                Css.rgba 0 0 255 0.5
                                    ]
                                ]
                                []
                        )
               )
