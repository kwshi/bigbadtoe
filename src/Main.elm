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


generateCoordinates : Int -> List ( Int, Int )
generateCoordinates n =
    let
        axis =
            List.range 0 (n - 1)
    in
    List.concatMap (\a -> List.map (Tuple.pair a) axis) axis


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
    generateCoordinates 3
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
    Html.main_
        [ Attr.css
            [ Css.displayFlex
            , Css.backgroundColor <| Css.hex "1d2021"
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.position Css.absolute
            , Css.top <| Css.rem 0
            , Css.bottom <| Css.rem 0
            , Css.left <| Css.rem 0
            , Css.right <| Css.rem 0
            , Css.paddingBottom <| Css.rem 8
            , Css.fontFamily Css.sansSerif
            ]
        ]
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
    Html.div
        [ Attr.css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "repeat(9, 1.5rem)"
            , Css.property "grid-template-rows" "repeat(9, 1.5rem)"
            , Css.property "gap" ".25rem"
            , Css.fontFamily Css.monospace
            , Css.fontWeight Css.bold
            ]
        ]
    <|
        (generateCoordinates 9
            |> List.map
                (\( row, col ) ->
                    let
                        mark =
                            Dict.get ( row, col ) board

                        br i j =
                            if modBy 3 row == i && modBy 3 col == j then
                                Css.px 8

                            else
                                Css.px 2
                    in
                    Html.div
                        [ Attr.css
                            [ Css.textAlign Css.center
                            , Css.property "grid-row" <| String.fromInt <| row + 1
                            , Css.property "grid-column" <| String.fromInt <| col + 1
                            , Css.borderRadius4 (br 0 0) (br 0 2) (br 2 2) (br 2 0)
                            , Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.justifyContent Css.center
                            , Css.color <| Css.rgb 255 255 255
                            , Css.backgroundColor <|
                                case mark of
                                    Just X ->
                                        Css.hex "cc241d"

                                    Just O ->
                                        Css.hex "458588"

                                    Nothing ->
                                        Css.hex "32302f"
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
            ++ (Dict.toList metaBoard
                    |> List.map
                        (\( ( metaRow, metaCol ), mark ) ->
                            Html.div
                                [ Attr.css
                                    [ Css.property "grid-row-start" <| String.fromInt <| 3 * metaRow + 1
                                    , Css.property "grid-column-start" <| String.fromInt <| 3 * metaCol + 1
                                    , Css.property "grid-row-end" "span 3"
                                    , Css.property "grid-column-end" "span 3"
                                    , Css.color <| Css.rgb 255 255 255
                                    , Css.displayFlex
                                    , Css.alignItems Css.center
                                    , Css.justifyContent Css.center
                                    , Css.fontSize <| Css.rem 4
                                    , Css.borderRadius <| Css.px 8
                                    , Css.backgroundColor <|
                                        case mark of
                                            X ->
                                                Css.hex "cc241d80"

                                            O ->
                                                Css.hex "45858880"
                                    ]
                                ]
                                [ Html.text <| showMark mark ]
                        )
               )
