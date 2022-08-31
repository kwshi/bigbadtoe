module Main exposing (..)

import Browser
import Css
import Dict
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Ev


type Player
    = X
    | O


type Outcome
    = Incomplete
    | WonBy Player
    | Tie


type alias Board =
    Dict.Dict ( Int, Int ) Outcome


boardGet : ( Int, Int ) -> Board -> Outcome
boardGet pos =
    Dict.get pos >> Maybe.withDefault Incomplete


type alias Model =
    { board : Board
    , current : Player
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


toggle : Player -> Player
toggle player =
    case player of
        X ->
            O

        O ->
            X


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click row col ->
            ( { model
                | board = Dict.insert ( row, col ) (WonBy model.current) model.board
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


showPlayer : Player -> String
showPlayer mark =
    case mark of
        X ->
            "X"

        O ->
            "O"


collectOutcomes : List Outcome -> Outcome
collectOutcomes =
    List.foldl
        (\outcomeSoFar lineOutcome ->
            case ( outcomeSoFar, lineOutcome ) of
                ( WonBy _, _ ) ->
                    outcomeSoFar

                ( _, WonBy _ ) ->
                    lineOutcome

                ( Tie, _ ) ->
                    lineOutcome

                ( Incomplete, _ ) ->
                    Incomplete
        )
        Tie


getWinner : ( Int, Int ) -> Board -> Outcome
getWinner ( rowOffset, colOffset ) board =
    let
        rows =
            List.range rowOffset (rowOffset + 2)

        cols =
            List.range colOffset (colOffset + 2)

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

        getLineOutcome line =
            case List.map (\cell -> boardGet cell board) line of
                [ WonBy X, WonBy X, WonBy X ] ->
                    WonBy X

                [ WonBy O, WonBy O, WonBy O ] ->
                    WonBy O

                [ WonBy _, WonBy _, WonBy _ ] ->
                    Tie

                _ ->
                    Incomplete
    in
    List.map getLineOutcome lines |> collectOutcomes


makeMetaBoard : Board -> Board
makeMetaBoard board =
    generateCoordinates 3
        |> List.map (\( r, c ) -> ( ( r, c ), getWinner ( 3 * r, 3 * c ) board ))
        |> Dict.fromList


viewBody : Model -> Html.Html Msg
viewBody model =
    let
        metaBoard =
            makeMetaBoard model.board

        outcome =
            getWinner ( 0, 0 ) metaBoard

        active =
            outcome == Incomplete
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
        , Html.text <| "current turn: " ++ showPlayer model.current
        , Html.text <|
            case outcome of
                WonBy p ->
                    "winner! " ++ showPlayer p

                Tie ->
                    "tie!"

                Incomplete ->
                    ""
        , Html.button [ Ev.onClick Restart ] [ Html.text "restart" ]
        ]


viewBoard : Bool -> Board -> Board -> Html.Html Msg
viewBoard active board metaBoard =
    Html.div
        [ Attr.css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "repeat(9, 2rem)"
            , Css.property "grid-template-rows" "repeat(9, 2rem)"
            , Css.property "gap" ".25rem"
            , Css.fontFamily Css.monospace
            , Css.fontWeight Css.bold
            , Css.fontSize <| Css.rem 1.5
            ]
        ]
    <|
        (generateCoordinates 9
            |> List.map
                (\( row, col ) ->
                    let
                        cell =
                            boardGet ( row, col ) board

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
                                case cell of
                                    WonBy X ->
                                        Css.hex "cc241d"

                                    WonBy O ->
                                        Css.hex "458588"

                                    _ ->
                                        Css.hex "32302f"
                            ]
                        , Ev.onClick <|
                            case ( active, cell ) of
                                ( True, Incomplete ) ->
                                    Click row col

                                _ ->
                                    Nop
                        ]
                        [ Html.text <|
                            case cell of
                                WonBy p ->
                                    showPlayer p

                                _ ->
                                    ""
                        ]
                )
        )
            ++ (Dict.toList metaBoard
                    |> List.map
                        (\( ( metaRow, metaCol ), outcome ) ->
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
                                    , Css.fontSize <| Css.rem 6
                                    , Css.borderRadius <| Css.px 8
                                    , case outcome of
                                        WonBy X ->
                                            Css.backgroundColor <| Css.hex "cc241da0"

                                        WonBy O ->
                                            Css.backgroundColor <| Css.hex "458588a0"

                                        Incomplete ->
                                            Css.visibility Css.hidden

                                        Tie ->
                                            Css.backgroundColor <| Css.hex "928374e0"
                                    ]
                                ]
                                [ Html.text <|
                                    case outcome of
                                        WonBy p ->
                                            showPlayer p

                                        _ ->
                                            ""
                                ]
                        )
               )
