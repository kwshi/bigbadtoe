module Game.Outcome exposing
    ( Outcome(..)
    , collectLineOutcomes
    , color
    , colorBright
    , fromSingleCell
    , getLineOutcome
    , getWinner
    , viewStatus
    , viewSubgrid
    )

import Css
import Game.Player as Player
import Html.Styled as Html
import Html.Styled.Attributes as Attr


type Outcome
    = Incomplete
    | WonBy Player.Player
    | Tie


color : Float -> Outcome -> Css.Color
color alpha outcome =
    case outcome of
        Incomplete ->
            Css.rgba 0 0 0 0

        WonBy player ->
            Player.color alpha player

        Tie ->
            Css.rgba 0x7C 0x6F 0x64 alpha


colorBright : Outcome -> Css.Color
colorBright outcome =
    case outcome of
        WonBy player ->
            Player.colorBright player

        _ ->
            Css.rgb 0xEB 0xDB 0xB2


collectLineOutcomes : List Outcome -> Outcome
collectLineOutcomes =
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


getLineOutcome : List Outcome -> Outcome
getLineOutcome =
    List.foldl
        (\cellOutcome outcomeSoFar ->
            case ( outcomeSoFar, cellOutcome ) of
                ( Nothing, WonBy _ ) ->
                    Just cellOutcome

                ( _, Incomplete ) ->
                    Just Incomplete

                ( Just Incomplete, _ ) ->
                    Just Incomplete

                ( Just Tie, _ ) ->
                    Just Tie

                ( _, Tie ) ->
                    Just Tie

                ( Just (WonBy p), WonBy q ) ->
                    if p == q then
                        outcomeSoFar

                    else
                        Just Tie
        )
        Nothing
        >> Maybe.withDefault Incomplete


fromSingleCell : Maybe Player.Player -> Outcome
fromSingleCell =
    Maybe.map WonBy >> Maybe.withDefault Incomplete


getWinner : Outcome -> Maybe Player.Player
getWinner outcome =
    case outcome of
        WonBy p ->
            Just p

        _ ->
            Nothing


viewSubgrid : Outcome -> Html.Html Int
viewSubgrid outcome =
    Html.div
        [ Attr.css <|
            [ Css.position Css.absolute
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.color <| Css.rgb 255 255 255
            , if outcome == Incomplete then
                Css.visibility Css.hidden

              else
                Css.visibility Css.visible
            , Css.fontSize <| Css.rem 6
            , Css.backgroundColor <| color (3 / 4) outcome
            ]
                ++ List.map ((|>) <| Css.rem 0)
                    [ Css.top, Css.bottom, Css.left, Css.right ]
        ]
        [ Html.text
            (getWinner outcome
                |> Maybe.map Player.toString
                |> Maybe.withDefault ""
            )
        ]


viewStatus : Player.Player -> Outcome -> Html.Html msg
viewStatus currentPlayer outcome =
    Html.span [] <|
        case outcome of
            WonBy player ->
                [ Html.text "mmm… ", Player.viewText player, Html.text " wins." ]

            Tie ->
                [ Html.text "hmmm… it's a tie." ]

            Incomplete ->
                [ Player.viewText currentPlayer, Html.text "’s turn to play." ]
