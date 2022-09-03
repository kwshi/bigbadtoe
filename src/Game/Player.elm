module Game.Player exposing (Player(..), color, toString, toggle, viewCell)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events


type Player
    = X
    | O


toggle : Player -> Player
toggle player =
    case player of
        X ->
            O

        O ->
            X


toString : Player -> String
toString player =
    case player of
        X ->
            "X"

        O ->
            "O"


color : Float -> Player -> Css.Color
color alpha player =
    case player of
        X ->
            Css.rgba 0xCC 0x24 0x1D alpha

        O ->
            Css.rgba 0x45 0x85 0x88 alpha


viewCell : Maybe Player -> Html.Html ()
viewCell cell =
    Html.div
        ((case cell of
            Just _ ->
                identity

            Nothing ->
                (::) <| Events.onClick ()
         )
            [ Attr.css
                [ Css.textAlign Css.center
                , Css.width <| Css.rem 3
                , Css.height <| Css.rem 3
                , Css.borderRadius <| Css.rem (1 / 3)
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                , Css.color <| Css.rgb 255 255 255
                , Css.backgroundColor <|
                    Maybe.withDefault (Css.rgb 0x32 0x30 0x2F) <|
                        Maybe.map (color 1) cell
                ]
            ]
        )
        [ Html.text <| Maybe.withDefault "" <| Maybe.map toString cell ]
