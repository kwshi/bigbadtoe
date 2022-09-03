module Game.Player exposing
    ( Player(..)
    , color
    , colorBright
    , toString
    , toggle
    , viewCell
    , viewText
    )

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
            Css.rgba 0x9D 0x00 0x06 alpha

        O ->
            Css.rgba 0x07 0x66 0x78 alpha


colorBright : Player -> Css.Color
colorBright player =
    case player of
        X ->
            Css.rgb 0xFB 0x49 0x34

        O ->
            Css.rgb 0x83 0xA5 0x98


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


viewText : Player -> Html.Html msg
viewText player =
    Html.span
        [ Attr.css
            [ Css.color <| colorBright player
            , Css.fontWeight Css.bold
            ]
        ]
        [ Html.text <| toString player ]
