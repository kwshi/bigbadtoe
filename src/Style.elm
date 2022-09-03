module Style exposing (..)

import Css exposing (..)


full : Style
full =
    batch <|
        position absolute
            :: List.map ((|>) <| Css.rem 0) [ top, bottom, left, right ]
