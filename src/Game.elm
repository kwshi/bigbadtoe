module Game exposing (..)

import Css
import Game.Grid as Grid
import Game.Outcome as Outcome
import Game.Player as Player
import Html.Styled as Html
import Html.Styled.Attributes as Attr


type State
    = State { grid : Grid.GridMeta, current : Player.Player }


init : Int -> Int -> State
init metaSize size =
    State
        { grid = Grid.init metaSize <| Grid.init size Nothing
        , current = Player.X
        }


click : ( Int, Int ) -> State -> State
click ( i, j ) (State s) =
    State
        { grid = Grid.update i (Grid.set j <| Just s.current) s.grid
        , current = Player.toggle s.current
        }


render : State -> { outcome : Outcome.Outcome, view : Html.Html ( Int, Int ) }
render (State s) =
    let
        renderSubgrid subgrid =
            let
                outcome =
                    Grid.map Outcome.fromSingleCell subgrid |> Grid.getOutcome
            in
            { outcome = outcome
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
                        Grid.view Player.viewCell subgrid
                    , Outcome.subgridView outcome
                    ]
            }

        subgrids =
            Grid.map renderSubgrid s.grid
    in
    { outcome = Grid.map .outcome subgrids |> Grid.getOutcome
    , view =
        Html.div
            [ Attr.css
                [ Css.property "grid-area" "board"
                , Css.property "user-select" "none"
                ]
            ]
            [ Grid.view .view subgrids ]
    }
