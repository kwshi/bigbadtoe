module Game.Grid exposing (..)

import Array
import Css
import Game.Outcome as Outcome
import Game.Player as Player
import Html.Styled as Html
import Html.Styled.Attributes as Attr


type Grid cell
    = Grid { size : Int, array : Array.Array cell }


type alias GridSingle =
    Grid (Maybe Player.Player)


type alias GridMeta =
    Grid GridSingle


get : Int -> Grid a -> Maybe a
get i (Grid g) =
    Array.get i g.array


update : Int -> (a -> a) -> Grid a -> Grid a
update i f grid =
    get i grid
        |> Maybe.map (\a -> set i (f a) grid)
        |> Maybe.withDefault grid


set : Int -> a -> Grid a -> Grid a
set i a (Grid g) =
    Grid { g | array = Array.set i a g.array }


init : Int -> a -> Grid a
init size a =
    Grid { size = size, array = Array.initialize (size * size) <| always a }


generateCoordinates : Int -> List ( Int, Int )
generateCoordinates n =
    let
        component =
            List.range 0 (n - 1)
    in
    List.concatMap (\i -> List.map (Tuple.pair i) component) component


positionToIndex : Int -> ( Int, Int ) -> Int
positionToIndex size ( row, col ) =
    row * size + col


generateLineIndices : Int -> List (List Int)
generateLineIndices size =
    let
        component =
            List.range 0 (size - 1)

        toIndex row col =
            positionToIndex size ( row, col )
    in
    -- diagonal
    List.map2 toIndex component component
        :: -- antidiagonal
           List.map2 toIndex (List.reverse component) component
        :: -- horizontals
           List.map (\r -> List.map (toIndex r) component) component
        ++ -- verticals
           List.map (\c -> List.map (\r -> toIndex r c) component) component


map : (a -> b) -> Grid a -> Grid b
map f (Grid g) =
    Grid { size = g.size, array = Array.map f g.array }


getOutcome : Grid Outcome.Outcome -> Outcome.Outcome
getOutcome (Grid g) =
    generateLineIndices g.size
        |> List.map
            (List.map
                (\i ->
                    Array.get i g.array
                        |> Maybe.withDefault Outcome.Incomplete
                )
                >> Outcome.getLineOutcome
            )
        |> Outcome.collectLineOutcomes


view : (a -> Html.Html msg) -> Grid a -> Html.Html ( Int, msg )
view viewCell (Grid g) =
    Html.div
        [ Attr.css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" <| "repeat(" ++ String.fromInt g.size ++ ", 1fr)"
            , Css.property "grid-template-rows" <| "repeat(" ++ String.fromInt g.size ++ ", 1fr)"
            , Css.property "gap" ".5rem"
            , Css.fontWeight Css.bold
            , Css.fontSize <| Css.rem 1.5
            ]
        ]
        (Array.toList g.array
            |> List.indexedMap (\i -> Html.map (Tuple.pair i) << viewCell)
        )
