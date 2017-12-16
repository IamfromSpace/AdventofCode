module Main exposing (..)

import Char
import Bitwise
import Html exposing (program)
import Html.Attributes as A
import Dict exposing (Dict)
import Set exposing (Set)
import Input


parse : String -> List Float
parse =
    String.split ","
        >> List.map
            (\s ->
                case s of
                    "nw" ->
                        pi / 6

                    "n" ->
                        pi / 2

                    "ne" ->
                        5 * pi / 6

                    "se" ->
                        7 * pi / 6

                    "s" ->
                        3 * pi / 2

                    _ ->
                        11 * pi / 6
            )


move : Float -> ( Float, Float ) -> ( Float, Float )
move d ( x, y ) =
    ( x + cos d, y + sin d )


angle : ( Float, Float ) -> ( Float, Float ) -> Float
angle ( xf, yf ) ( xt, yt ) =
    atan2 (yt - yf) (xt - xf) + pi


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( xf, yf ) ( xt, yt ) =
    sqrt ((xf - xt) ^ 2 + (yf - yt) ^ 2)


stepsTo : Int -> ( Float, Float ) -> ( Float, Float ) -> Int
stepsTo steps from to =
    if (distance from to) < 0.5 || steps > 2000 then
        steps
    else
        let
            optAngle =
                angle from to

            a =
                (toFloat (round ((optAngle + 5 * pi / 6) / (pi / 3)))) * (pi / 3) + pi / 6
        in
            stepsTo (steps + 1) (move a from) to


pt1 : List Float -> Int
pt1 =
    List.foldr move ( 0, 0 ) >> flip (stepsTo 0) ( 0, 0 )


pt2 : List Float -> Int
pt2 =
    let
        getVisited =
            List.foldl
                (\direction ( visited, point ) ->
                    let
                        newPoint =
                            move direction point
                    in
                        ( newPoint :: visited, newPoint )
                )
                ( [ ( 0, 0 ) ], ( 0, 0 ) )
                >> Tuple.first
    in
        getVisited
            >> List.map (flip (stepsTo 0) ( 0, 0 ))
            >> List.maximum
            >> Maybe.withDefault -1


easyWay : String -> Maybe ( Int, Int )
easyWay =
    let
        countSteps ( x, y, z ) =
            (abs x + abs y + abs z) // 2
    in
        String.split ","
            >> List.foldl
                (\s ->
                    Maybe.andThen
                        (\( ( x, y, z ), prevMax ) ->
                            let
                                nextPoint =
                                    case s of
                                        "n" ->
                                            Just ( x, y + 1, z - 1 )

                                        "nw" ->
                                            Just ( x - 1, y + 1, z )

                                        "sw" ->
                                            Just ( x - 1, y, z + 1 )

                                        "s" ->
                                            Just ( x, y - 1, z + 1 )

                                        "se" ->
                                            Just ( x + 1, y - 1, z )

                                        "ne" ->
                                            Just ( x + 1, y, z - 1 )

                                        _ ->
                                            Nothing
                            in
                                Maybe.map (\n -> ( n, max (countSteps n) prevMax )) nextPoint
                        )
                )
                (Just ( ( 0, 0, 0 ), 0 ))
            >> Maybe.map (Tuple.mapFirst countSteps)


main : Program Never () a
main =
    program
        { init = ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , view =
            \_ ->
                let
                    p =
                        parse Input.input
                in
                    Html.div [ A.style [ ( "padding", "5px" ) ] ]
                        [ Html.pre [] [ Html.text <| (++) "easyWay: " <| toString <| easyWay Input.input ]
                        , Html.pre [] [ Html.text <| (++) "Pt1: " <| toString <| pt1 p ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex1 (3): " <| toString <| pt1 <| parse Input.p1ex1 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex2 (0): " <| toString <| pt1 <| parse Input.p1ex2 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex3 (2): " <| toString <| pt1 <| parse Input.p1ex3 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex4 (3): " <| toString <| pt1 <| parse Input.p1ex4 ]
                        , Html.pre [] [ Html.text <| (++) "Pt2: " <| toString <| pt2 p ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex1 (3): " <| toString <| pt2 <| parse Input.p1ex1 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex2 (2): " <| toString <| pt2 <| parse Input.p1ex2 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex3 (2): " <| toString <| pt2 <| parse Input.p1ex3 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex4 (3): " <| toString <| pt2 <| parse Input.p1ex4 ]
                        , Html.pre [] [ Html.text <| (++) "parsed: " <| toString <| p ]
                        ]
        , subscriptions = \_ -> Sub.none
        }
