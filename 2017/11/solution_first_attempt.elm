module Main exposing (..)

import Char
import Bitwise
import Html exposing (program)
import Html.Attributes as A
import Dict exposing (Dict)
import Set exposing (Set)
import Input


parse : String -> List String
parse =
    String.split ","


countNorths : List String -> { n : Int, nw : Int, ne : Int }
countNorths =
    List.foldr
        (\s acc ->
            case s of
                "n" ->
                    { acc | n = acc.n + 1 }

                "nw" ->
                    { acc | nw = acc.nw + 1 }

                "ne" ->
                    { acc | ne = acc.ne + 1 }

                "s" ->
                    { acc | n = acc.n - 1 }

                "sw" ->
                    { acc | ne = acc.ne - 1 }

                _ ->
                    { acc | nw = acc.nw - 1 }
        )
        { n = 0, nw = 0, ne = 0 }


sign : comparable -> number
sign i =
    if i >= 0 then
        1
    else
        -1


northsToTuple : { n : Int, nw : Int, ne : Int } -> ( Int, Int, Int )
northsToTuple { n, nw, ne } =
    if sign nw == sign ne then
        ( nw, n, ne )
    else if sign nw /= sign n then
        ( n, ne, -nw )
    else
        ( -ne, nw, n )


tupleToMin : ( Int, Int, Int ) -> Int
tupleToMin ( left, center, right ) =
    abs <|
        center
            + (if abs left > abs right then
                left
               else
                right
              )


pt1 : List String -> Int
pt1 =
    countNorths >> northsToTuple >> tupleToMin


pt2 : List String -> Int
pt2 s =
    0


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
                        [ Html.pre [] [ Html.text <| (++) "Pt1: " <| toString <| pt1 p ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex1 (3): " <| toString <| pt1 <| parse Input.p1ex1 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex2 (0): " <| toString <| pt1 <| parse Input.p1ex2 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex3 (2)`: " <| toString <| pt1 <| parse Input.p1ex3 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1ex4 (3): " <| toString <| pt1 <| parse Input.p1ex4 ]
                        , Html.pre [] [ Html.text <| (++) "Pt2: " <| toString <| pt2 p ]
                        , Html.pre [] [ Html.text <| (++) "parsed: " <| toString <| p ]
                        ]
        , subscriptions = \_ -> Sub.none
        }
