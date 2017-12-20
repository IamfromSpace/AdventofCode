module Main exposing (..)

import Html exposing (program)
import Html.Attributes as A
import ParseInt
import Input
import Dict exposing (Dict)


parseInt : String -> Int
parseInt s =
    let
        parse : String -> Int
        parse =
            ParseInt.parseInt >> Result.toMaybe >> Maybe.withDefault 0
    in
        case String.toList s of
            head :: tail ->
                if head == '-' then
                    (parse <| String.fromList tail) * -1
                else
                    parse s

            [] ->
                0


parse : String -> Dict Int Int
parse s =
    String.split " " s |> List.map parseInt |> List.indexedMap (,) |> Dict.fromList


pt1 : Dict Int Int -> ( Int, Int )
pt1 =
    let
        findMax =
            Dict.foldl
                (\k v ( p, max ) ->
                    if v > max then
                        ( k, v )
                    else
                        ( p, max )
                )
                ( 0, 0 )

        distribute amount from =
            Dict.map
                (\k v ->
                    v
                        + amount
                        // 16
                        + (if (amount % 16) > ((k - from - 1) % 16) then
                            1
                           else
                            0
                          )
                        - (if k == from then
                            amount
                           else
                            0
                          )
                )

        go : Int -> List ( Int, Dict Int Int ) -> Dict Int Int -> ( Int, Int )
        go count acc d =
            let
                ( from, amount ) =
                    findMax d
            in
                if count > 10000 then
                    ( -1, count )
                else
                    case List.filter (\( c, x ) -> x == d) acc of
                        [] ->
                            go (count + 1) (( count, d ) :: acc) (distribute amount from d)

                        ( c, _ ) :: _ ->
                            ( c, count )
    in
        go 0 []


pt2 : Dict Int Int -> Int
pt2 x =
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
                        , Html.pre [] [ Html.text <| (++) "Pt2: " <| toString <| pt2 p ]
                        , Html.pre [] [ Html.text <| (++) "Parsed Input: " <| toString p ]
                        ]
        , subscriptions = \_ -> Sub.none
        }
