module KnotHash exposing (..)

import Char
import Bitwise
import Html exposing (program)
import Html.Attributes as A
import Dict exposing (Dict)
import ParseInt
import Input


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


parse : String -> List Int
parse =
    String.split "," >> List.map parseInt


zip : (a -> b -> c) -> List a -> List b -> List c
zip fn =
    let
        go acc x y =
            case x of
                h :: t ->
                    case y of
                        h2 :: t2 ->
                            go (fn h h2 :: acc) t t2

                        [] ->
                            acc

                [] ->
                    acc
    in
        go []


step : Int -> Int -> Int -> Dict Int Int -> Dict Int Int
step depth from count original =
    let
        base =
            List.range from (from + count - 1) |> List.map (\x -> x % depth)

        list =
            zip (,) base (List.reverse base)
    in
        List.foldr (\( from, to ) -> Dict.update to (\_ -> Dict.get from original)) original list


startDict : Int -> Dict Int Int
startDict depth =
    let
        base =
            List.range 0 (depth - 1)
    in
        Dict.fromList (zip (,) base base)


hashRound : Int -> ( Dict Int Int, Int, Int ) -> List Int -> ( Dict Int Int, Int, Int )
hashRound depth ( acc, skipSize, i ) list =
    case list of
        count :: tail ->
            hashRound
                depth
                ( step depth i count acc
                , (skipSize + 1) % depth
                , (count + skipSize + i) % depth
                )
                tail

        [] ->
            ( acc, skipSize, i )


pt1 : List Int -> Dict Int Int
pt1 =
    hashRound 256 ( startDict 256, 0, 0 ) >> fst3


parse2 : String -> List Int
parse2 =
    String.toList >> List.map Char.toCode >> flip (++) [ 17, 31, 73, 47, 23 ]


sparseToDense : Dict Int Int -> List Int
sparseToDense d =
    let
        go accList accItem i =
            if i > 255 then
                accList
            else
                let
                    nextItem =
                        Bitwise.xor (Dict.get i d |> Maybe.withDefault 0) accItem
                in
                    if i % 16 == 15 then
                        go (nextItem :: accList) 0 (i + 1)
                    else
                        go accList nextItem (i + 1)
    in
        go [] 0 0 |> List.reverse


toHash : List Int -> String
toHash =
    List.map ParseInt.toHex >> List.map (zeroPad2 >> String.toLower) >> List.foldr (++) ""


zeroPad2 : String -> String
zeroPad2 s =
    if String.length s == 2 then
        s
    else
        "0" ++ s


fst3 : ( a, b, c ) -> a
fst3 ( a, _, _ ) =
    a


pt2 : List Int -> String
pt2 =
    List.repeat 64
        >> List.concat
        >> hashRound 256 ( startDict 256, 0, 0 )
        >> fst3
        >> sparseToDense
        >> toHash


knotHash : String -> String
knotHash =
    parse2 >> pt2


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

                    p2 =
                        parse2 Input.input
                in
                    Html.div [ A.style [ ( "padding", "5px" ) ] ]
                        [ Html.pre [] [ Html.text <| (++) "Pt1: " <| toString <| pt1 p ]
                        , Html.pre [] [ Html.text <| (++) "Pt2: " <| toString <| pt2 p2 ]
                        , Html.pre [] [ Html.text <| (++) "aoc: " <| toString <| "33efeb34ea91902bb2f59c9920caa6cd" ]
                        , Html.pre [] [ Html.text <| (++) "err: " <| toString <| "70b856a24d586194331398c7fcfaaaf" ]
                        , Html.pre [] [ Html.text <| (++) "parsed: " <| toString <| p ]
                        , Html.pre [] [ Html.text <| (++) "parsed2: " <| toString <| p2 ]
                        ]
        , subscriptions = \_ -> Sub.none
        }
