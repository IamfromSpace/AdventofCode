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


parse : String -> String
parse =
    identity


fst : ( a, b, c, d, e, f ) -> a
fst ( head, inGarbage, ignoreNext, depth, score, _ ) =
    head


pt1 : String -> ( Int, Int )
pt1 s =
    let
        go inGarbage ignoreNext depth score cancelled list =
            case list of
                head :: tail ->
                    if ignoreNext then
                        case fst <| Debug.log "head" ( head, inGarbage, ignoreNext, depth, score, cancelled ) of
                            _ ->
                                go inGarbage False depth score cancelled tail
                    else if inGarbage then
                        case fst <| Debug.log "head" ( head, inGarbage, ignoreNext, depth, score, cancelled ) of
                            '>' ->
                                go False False depth score cancelled tail

                            '!' ->
                                go True True depth score cancelled tail

                            _ ->
                                go True False depth score (cancelled + 1) tail
                    else
                        case fst <| Debug.log "head" ( head, inGarbage, ignoreNext, depth, score, cancelled ) of
                            '<' ->
                                go True False depth score cancelled tail

                            '!' ->
                                go False True depth score cancelled tail

                            '{' ->
                                go False False (depth + 1) score cancelled tail

                            '}' ->
                                if depth > 0 then
                                    go False False (depth - 1) (score + depth) cancelled tail
                                else
                                    go False False depth score cancelled tail

                            _ ->
                                go False False depth score cancelled tail

                [] ->
                    ( score, cancelled )
    in
        go False False 0 0 0 (String.toList s)


pt2 : a -> Int
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
