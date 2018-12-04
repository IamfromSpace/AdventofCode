module Main exposing (..)

--import Dict exposing (Dict)

import Set exposing (Set)
import Html exposing (program)
import Html.Attributes as A
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


type alias Parsed =
    Int


type State
    = A
    | B
    | C
    | D
    | E
    | F


step : ( Set Int, Int, State ) -> ( Set Int, Int, State )
step ( tape, i, state ) =
    case state of
        A ->
            if not (Set.member i tape) then
                ( Set.insert i tape, i + 1, B )
            else
                ( Set.remove i tape, i - 1, F )

        B ->
            if not (Set.member i tape) then
                ( Set.remove i tape, i + 1, C )
            else
                ( Set.remove i tape, i + 1, D )

        C ->
            if not (Set.member i tape) then
                ( Set.insert i tape, i - 1, D )
            else
                ( Set.insert i tape, i + 1, E )

        D ->
            if not (Set.member i tape) then
                ( Set.remove i tape, i - 1, E )
            else
                ( Set.remove i tape, i - 1, D )

        E ->
            if not (Set.member i tape) then
                ( Set.remove i tape, i + 1, A )
            else
                ( Set.insert i tape, i + 1, C )

        F ->
            if not (Set.member i tape) then
                ( Set.insert i tape, i - 1, A )
            else
                ( Set.insert i tape, i + 1, A )


parse : String -> Parsed
parse =
    always 0


pt1 : Parsed -> Int
pt1 _ =
    let
        go i state =
            if i >= 12794428 then
                let
                    ( tape, _, _ ) =
                        state
                in
                    Set.size tape
            else
                go (i + 1) (step state)
    in
        go 0 ( Set.empty, 0, A )


pt2 : Parsed -> Int
pt2 =
    always 0


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
