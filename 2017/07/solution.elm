module Main exposing (..)

import Html exposing (program)
import Html.Attributes as A
import ParseInt
import Input
import Dict exposing (Dict)
import Set exposing (Set)


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


parse : String -> Dict String ( Set String, Int )
parse x =
    String.split "\n" x
        |> List.map
            (\s ->
                case String.split " -> " s of
                    nameWeight :: t ->
                        let
                            ( name, weight ) =
                                case String.split " " nameWeight of
                                    name :: remainder ->
                                        case remainder of
                                            weightString :: _ ->
                                                ( name, weightString |> String.dropLeft 1 |> String.dropRight 1 |> parseInt )

                                            _ ->
                                                ( name, 0 )

                                    _ ->
                                        ( "", 0 )
                        in
                            case t of
                                discs :: _ ->
                                    ( name, ( String.split ", " discs |> Set.fromList, weight ) )

                                _ ->
                                    ( name, ( Set.empty, weight ) )

                    _ ->
                        ( "", ( Set.empty, 0 ) )
            )
        |> Dict.fromList


pt1 : Dict String ( Set String, Int ) -> String
pt1 x =
    let
        allSets =
            Dict.toList x |> List.map (Tuple.second >> Tuple.first) |> List.foldr Set.union Set.empty

        allPrograms =
            Dict.toList x |> List.map Tuple.first
    in
        case List.filter (\x -> Set.member x allSets |> not) allPrograms of
            head :: _ ->
                head

            _ ->
                "NOT FOUND"


getWeight : Dict String ( Set String, Int ) -> String -> Int
getWeight dict name =
    case Dict.get name dict of
        Just ( children, own ) ->
            own + Set.foldr (\c sum -> sum + getWeight dict c) 0 children

        Nothing ->
            0



-- This just returns all the ones that are imbalanced.
-- Using this info, you can use the lightest set, figured out which node is off
-- by how much, look up its weight, and subtract.
-- I did this manually to do it faster.


pt2 : Dict String ( Set String, Int ) -> List (List ( String, Int ))
pt2 x =
    let
        allProgs =
            Dict.toList x |> List.map Tuple.first
    in
        List.filter
            (\p ->
                case Dict.get p x of
                    Just ( children, _ ) ->
                        Set.size (Set.map (getWeight x) children) > 1

                    Nothing ->
                        False
            )
            allProgs
            |> List.map
                (\z ->
                    case Dict.get z x of
                        Nothing ->
                            []

                        Just ( c, _ ) ->
                            List.map (\zz -> ( zz, getWeight x zz )) (Set.toList c)
                )


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
