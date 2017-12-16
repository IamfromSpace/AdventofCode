module Main exposing (..)

import Html exposing (program)
import Html.Attributes as A
import Dict exposing (Dict)
import Set exposing (Set)
import Input


parse : String -> Dict String (Set String)
parse =
    String.split "\n"
        >> List.foldr
            (\n p ->
                case String.split " <-> " n of
                    [ l, r ] ->
                        Dict.insert l (String.split ", " r |> Set.fromList) p

                    _ ->
                        p
            )
            Dict.empty


type alias State =
    { idToGroup : Dict String Int
    , groupToIds : Dict Int (Set String)
    , nextGroupId : Int
    }


mempty : State
mempty =
    { idToGroup = Dict.empty
    , groupToIds = Dict.empty
    , nextGroupId = 0
    }


joinGroup : Int -> Int -> State -> State
joinGroup from to state =
    { state
        | idToGroup =
            case Dict.get from state.groupToIds of
                Just s ->
                    Set.foldr (flip Dict.insert to) state.idToGroup s

                Nothing ->
                    state.idToGroup
        , groupToIds =
            case Dict.get from state.groupToIds of
                Just s1 ->
                    let
                        removed =
                            Dict.remove from state.groupToIds
                    in
                        case Dict.get to state.groupToIds of
                            Just s2 ->
                                Dict.insert to (Set.union s1 s2) removed

                            Nothing ->
                                Dict.insert to s1 removed

                Nothing ->
                    state.groupToIds
    }


addKeyToGroup : Int -> String -> State -> State
addKeyToGroup group id state =
    case Dict.get id state.idToGroup of
        Just g ->
            joinGroup g group state

        Nothing ->
            { state
                | idToGroup = Dict.insert id group state.idToGroup
                , groupToIds =
                    case Dict.get group state.groupToIds of
                        Just s ->
                            Dict.insert group (Set.insert id s) state.groupToIds

                        Nothing ->
                            Dict.insert group (Set.singleton id) state.groupToIds
            }


toGroups : String -> Set String -> State -> State
toGroups id s state =
    let
        ( group, withNewGroup ) =
            case Dict.get id state.idToGroup of
                Just existingGroup ->
                    ( existingGroup, state )

                Nothing ->
                    ( state.nextGroupId
                    , addKeyToGroup
                        state.nextGroupId
                        id
                        { state | nextGroupId = state.nextGroupId + 1 }
                    )
    in
        Set.foldr (addKeyToGroup group) withNewGroup s


pt1 : Dict String (Set String) -> Int
pt1 s =
    let
        state =
            Dict.foldr toGroups mempty s
    in
        Dict.get "0" state.idToGroup
            |> Maybe.andThen (flip Dict.get state.groupToIds)
            |> Maybe.map Set.size
            |> Maybe.withDefault -1


pt2 : Dict String (Set String) -> Int
pt2 =
    Dict.foldr toGroups mempty >> .groupToIds >> Dict.size


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
                        , Html.pre [] [ Html.text <| (++) "ex (6): " <| toString <| pt1 <| parse Input.ex ]
                        , Html.pre [] [ Html.text <| (++) "Pt2: " <| toString <| pt2 p ]
                        , Html.pre [] [ Html.text <| (++) "ex (6): " <| toString <| pt2 <| parse Input.ex ]
                        , Html.pre [] [ Html.text <| (++) "parsed: " <| toString <| p ]
                        ]
        , subscriptions = \_ -> Sub.none
        }
