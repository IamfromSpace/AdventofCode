module Main exposing (..)

--import Char
--import Bitwise

import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (Html, program)
import Html.Attributes as A
import ParseInt
import Input


parseInt : String -> Maybe Int
parseInt s =
    let
        parse : String -> Maybe Int
        parse =
            ParseInt.parseInt >> Result.toMaybe
    in
        case String.toList s of
            head :: tail ->
                if head == '-' then
                    (Maybe.map ((*) -1) <| parse <| String.fromList tail)
                else
                    parse s

            [] ->
                Nothing


parse1 : String -> Set ( Int, Int )
parse1 =
    String.split "\n"
        >> List.indexedMap (,)
        >> List.foldr
            (\( y, str ) set ->
                List.foldr
                    (\( x, c ) set ->
                        if c == '#' then
                            Set.insert ( x, y ) set
                        else
                            set
                    )
                    set
                    (String.toList str |> List.indexedMap (,))
            )
            Set.empty


parse2 : String -> Dict ( Int, Int ) Int
parse2 =
    String.split "\n"
        >> List.indexedMap (,)
        >> List.foldr
            (\( y, str ) dict ->
                List.foldr
                    (\( x, c ) dict ->
                        if c == '#' then
                            Dict.insert ( x, y ) 2 dict
                        else
                            dict
                    )
                    dict
                    (String.toList str |> List.indexedMap (,))
            )
            Dict.empty


type alias State1 =
    { grid : Set ( Int, Int )
    , carrierPos : ( Int, Int )
    , carrierDir : Int
    }


step1 : State1 -> ( State1, Bool )
step1 { grid, carrierPos, carrierDir } =
    let
        ( x, y ) =
            carrierPos

        didNotInfect =
            Set.member carrierPos grid

        ( nextGrid, nextDir ) =
            if didNotInfect then
                ( Set.remove carrierPos grid, (carrierDir - 1) % 4 )
            else
                ( Set.insert carrierPos grid, (carrierDir + 1) % 4 )
    in
        ( { grid = nextGrid
          , carrierDir = nextDir
          , carrierPos =
                case nextDir of
                    0 ->
                        ( x + 1, y )

                    1 ->
                        ( x, y - 1 )

                    2 ->
                        ( x - 1, y )

                    _ ->
                        ( x, y + 1 )
          }
        , not didNotInfect
        )


pt1 : Int -> Int -> Set ( Int, Int ) -> Int
pt1 burstCount center grid =
    let
        init =
            State1 grid ( center, center ) 1

        go i n acc =
            if i >= burstCount then
                n
            else
                let
                    ( nextAcc, didInfect ) =
                        step1 acc

                    nextN =
                        if didInfect then
                            n + 1
                        else
                            n
                in
                    go (i + 1) nextN nextAcc
    in
        go 0 0 init


type alias State2 =
    { grid : Dict ( Int, Int ) Int
    , carrierPos : ( Int, Int )
    , carrierDir : Int
    }


step2 : State2 -> ( State2, Bool )
step2 { grid, carrierPos, carrierDir } =
    let
        ( x, y ) =
            carrierPos

        nodeState =
            Dict.get carrierPos grid |> Maybe.withDefault 0

        ( nextGrid, nextDir, didInfect ) =
            case nodeState of
                0 ->
                    ( Dict.insert carrierPos 1 grid, (carrierDir + 1) % 4, False )

                1 ->
                    ( Dict.insert carrierPos 2 grid, carrierDir, True )

                2 ->
                    ( Dict.insert carrierPos 3 grid, (carrierDir - 1) % 4, False )

                _ ->
                    ( Dict.insert carrierPos 0 grid, (carrierDir + 2) % 4, False )
    in
        ( { grid = nextGrid
          , carrierDir = nextDir
          , carrierPos =
                case nextDir of
                    0 ->
                        ( x + 1, y )

                    1 ->
                        ( x, y - 1 )

                    2 ->
                        ( x - 1, y )

                    _ ->
                        ( x, y + 1 )
          }
        , didInfect
        )


pt2 : Int -> Int -> Dict ( Int, Int ) Int -> Int
pt2 burstCount center grid =
    let
        init =
            State2 grid ( center, center ) 1

        go i n acc =
            if i >= burstCount then
                n
            else
                let
                    ( nextAcc, didInfect ) =
                        step2 acc

                    nextN =
                        if didInfect then
                            n + 1
                        else
                            n
                in
                    go (i + 1) nextN nextAcc
    in
        go 0 0 init


pre : String -> a -> Html b
pre label value =
    Html.pre [] [ Html.text <| (++) (label ++ ": ") <| toString value ]


main : Program Never () a
main =
    program
        { init = ( (), Cmd.none )
        , update = \_ _ -> ( (), Cmd.none )
        , view =
            \_ ->
                Html.div [ A.style [ ( "padding", "5px" ) ] ]
                    [ pre "Pt1" <| pt1 10000 12 <| parse1 Input.input
                    , pre "Pt1 test" <| pt1 10000 1 <| parse1 Input.test1
                    , pre "Pt2" <| pt2 10000000 12 <| parse2 Input.input
                    , pre "Pt2 test" <| pt2 100 1 <| parse2 Input.test1
                    , pre "parsed" <| parse1 Input.input
                    , pre "parsed test" <| parse1 Input.test1
                    ]
        , subscriptions = \_ -> Sub.none
        }
