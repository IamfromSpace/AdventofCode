module Main exposing (..)

--import Char
--import Bitwise
--import Set exposing (Set)

import Dict exposing (Dict)
import Html exposing (program)
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


type Pointer
    = Val Int
    | Reg String


type Inst
    = Op String (Int -> Int -> Int) String Pointer
    | Jump Pointer Pointer


type alias Parsed =
    Dict Int Inst


parse : String -> Parsed
parse =
    String.split "\n"
        >> List.map
            (\z ->
                case String.split " " z of
                    [ "set", x, y ] ->
                        Op "set" (always identity) x <|
                            case parseInt y of
                                Just n ->
                                    Val n

                                Nothing ->
                                    Reg y

                    [ "sub", x, y ] ->
                        Op "sub" (-) x <|
                            case parseInt y of
                                Just n ->
                                    Val n

                                Nothing ->
                                    Reg y

                    [ "mul", x, y ] ->
                        Op "mul" (*) x <|
                            case parseInt y of
                                Just n ->
                                    Val n

                                Nothing ->
                                    Reg y

                    [ "jnz", x, y ] ->
                        Jump
                            (case parseInt x of
                                Just n ->
                                    Val n

                                Nothing ->
                                    Reg x
                            )
                            (case parseInt y of
                                Just n ->
                                    Val n

                                Nothing ->
                                    Reg y
                            )

                    _ ->
                        Debug.log ("parse error b!" ++ z) <| Op "error" (+) z (Reg "error")
             --TODO
            )
        >> List.indexedMap (,)
        >> Dict.fromList


type alias State =
    { ic : Int
    , lastSound : Maybe Int
    , regs : Dict String Int
    , recovered : Maybe Int
    , waiting : Bool
    , id : Int
    }


adv1 : Inst -> State -> ( State, Bool )
adv1 inst s =
    let
        state =
            { s | ic = s.ic + 1 }

        {-
           log2 =
               Debug.log "state regs, pointer" ( s.regs, s.ic )

           log =
               Debug.log "state inst" <| Tuple.first ( inst, log2 )
        -}
        log =
            if s.ic == 28 then
                Debug.log "g" <| Dict.get "g" s.regs
            else
                Dict.get "g" s.regs
    in
        case inst of
            Op name op reg pointer ->
                let
                    regValue =
                        Dict.get reg state.regs |> Maybe.withDefault 0

                    instValue =
                        case pointer of
                            Val v ->
                                v

                            Reg r ->
                                Dict.get r state.regs |> Maybe.withDefault 0
                in
                    ( { state | regs = Dict.insert reg (op regValue instValue) state.regs }, name == "mul" )

            Jump testPointer offsetPointer ->
                let
                    test =
                        case testPointer of
                            Val v ->
                                v

                            Reg r ->
                                Dict.get r state.regs |> Maybe.withDefault 0

                    offset =
                        case offsetPointer of
                            Val v ->
                                v

                            Reg r ->
                                Dict.get r state.regs |> Maybe.withDefault 0
                in
                    ( if test /= 0 then
                        { state | ic = s.ic + offset }
                      else
                        state
                    , False
                    )


pt1 : Parsed -> Maybe Int
pt1 insts =
    let
        go acc count its =
            if its > 1000000000 then
                Nothing
            else
                case Dict.get acc.ic insts of
                    Just inst ->
                        let
                            ( nextAcc, wasMul ) =
                                adv1 inst acc

                            nextCount =
                                if wasMul then
                                    count + 1
                                else
                                    count
                        in
                            go nextAcc nextCount (its + 1)

                    Nothing ->
                        Just count
    in
        go (makeProg 0) 0 0


makeProg : Int -> State
makeProg id =
    { ic = 0
    , lastSound = Nothing
    , regs = Dict.empty |> Dict.insert "p" id
    , recovered = Nothing
    , waiting = False
    , id = id
    }


notPrime : Int -> Bool
notPrime i =
    List.any (\x -> i % x == 0) (List.range 2 (ceiling (sqrt (toFloat i))))


pt2 : Int
pt2 =
    let
        go i count =
            if i > 125400 then
                count
            else
                go (i + 17)
                    (if notPrime i then
                        count + 1
                     else
                        count
                    )
    in
        go 108400 0



--9217 is too high


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
                        , Html.pre [] [ Html.text <| (++) "Pt2: " <| toString <| pt2 ]
                        , Html.pre [] [ Html.text <| (++) "7: " <| toString <| notPrime 7 ]
                        , Html.pre [] [ Html.text <| (++) "2: " <| toString <| notPrime 2 ]
                        , Html.pre [] [ Html.text <| (++) "15: " <| toString <| notPrime 15 ]
                        , Html.pre [] [ Html.text <| (++) "17: " <| toString <| notPrime 17 ]
                        , Html.pre [] [ Html.text <| (++) "parsed: " <| toString <| p ]
                        ]
        , subscriptions = \_ -> Sub.none
        }
