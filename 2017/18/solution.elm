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
    = Snd Pointer
    | Op String (Int -> Int -> Int) String Pointer
    | Rec String
    | Jump Pointer Pointer


type alias Parsed =
    Dict Int Inst


parse : String -> Parsed
parse =
    String.split "\n"
        >> List.map
            (\z ->
                case String.split " " z of
                    [ "snd", x ] ->
                        Snd <|
                            case parseInt x of
                                Just n ->
                                    Val n

                                Nothing ->
                                    Reg x

                    [ "set", x, y ] ->
                        Op "set" (always identity) x <|
                            case parseInt y of
                                Just n ->
                                    Val n

                                Nothing ->
                                    Reg y

                    [ "add", x, y ] ->
                        Op "add" (+) x <|
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

                    [ "mod", x, y ] ->
                        Op "mod" (%) x <|
                            case parseInt y of
                                Just n ->
                                    Val n

                                Nothing ->
                                    Reg y

                    [ "rcv", x ] ->
                        Rec x

                    [ "jgz", x, y ] ->
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
                        Debug.log ("parse error b!" ++ z) <| Snd (Reg "error")
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


adv1 : Inst -> State -> State
adv1 inst s =
    let
        state =
            { s | ic = s.ic + 1 }
    in
        case inst of
            Snd pointer ->
                let
                    val =
                        case pointer of
                            Val v ->
                                v

                            Reg r ->
                                Dict.get r state.regs |> Maybe.withDefault 0
                in
                    { state | lastSound = Just val }

            Op _ op reg pointer ->
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
                    { state | regs = Dict.insert reg (op regValue instValue) state.regs }

            Rec reg ->
                let
                    regVal =
                        Dict.get reg state.regs |> Maybe.withDefault 0
                in
                    if regVal == 0 then
                        state
                    else
                        { state | recovered = state.lastSound }

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
                    if test > 0 then
                        { state | ic = s.ic + offset }
                    else
                        state


pt1 : Parsed -> Maybe Int
pt1 insts =
    let
        go acc =
            case Dict.get acc.ic insts of
                Just inst ->
                    let
                        nextAcc =
                            adv1 inst acc
                    in
                        case nextAcc.recovered of
                            Just x ->
                                Just x

                            Nothing ->
                                go nextAcc

                Nothing ->
                    Nothing
    in
        go (makeProg 0)


makeProg : Int -> State
makeProg id =
    { ic = 0
    , lastSound = Nothing
    , regs = Dict.empty |> Dict.insert "p" id
    , recovered = Nothing
    , waiting = False
    , id = id
    }


adv2 : ( Inst, List Int, List Int ) -> State -> ( State, List Int, List Int )
adv2 ( inst, readBuf, writeBuf ) s =
    let
        state =
            { s | ic = s.ic + 1 }
    in
        case inst of
            Snd pointer ->
                let
                    instValue =
                        case pointer of
                            Val v ->
                                v

                            Reg r ->
                                Dict.get r state.regs |> Maybe.withDefault 0
                in
                    ( state
                    , readBuf
                    , writeBuf ++ [ instValue ]
                    )

            Op _ op reg pointer ->
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
                    ( { state | regs = Dict.insert reg (op regValue instValue) state.regs }
                    , readBuf
                    , writeBuf
                    )

            Rec reg ->
                case readBuf of
                    head :: tail ->
                        ( { state | waiting = False, regs = Dict.insert reg head state.regs }
                        , tail
                        , writeBuf
                        )

                    [] ->
                        --Don't advance the counter
                        ( { state | waiting = True, ic = s.ic }, readBuf, writeBuf )

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
                    ( if test > 0 then
                        { state | ic = s.ic + offset }
                      else
                        state
                    , readBuf
                    , writeBuf
                    )



--9217 is too high


pt2 : Parsed -> Maybe Int
pt2 insts =
    let
        go p0 p1 bFor0 bFor1 sendCount its =
            if its > 1000000 then
                Debug.log "too many iterations!" Nothing
            else if p0.waiting && p1.waiting && List.length bFor0 == 0 && List.length bFor1 == 0 then
                Just sendCount
            else if not p1.waiting || List.length bFor1 > 0 then
                case Dict.get p1.ic insts of
                    Just inst ->
                        let
                            ( p1Next, bFor1Next, bFor0Next ) =
                                adv2 ( inst, bFor1, bFor0 ) p1

                            sendCountNext =
                                if List.length bFor0 == List.length bFor0Next then
                                    sendCount
                                else
                                    Debug.log "count" <| sendCount + 1
                        in
                            go p0 p1Next bFor0Next bFor1Next sendCountNext (its + 1)

                    Nothing ->
                        Debug.log "couldn't get p1.ic" Nothing
            else
                case Dict.get p0.ic insts of
                    Just inst ->
                        let
                            ( p0Next, bFor0Next, bFor1Next ) =
                                adv2 ( inst, bFor0, bFor1 ) p0
                        in
                            go p0Next p1 bFor0Next bFor1Next sendCount (its + 1)

                    Nothing ->
                        Debug.log "couldn't get p0.ic" Nothing
    in
        go (makeProg 0) (makeProg 1) [] [] 0 0


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
                        [ Html.pre [] [ Html.text <| (++) "Pt1 test: " <| toString <| pt1 <| parse Input.test1 ]
                        , Html.pre [] [ Html.text <| (++) "Pt1: " <| toString <| pt1 p ]
                        , Html.pre [] [ Html.text <| (++) "Pt2 test1: " <| toString <| pt2 <| parse Input.test1 ]
                        , Html.pre [] [ Html.text <| (++) "Pt2 test: " <| toString <| pt2 <| parse Input.test2 ]
                        , Html.pre [] [ Html.text <| (++) "Pt2: " <| toString <| pt2 p ]
                        , Html.pre [] [ Html.text <| (++) "parsed: " <| toString <| p ]
                        ]
        , subscriptions = \_ -> Sub.none
        }
