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


type Comp
    = Eq
    | NotEq
    | LessThan
    | LessThanEq
    | GreaterThan
    | GreaterThanEq


type Cond
    = Cond String Int Comp


type Inst
    = Inst Int String Int Cond


strToComp : String -> Comp
strToComp s =
    case s of
        "==" ->
            Eq

        "<=" ->
            LessThanEq

        ">=" ->
            GreaterThanEq

        "<" ->
            LessThan

        ">" ->
            GreaterThan

        _ ->
            NotEq


strToCond : String -> Cond
strToCond s =
    case String.split " " s of
        [ x, y, z ] ->
            Cond x (parseInt z) (strToComp y)

        _ ->
            Cond "" 0 Eq


strToInst : String -> Inst
strToInst s =
    case String.split " if " s of
        [ l, r ] ->
            case String.split " " l of
                [ a, "dec", b ] ->
                    Inst -1 a (parseInt b) (strToCond r)

                [ a, "inc", b ] ->
                    Inst 1 a (parseInt b) (strToCond r)

                _ ->
                    Inst 1 "" 0 (Cond "" 0 Eq)

        _ ->
            Inst 1 "" 0 (Cond "" 0 Eq)


parse : String -> List Inst
parse =
    String.split "\n" >> List.map strToInst


compare : Comp -> Int -> Int -> Bool
compare c l r =
    case c of
        Eq ->
            l == r

        NotEq ->
            l /= r

        LessThan ->
            l < r

        GreaterThan ->
            l > r

        LessThanEq ->
            l <= r

        GreaterThanEq ->
            l >= r


registers : List Inst -> ( Dict String Int, Int )
registers zzz =
    List.foldl
        (\(Inst sign reg amount (Cond compReg compAmount op)) ( regs, max ) ->
            let
                nextRegs =
                    if
                        (case Dict.get compReg regs of
                            Just x ->
                                compare op x compAmount

                            Nothing ->
                                compare op 0 compAmount
                        )
                    then
                        Dict.update
                            reg
                            (\mv ->
                                case mv of
                                    Just y ->
                                        Just (y + sign * amount)

                                    Nothing ->
                                        Just (sign * amount)
                            )
                            regs
                    else
                        regs

                currentMax =
                    dictMax nextRegs
            in
                ( nextRegs
                , if currentMax > max then
                    currentMax
                  else
                    max
                )
        )
        ( Dict.empty, 0 )
        zzz


dictMax : Dict comparable number -> number
dictMax =
    Dict.foldr
        (\_ x max ->
            if x > max then
                x
            else
                max
        )
        0


pt1 : List Inst -> Int
pt1 =
    registers
        >> Tuple.first
        >> dictMax


pt2 : List Inst -> Int
pt2 =
    registers
        >> Tuple.second


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
