module Main exposing (..)

import Dict exposing (Dict)


input : String
input =
    ""


solution1 : String -> Int
solution1 s =
    0


genMap : Int -> Int
genMap max =
    let
        go : Dict ( Int, Int ) Int -> Int -> Int
        go acc i =
            let
                innerWidth =
                    let
                        base =
                            floor (sqrt (toFloat i))
                    in
                        if (base // 2 * 2) == base then
                            base - 1
                        else
                            base

                circlePos =
                    Debug.log "circlePos" <| i - innerWidth ^ 2

                outerWidth =
                    Debug.log "outerWidth" <| innerWidth + 2

                x =
                    case circlePos // (outerWidth - 1) of
                        0 ->
                            (outerWidth - 1) // 2

                        1 ->
                            (outerWidth - 1) // 2 - (circlePos - (outerWidth - 2))

                        2 ->
                            -(outerWidth - 1) // 2

                        _ ->
                            (circlePos - 3 * (outerWidth - 1)) - (outerWidth - 1) // 2 + 1

                y =
                    case circlePos // (outerWidth - 1) of
                        0 ->
                            Debug.log "A" ((outerWidth - 1) // 2 - 1) - (circlePos)

                        1 ->
                            Debug.log "B" -(outerWidth - 1) // 2

                        2 ->
                            Debug.log "C" (circlePos - 2 * (outerWidth - 1)) - ((outerWidth - 1) // 2 - 1)

                        _ ->
                            Debug.log "D" (outerWidth - 1) // 2

                value =
                    (Dict.get ( x + 1, y + 1 ) acc |> Maybe.withDefault 0)
                        + (Dict.get ( x + 1, y ) acc |> Maybe.withDefault 0)
                        + (Dict.get ( x + 1, y - 1 ) acc |> Maybe.withDefault 0)
                        + (Dict.get ( x, y - 1 ) acc |> Maybe.withDefault 0)
                        + (Dict.get ( x, y + 1 ) acc |> Maybe.withDefault 0)
                        + (Dict.get ( x - 1, y + 1 ) acc |> Maybe.withDefault 0)
                        + (Dict.get ( x - 1, y ) acc |> Maybe.withDefault 0)
                        + (Dict.get ( x - 1, y - 1 ) acc |> Maybe.withDefault 0)
            in
                if Tuple.second (Debug.log "value" ( ( i, x, y ), value )) > max || i > 500 then
                    value
                else
                    go (Dict.insert ( x, y ) value acc) (i + 1)
    in
        go (Dict.insert ( 0, 0 ) 1 Dict.empty) 1


solution2 : String -> Int
solution2 s =
    0


output : ( Int, Int )
output =
    Debug.log "solution" ( solution1 input, genMap 289326 )
