module BezierBenchmarks exposing (main)

import Benchmark as B exposing (Benchmark)
import Benchmark.Alternative as BA
import Benchmark.Runner.Alternative exposing (Program, program)
import Bezier


main : Program
main =
    program suite


suite : Benchmark
suite =
    let
        times =
            List.range 0 100
                |> List.map (\i -> toFloat i / 100)
    in
    B.describe "Bezier"
        [ BA.rank "point"
            (\f -> List.map f times)
            [ ( "bezierPointSimple", Bezier.bezierPointSimple 0.1 1 0.5 0.2 )
            , ( "bezierPointAdvancedOriginal", Bezier.bezierPointAdvancedOriginal 0.1 1 0.5 0.2 )
            , ( "bezierPointAdvancedOptimized", Bezier.bezierPointAdvancedOptimized 0.1 1 0.5 0.2 )
            ]
        , BA.rank "easing"
            (\f -> List.map f times)
            easingBenchmarks
        ]


easingBenchmarks : List ( String, Float -> Float )
easingBenchmarks =
    [ ( "bezierBinFixed", Bezier.bezierBinFixed 0.1 1 0.5 0.2 )
    , ( "bezierBinEpsilon", Bezier.bezierBinEpsilon 0.1 1 0.5 0.2 )
    , ( "bezierBinHybrid", Bezier.bezierBinHybrid 0.1 1 0.5 0.2 )
    ]
        |> List.map
            (\( funcName, f ) ->
                let
                    fit =
                        measureFit f

                    fitPercent =
                        toFloat (round (fit * 10000)) / 100

                    label =
                        funcName ++ " (" ++ String.fromFloat fitPercent ++ "% accuracy)"
                in
                ( label, f )
            )


measureFit : (Float -> Float) -> Float
measureFit f =
    let
        times =
            List.range 0 1000000
                |> List.map (\i -> toFloat i / 1000000)

        scores =
            times
                |> List.map
                    (\t ->
                        let
                            ( x, yExpected ) =
                                Bezier.bezierPoint 0.1 1 0.5 0.2 t

                            yActual =
                                f x
                        in
                        if yActual == yExpected then
                            1

                        else
                            1 - (abs (yExpected - yActual) / yExpected)
                    )

        sum =
            List.foldl (+) 0 scores

        mean =
            sum / (toFloat <| List.length scores)
    in
    mean
