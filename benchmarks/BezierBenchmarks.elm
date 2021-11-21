module BezierBenchmarks exposing (main)

import Benchmark as B exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Bezier


main : BenchmarkProgram
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
        [ B.describe "point"
            [ B.benchmark "bezierPointSimple" <|
                \_ -> List.map (Bezier.bezierPointSimple 0.1 1 0.5 0.2) times
            , B.benchmark "bezierPointAdvancedOriginal" <|
                \_ -> List.map (Bezier.bezierPointAdvancedOriginal 0.1 1 0.5 0.2) times
            , B.benchmark "bezierPointAdvancedOptimized" <|
                \_ -> List.map (Bezier.bezierPointAdvancedOptimized 0.1 1 0.5 0.2) times
            ]
        , B.describe "easing"
            (easingBenchmarks times)
        ]


easingBenchmarks : List Float -> List Benchmark
easingBenchmarks times =
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
                B.benchmark label
                    (\_ -> List.map f times)
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
