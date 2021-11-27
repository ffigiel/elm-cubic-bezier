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
        [ BA.rank "point (cached)"
            (\f -> List.map f times)
            [ ( "bezierPointSimple", Bezier.bezierPointSimple 0.1 1 0.5 0.2 )
            , ( "bezierPointAdvancedOriginal", Bezier.bezierPointAdvancedOriginal 0.1 1 0.5 0.2 )
            , ( "bezierPointAdvancedOptimized", Bezier.bezierPointAdvancedOptimized 0.1 1 0.5 0.2 )
            ]
        , BA.rank "point (cold)"
            (\f ->
                List.map (\t -> f 0.1 1 0.5 0.2 t) times
            )
            [ ( "bezierPointSimple", Bezier.bezierPointSimple )
            , ( "bezierPointAdvancedOriginal", Bezier.bezierPointAdvancedOriginal )
            , ( "bezierPointAdvancedOptimized", Bezier.bezierPointAdvancedOptimized )
            ]
        , BA.rank "easing (cached)"
            (\f -> List.map f times)
            easingBenchmarks
        , BA.rank "easing (cold)"
            (\f ->
                List.map (\t -> f 0.1 1 0.5 0.2 t) times
            )
            [ ( "bezierBinFixed", Bezier.bezierBinFixed )
            , ( "bezierBinEpsilon", Bezier.bezierBinEpsilon )
            , ( "bezierBinHybrid", Bezier.bezierBinHybrid )
            ]
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
                    acc =
                        measureAccuracy f

                    accPercent =
                        toFloat (round (acc * 10000)) / 100

                    label =
                        funcName ++ " (" ++ String.fromFloat accPercent ++ "% accuracy)"
                in
                ( label, f )
            )


measureAccuracy : (Float -> Float) -> Float
measureAccuracy f =
    let
        times =
            List.range 0 10000
                |> List.map (\i -> toFloat i / 10000)

        scores =
            times
                |> List.map
                    (\t ->
                        let
                            ( x, yExpected ) =
                                Bezier.bezierPoint 0.1 1 0.5 0.2 t

                            yActual =
                                f x

                            diff =
                                abs (yExpected - yActual)

                            range =
                                max diff (abs yExpected)
                        in
                        if diff == 0 then
                            1

                        else
                            1 - (diff / range)
                    )

        sum =
            List.foldl (+) 0 scores

        mean =
            sum / (toFloat <| List.length scores)
    in
    mean
