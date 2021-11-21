module Bezier exposing
    ( BezierEasingFunc
    , BezierPointFunc
    , bezierBinEpsilon
    , bezierBinFixed
    , bezierPoint
    , bezierPointAdvancedOptimized
    , bezierPointAdvancedOriginal
    , bezierPointSimple
    )

-- EASINGS


type alias BezierEasingFunc =
    Float -> Float -> Float -> Float -> Float -> Float


{-| Binary search with a fixed number of steps
-}
bezierBinFixed : BezierEasingFunc
bezierBinFixed x1 y1 x2 y2 =
    let
        precision =
            8
    in
    \time ->
        if time == 0 then
            0

        else if time == 1 then
            1

        else
            List.range 0 precision
                |> List.foldl
                    (\_ q ->
                        let
                            ( minX, maxX ) =
                                q.range

                            pivot =
                                (minX + maxX) / 2

                            ( x, y ) =
                                bezierPointSimple x1 y1 x2 y2 pivot
                        in
                        if x == time then
                            { q | value = y }

                        else
                            let
                                newRange =
                                    if x < time then
                                        ( pivot, maxX )

                                    else
                                        ( minX, pivot )
                            in
                            { q | range = newRange, value = y }
                    )
                    { range = ( 0, 1 ), value = 0 }
                |> (\c -> c.value)


{-| Binary search with a fixed precision
-}
bezierBinEpsilon : BezierEasingFunc
bezierBinEpsilon x1 y1 x2 y2 =
    let
        func =
            bezierPointSimple x1 y1 x2 y2
    in
    \time ->
        if time == 0 then
            0

        else if time == 1 then
            1

        else
            bezierBinEpsilonHelper func time ( 0, 1 )


epsilon =
    0.0005


bezierBinEpsilonHelper f t ( tMin, tMax ) =
    let
        tMid =
            (tMin + tMax) / 2

        ( x, y ) =
            f tMid
    in
    if abs (t - x) < epsilon then
        y

    else
        let
            newRange =
                if x < t then
                    ( tMid, tMax )

                else
                    ( tMin, tMid )
        in
        bezierBinEpsilonHelper f t newRange



-- POINTS


type alias BezierPointFunc =
    Float -> Float -> Float -> Float -> Float -> ( Float, Float )


bezierPoint : BezierPointFunc
bezierPoint =
    bezierPointSimple


{-| Naive approach, reverse-engineered from the gifs at
<https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Higher-order_curves>
-}
bezierPointSimple : BezierPointFunc
bezierPointSimple x1 y1 x2 y2 time =
    let
        q0 =
            interpolate2d ( 0, 0 ) ( x1, y1 ) time

        q1 =
            interpolate2d ( x1, y1 ) ( x2, y2 ) time

        q2 =
            interpolate2d ( x2, y2 ) ( 1, 1 ) time

        r0 =
            interpolate2d q0 q1 time

        r1 =
            interpolate2d q1 q2 time

        b =
            interpolate2d r0 r1 time
    in
    b


{-| Return a point on line segment (a, b) for given t between (0,1)
-}
interpolate2d : ( Float, Float ) -> ( Float, Float ) -> Float -> ( Float, Float )
interpolate2d ( xa, ya ) ( xb, yb ) t =
    ( xa + t * (xb - xa)
    , ya + t * (yb - ya)
    )


{-| This is Easing.bezier from elm-community/timing-functions, modified to return (x, y) instead of y
-}
bezierPointAdvancedOriginal : BezierPointFunc
bezierPointAdvancedOriginal x1 y1 x2 y2 time =
    let
        lerp_ from to v =
            from + (to - from) * v

        pair_ interpolate ( a0, b0 ) ( a1, b1 ) v =
            ( interpolate a0 a1 v, interpolate b0 b1 v )

        casteljau_ ps =
            case ps of
                [ ( x, y ) ] ->
                    ( x, y )

                xs ->
                    List.map2 (\x y -> pair_ lerp_ x y time) xs (Maybe.withDefault [] (List.tail xs))
                        |> casteljau_
    in
    casteljau_ [ ( 0, 0 ), ( x1, y1 ), ( x2, y2 ), ( 1, 1 ) ]


{-| Easing.bezier again, this time without local functions
-}
bezierPointAdvancedOptimized : BezierPointFunc
bezierPointAdvancedOptimized x1 y1 x2 y2 =
    casteljau [ ( 0, 0 ), ( x1, y1 ), ( x2, y2 ), ( 1, 1 ) ]


lerp from to v =
    from + (to - from) * v


pair interpolate ( a0, b0 ) ( a1, b1 ) v =
    ( interpolate a0 a1 v, interpolate b0 b1 v )


casteljau ps t =
    case ps of
        [ ( x, y ) ] ->
            ( x, y )

        _ ->
            casteljau
                (List.map2 (\x y -> pair lerp x y t) ps (Maybe.withDefault [] (List.tail ps)))
                t
