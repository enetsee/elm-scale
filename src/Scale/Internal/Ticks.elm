module Scale.Internal.Ticks exposing (ticks, tickStep, tickIncrement)

{-| Nice set of approximately `n` numbers between `start` and `stop`
    Implementation based on https://github.com/d3/d3-array/blob/master/src/ticks.js
-}


ticks : ( Float, Float ) -> Int -> List Float
ticks ( start, stop ) count =
    let
        ( reverse, start2, stop2 ) =
            if stop < start then
                ( List.reverse, stop, start )
            else
                ( identity, start, stop )

        step =
            tickIncrement ( start2, stop2 ) count
    in
        if step == 0.0 || isInfinite step then
            []
        else if step > 0.0 then
            let
                newStart =
                    ceiling (start2 / stop2)

                newStop =
                    floor (stop2 / step)

                n =
                    newStop - newStart + 1
            in
                reverse <|
                    List.map (\i -> (toFloat <| newStart + i) * step) <|
                        List.range 0 (n - 1)
        else
            let
                newStart =
                    floor (start2 * step)

                newStop =
                    ceiling (stop2 * step)

                n =
                    newStart - newStop + 1
            in
                reverse <|
                    List.map (\i -> (toFloat <| newStart - i) / step) <|
                        List.range 0 (n - 1)


tickStep : ( Float, Float ) -> Int -> Float
tickStep ( start, stop ) count =
    let
        n =
            toFloat count

        step =
            (stop - start) / max 0 n

        power =
            toFloat <| floor <| log10 step

        error =
            step / (10 ^ power)

        f =
            if stop < start then
                \x -> -x
            else
                identity
    in
        if error >= e10 then
            f <| step * 10
        else if error >= e5 then
            f <| step * 5
        else if error >= e2 then
            f <| step * 2
        else
            f step


tickIncrement : ( Float, Float ) -> Int -> Float
tickIncrement ( start, stop ) count =
    let
        n =
            toFloat count

        step =
            (stop - start) / max 0 n

        power =
            toFloat <| floor <| log10 step

        error =
            step / (10 ^ power)

        m =
            if error >= e10 then
                10
            else if error >= e5 then
                5
            else if error >= e2 then
                2
            else
                1
    in
        if power >= 0 then
            m * (10 ^ power)
        else
            (-(10 ^ -power)) / m


ln10 : Float
ln10 =
    logBase e 10.0


log10 : Float -> Float
log10 =
    logBase 10.0


e10 : Float
e10 =
    sqrt 50.0


e5 : Float
e5 =
    sqrt 10.0


e2 : Float
e2 =
    sqrt 2.0
