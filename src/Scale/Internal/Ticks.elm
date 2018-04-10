module Scale.Internal.Ticks exposing (ticks, tickStep, tickIncrement)

{-| Nice set of approximately `n` numbers between `start` and `stop`
    Implementation based on https://github.com/d3/d3-array/blob/master/src/ticks.js
-}


ticks : ( Float, Float ) -> Int -> List Float
ticks ( start, stop ) count =
    let
        ( reverse, startVal, stopVal ) =
            if stop < start then
                ( List.reverse, stop, start )
            else
                ( identity, start, stop )

        step =
            tickIncrement ( startVal, stopVal ) count
    in
        if startVal == stopVal && count > 0 then
            [ startVal ]
        else if step == 0.0 || isInfinite step then
            []
        else if step > 0.0 then
            let
                newStart =
                    ceiling (startVal / step)

                newStop =
                    floor (stopVal / step)

                n =
                    newStop - newStart + 1
            in
                reverse <|
                    List.map (\i -> (toFloat <| newStart + i) * step) <|
                        List.range 0 (n - 1)
        else
            let
                newStart =
                    floor (startVal * step)

                newStop =
                    ceiling (stopVal * step)

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

        delta =
            abs <| stop - start

        step0 =
            delta / (max 0.0 n)

        step1 =
            10.0 ^ (log10 step0)

        error =
            step0 / step1

        f =
            if stop < start then
                \x -> -x
            else
                identity
    in
        if error >= e10 then
            f <| step1 * 10
        else if error >= e5 then
            f <| step1 * 5
        else if error >= e2 then
            f <| step1 * 2
        else
            f step1


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
