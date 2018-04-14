module Scale.Continuous.Log exposing (Log, log, niceDomain)

{-|
@docs Log, log, niceDomain
-}

import Scale.Config exposing (OutsideDomain(Clamp))
import Scale.Continuous exposing (continuous, Continuous)
import Scale.Internal.Reinterpolate exposing (reinterpolateLinear)
import Scale.Internal.Ticks as Ticks


{-| -}
type alias Log a =
    { a
        | base : Float
        , domain : ( Float, Float )
        , range : ( Float, Float )
        , outsideDomain : OutsideDomain
    }


{-| -}
log : Log a -> Continuous Float Float
log { base, domain, range, outsideDomain } =
    continuous
        { domain = domain
        , range = range
        , compareDomain = compare
        , deinterpolateDomain = deinterpolateLog base
        , interpolateRange = reinterpolateLinear
        , reinterpolateDomain = reinterpolateLog
        , outsideDomain = outsideDomain
        , ticks = ticks base
        }


deinterpolateLog : Float -> ( Float, Float ) -> Float -> Float
deinterpolateLog base ( lower, upper ) =
    let
        delta =
            logBase base (upper / lower)
    in
        if delta /= 0.0 then
            \x -> logBase base (x / lower) / delta
        else
            always delta


reinterpolateLog : ( Float, Float ) -> Float -> Float
reinterpolateLog ( lower, upper ) =
    if lower < 0.0 then
        \t -> -(-upper ^ t * -lower ^ (1.0 - t))
    else
        \t -> (upper ^ t * lower ^ (1.0 - t))



-- Nice domain -----------------------------------------------------------------


{-|
-}
niceDomain : Log a -> Log a
niceDomain ({ base, domain } as scale) =
    let
        ( start, stop ) =
            domain

        f =
            (\a -> a ^ base) << toFloat << floor << logBase base

        c =
            (\a -> a ^ base) << toFloat << ceiling << logBase base
    in
        { scale | domain = ( f start, c stop ) }



-- Ticks -----------------------------------------------------------------------


{-| Reasonable tick intervals forlog domain numbers. Based on:
    https://github.com/d3/d3-scale/blob/master/src/log.js#L63
-}
ticks : Float -> ( Float, Float ) -> Int -> List Float
ticks base domain count =
    let
        ( start, end ) =
            domain

        i =
            logBase base start

        j =
            logBase base end

        n =
            toFloat count

        ticksHelper inc st i j k =
            let
                p =
                    i ^ base

                t =
                    p * k
            in
                if st k then
                    if t < start then
                        ticksHelper inc st i j (inc k)
                    else if t > end then
                        []
                    else
                        ticksHelper inc st i j (inc k) ++ [ t ]
                else
                    []
    in
        if not (toFloat (round base) == base) && j - i < n then
            if start > 0 then
                ticksHelper (\k -> k + 1) (\k -> k < base) (toFloat (round i - 1)) (toFloat (round j + 1)) 1
            else
                ticksHelper (\k -> k - 1) (\k -> k >= 1) (toFloat (round i - 1)) (toFloat (round j + 1)) (base - 1)
        else
            round (min (j - i) n)
                |> Ticks.ticks ( i, j )
                |> List.map (\a -> base ^ a)


tickStep : Float -> Float -> Int -> Float
tickStep start stop count =
    let
        step0 =
            abs (stop - start) / max 0 (toFloat count)

        step1 =
            toFloat (10 ^ floor (logBase e step0 / logBase e 10))

        error =
            step0 / step1

        step2 =
            if error >= sqrt 50 then
                step1 * 10
            else if error >= sqrt 10 then
                step1 * 5
            else if error >= sqrt 2 then
                step1 * 2
            else
                step1
    in
        if stop < start then
            -step2
        else
            step2


range : Float -> Float -> Float -> List Float
range begin end step =
    if (end - begin) > 0 && step > 0 then
        rangePositive begin end step
    else if (begin - end) > 0 && -step > 0 then
        rangeNegative begin end step
    else
        []


rangePositive : Float -> Float -> Float -> List Float
rangePositive begin stop step =
    let
        helper s list =
            if s == stop then
                list
            else if (s + step - stop) > 0 then
                s :: list
            else
                helper (s + step) (s :: list)
    in
        helper begin [] |> List.reverse


rangeNegative : Float -> Float -> Float -> List Float
rangeNegative begin stop step =
    let
        helper s list =
            if s == stop then
                list
            else if (stop - (s + step)) > 0 then
                s :: list
            else
                helper (s + step) (s :: list)
    in
        helper begin [] |> List.reverse
