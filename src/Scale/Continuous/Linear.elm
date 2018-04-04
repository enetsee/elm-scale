module Scale.Continuous.Linear exposing (Linear, linear, niceDomain)

{-|
@docs Linear, linear, niceDomain
-}

import Scale.Continuous exposing (continuous, Continuous)
import Scale.Config exposing (OutsideDomain)
import Scale.Internal.Ticks as Ticks exposing (tickIncrement)
import Scale.Internal.Deinterpolate exposing (deinterpolateLinear)
import Scale.Internal.Reinterpolate exposing (reinterpolateLinear)


{-| Linear scale
-}
type alias Linear a =
    { a
        | domain : ( Float, Float )
        , range : ( Float, Float )
        , outsideDomain : OutsideDomain
    }


{-| -}
linear :
    Linear a
    -> Continuous Float Float
linear { domain, range, outsideDomain } =
    continuous
        { domain = domain
        , range = range
        , compareDomain = compare
        , deinterpolateDomain = deinterpolateLinear
        , interpolateRange = reinterpolateLinear
        , reinterpolateDomain = reinterpolateLinear
        , outsideDomain = outsideDomain
        , ticks = Ticks.ticks
        }


{-| -}
ticks : Linear a -> Int -> List Float
ticks { domain } count =
    Ticks.ticks domain count


{-| -}
niceDomain : Int -> Linear a -> Linear a
niceDomain count ({ domain } as scale) =
    let
        ( start, stop ) =
            domain

        ( start2, stop2 ) =
            if stop < start then
                ( stop, start )
            else
                ( start, stop )

        step =
            tickIncrement ( start2, stop2 ) count
    in
        if step == 0.0 then
            scale
        else if step > 0 then
            let
                newStart =
                    (toFloat <| floor (start2 / step)) * step

                newStop =
                    (toFloat <| ceiling (stop2 / step)) * step

                step2 =
                    tickIncrement ( newStart, newStop ) count
            in
                { scale
                    | domain =
                        ( (toFloat <| floor (newStart / step2)) * step2
                        , (toFloat <| ceiling (newStop / step2)) * step2
                        )
                }
        else
            let
                newStart =
                    (toFloat <| ceiling (start * step)) / step

                newStop =
                    (toFloat <| floor (stop * step)) / step

                step2 =
                    tickIncrement ( newStart, newStop ) count
            in
                { scale
                    | domain =
                        ( (toFloat <| ceiling (newStart * step2)) / step2
                        , (toFloat <| floor (newStop * step2)) / step2
                        )
                }
