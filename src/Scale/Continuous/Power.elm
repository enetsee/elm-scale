module Scale.Continuous.Power exposing (Power, power, niceDomain)

{-|
@docs Power, power, niceDomain
-}

import Scale.Config exposing (OutsideDomain(Clamp))
import Scale.Continuous exposing (continuous, Continuous)
import Scale.Continuous.Linear as Linear
import Scale.Internal.Ticks as Ticks


{-| Power scale
-}
type alias Power a =
    { a
        | exponent : Float
        , domain : ( Float, Float )
        , range : ( Float, Float )
        , outsideDomain : OutsideDomain
    }


{-| -}
power : Power a -> Continuous Float Float
power { exponent, domain, range, outsideDomain } =
    continuous
        { domain = domain
        , range = range
        , compareDomain = compare
        , deinterpolateDomain = deinterpolatePower exponent
        , interpolateRange = reinterpolatePower exponent
        , reinterpolateDomain = reinterpolateLinear
        , outsideDomain = outsideDomain
        , ticks = Ticks.ticks
        }


reinterpolateLinear : ( number, number ) -> number -> number
reinterpolateLinear ( lower, upper ) =
    let
        delta =
            upper - lower
    in
        \t -> lower + delta * t


deinterpolatePower : Float -> ( Float, Float ) -> Float -> Float
deinterpolatePower exponent ( lower, upper ) =
    let
        lower_ =
            raise lower exponent

        upper_ =
            (raise upper exponent) - lower_
    in
        if upper_ /= 0.0 then
            \x -> ((raise x exponent) - lower_) / upper_
        else
            always 0.0


reinterpolatePower : Float -> ( Float, Float ) -> Float -> Float
reinterpolatePower exponent ( lower, upper ) =
    let
        lower_ =
            raise lower exponent

        upper_ =
            (raise upper exponent) - lower_
    in
        \t ->
            raise (lower_ + upper_ * t) (1.0 / exponent)



-- Nice domain -----------------------------------------------------------------


{-| -}
niceDomain : Int -> Power a -> Power a
niceDomain count scale =
    Linear.niceDomain count scale



-- Ticks -----------------------------------------------------------------------
-- Helpers ---------------------------------------------------------------------


raise : Float -> Float -> Float
raise x exponent =
    if x < 0.0 then
        -(-x ^ exponent)
    else
        x ^ exponent
