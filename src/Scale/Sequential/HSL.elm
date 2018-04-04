module Scale.Sequential.HSL exposing (HSL, hsl)

{-|
@docs HSL, hsl
-}

import Color exposing (Color)
import Color.Interpolate as Interpolate
import Scale.Config exposing (OutsideDomain)
import Scale.Internal.Color exposing (fromHsla)
import Scale.Internal.Deinterpolate exposing (deinterpolateLinear)
import Scale.Internal.Ticks as Ticks
import Scale.Sequential exposing (Sequential, sequential)


{-| -}
type alias HSL a =
    { a
        | domain : ( Float, Float )
        , range : ( Color, Color )
        , long : Bool
        , outsideDomain : OutsideDomain
    }


{-| -}
hsl : HSL a -> Sequential Float Color
hsl { domain, range, long, outsideDomain } =
    let
        ( start, stop ) =
            range
    in
        sequential
            { domain = domain
            , compareDomain = compare
            , deinterpolateDomain = deinterpolateLinear
            , interpolator =
                if long then
                    Interpolate.hslLong start stop >> fromHsla
                else
                    Interpolate.hsl start stop >> fromHsla
            , outsideDomain = outsideDomain
            , ticks = Ticks.ticks
            }
