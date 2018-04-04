module Scale.Sequential.RGBBasis exposing (RGBBasis, rgbBasis)

{-|
@docs RGBBasis , rgbBasis
-}

import Color exposing (Color)
import Color.Interpolate as Interpolate
import Scale.Config exposing (OutsideDomain)
import Scale.Internal.Color exposing (fromRgba)
import Scale.Internal.Deinterpolate exposing (deinterpolateLinear)
import Scale.Internal.Ticks as Ticks
import Scale.Sequential exposing (Sequential, sequential)


{-| -}
type alias RGBBasis a =
    { a
        | domain : ( Float, Float )
        , range : List Color
        , closed : Bool
        , outsideDomain : OutsideDomain
    }


{-| -}
rgbBasis : RGBBasis a -> Sequential Float Color
rgbBasis { domain, range, closed, outsideDomain } =
    sequential
        { domain = domain
        , compareDomain = compare
        , deinterpolateDomain = deinterpolateLinear
        , interpolator =
            if closed then
                Interpolate.rgbBasisClosed range >> fromRgba
            else
                Interpolate.rgbBasis range >> fromRgba
        , outsideDomain = outsideDomain
        , ticks = Ticks.ticks
        }
