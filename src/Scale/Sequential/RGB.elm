module Scale.Sequential.RGB exposing (RGB, rgb)

{-|
@docs RGB , rgb
-}

import Color exposing (Color)
import Color.Interpolate as Interpolate
import Scale.Config exposing (OutsideDomain)
import Scale.Internal.Color exposing (fromRgba)
import Scale.Internal.Deinterpolate exposing (deinterpolateLinear)
import Scale.Internal.Ticks as Ticks
import Scale.Sequential exposing (Sequential, sequential)


{-| -}
type alias RGB a =
    { a
        | domain : ( Float, Float )
        , range : ( Color, Color )
        , gamma : Maybe Float
        , outsideDomain : OutsideDomain
    }


{-| -}
rgb : RGB a -> Sequential Float Color
rgb { domain, range, gamma, outsideDomain } =
    let
        ( start, stop ) =
            range
    in
        sequential
            { domain = domain
            , compareDomain = compare
            , deinterpolateDomain = deinterpolateLinear
            , interpolator = Interpolate.rgb gamma start stop >> fromRgba
            , outsideDomain = outsideDomain
            , ticks = Ticks.ticks
            }
