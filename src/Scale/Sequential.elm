module Scale.Sequential exposing (Sequential, sequential, scale, ticks)

{-|
@docs Sequential, sequential, scale, ticks
-}

import Scale.Config exposing (OutsideDomain(..))
import Scale.Internal.Deinterpolate exposing (deinterpolateClamp, deinterpolateDiscard, deinterpolateLinear)


{-| -}
type Sequential domain range
    = Sequential (SequentialInternal domain range)


type alias SequentialInternal domain range =
    { domain : ( domain, domain )
    , compareDomain : domain -> domain -> Order
    , deinterpolateDomain : ( domain, domain ) -> domain -> Float
    , interpolator : Float -> range
    , outsideDomain : OutsideDomain
    , ticks : ( domain, domain ) -> Int -> List domain
    }


{-| -}
sequential :
    { a
        | compareDomain : domain -> domain -> Order
        , deinterpolateDomain : ( domain, domain ) -> domain -> Float
        , domain : ( domain, domain )
        , interpolator : Float -> range
        , outsideDomain : OutsideDomain
        , ticks : ( domain, domain ) -> Int -> List domain
    }
    -> Sequential domain range
sequential { domain, compareDomain, deinterpolateDomain, interpolator, outsideDomain, ticks } =
    Sequential <|
        { domain = domain
        , compareDomain = compareDomain
        , deinterpolateDomain = deinterpolateDomain
        , interpolator = interpolator
        , outsideDomain = outsideDomain
        , ticks = ticks
        }


{-| -}
ticks : Sequential domain range -> Int -> List domain
ticks (Sequential s) n =
    s.ticks s.domain n


{-| -}
scale : Sequential domain range -> domain -> Maybe range
scale (Sequential { compareDomain, deinterpolateDomain, domain, interpolator, outsideDomain }) =
    Maybe.map interpolator
        << (case outsideDomain of
                Clamp ->
                    deinterpolateClamp compareDomain deinterpolateDomain domain

                Discard ->
                    deinterpolateDiscard compareDomain deinterpolateDomain domain

                Allow ->
                    Just << deinterpolateDomain domain
           )
