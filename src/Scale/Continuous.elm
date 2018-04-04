module Scale.Continuous
    exposing
        ( Continuous
        , continuous
        , scale
        , unscale
        , ticks
        )

{-| Continuous Scale
@docs Continuous, continuous, scale , unscale, ticks


-}

import Scale.Config exposing (OutsideDomain(..))
import Scale.Internal.Deinterpolate exposing (deinterpolateClamp, deinterpolateDiscard, deinterpolateLinear)
import Scale.Internal.Reinterpolate exposing (reinterpolateClamp, reinterpolateDiscard)


{-| -}
type Continuous domain range
    = Continuous (ContinuousInternal domain range)


type alias ContinuousInternal domain range =
    { domain : ( domain, domain )
    , range : ( range, range )
    , compareDomain : domain -> domain -> Order
    , deinterpolateDomain : ( domain, domain ) -> domain -> Float
    , interpolateRange : ( range, range ) -> Float -> range
    , reinterpolateDomain : ( domain, domain ) -> range -> domain
    , outsideDomain : OutsideDomain
    , ticks : ( domain, domain ) -> Int -> List domain
    }


{-| -}
continuous :
    { a
        | compareDomain : domain -> domain -> Order
        , deinterpolateDomain : ( domain, domain ) -> domain -> Float
        , domain : ( domain, domain )
        , interpolateRange : ( range, range ) -> Float -> range
        , outsideDomain : OutsideDomain
        , range : ( range, range )
        , reinterpolateDomain : ( domain, domain ) -> range -> domain
        , ticks : ( domain, domain ) -> Int -> List domain
    }
    -> Continuous domain range
continuous c =
    Continuous
        { domain = c.domain
        , range = c.range
        , compareDomain = c.compareDomain
        , deinterpolateDomain = c.deinterpolateDomain
        , interpolateRange = c.interpolateRange
        , reinterpolateDomain = c.reinterpolateDomain
        , outsideDomain = c.outsideDomain
        , ticks = c.ticks
        }


{-| -}
ticks : Continuous domain range -> Int -> List domain
ticks (Continuous c) count =
    c.ticks c.domain count


{-| -}
scale : Continuous domain range -> (domain -> Maybe range)
scale (Continuous { compareDomain, domain, range, outsideDomain, deinterpolateDomain, interpolateRange }) =
    Maybe.map (interpolateRange range)
        << (case outsideDomain of
                Clamp ->
                    deinterpolateClamp compareDomain deinterpolateDomain domain

                Discard ->
                    deinterpolateDiscard compareDomain deinterpolateDomain domain

                Allow ->
                    Just << deinterpolateDomain domain
           )


{-| -}
unscale : Continuous domain Float -> (Float -> Maybe domain)
unscale (Continuous { domain, range, outsideDomain, reinterpolateDomain, interpolateRange }) =
    (case outsideDomain of
        Clamp ->
            reinterpolateDiscard reinterpolateDomain domain

        Discard ->
            reinterpolateClamp reinterpolateDomain domain

        Allow ->
            Just << reinterpolateDomain domain
    )
        << (deinterpolateLinear range)
