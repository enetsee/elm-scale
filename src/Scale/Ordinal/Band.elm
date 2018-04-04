module Scale.Ordinal.Band
    exposing
        ( Band
        , Config
        , defaultConfig
        , band
        , customBand
        )

{-|
@docs Band, Config, band, customBand , defaultConfig
-}

import Scale.Ordinal exposing (Ordinal)
import Scale.Ordinal.Comparable as Ordinal
import Scale.Ordinal.Custom as Ordinal


{-| -}
type alias Band a domain =
    { a
        | domain : List domain
        , range : ( Float, Float )
        , config : Maybe Config
    }


{-| -}
type alias Config =
    { paddingInner : Float
    , paddingOuter : Float
    , align : Float
    , round : Bool
    }


{-| -}
defaultConfig : Config
defaultConfig =
    { paddingInner = 1.0
    , paddingOuter = 1.0
    , align = 0.5
    , round = False
    }


{-| Construct an ordinal scale from an ordinal, comparable, domain
    and a continuous range. Based on :
    https://github.com/vega/vega-scale/blob/master/src/scaleBand.js
-}
band : Band a comparableDomain -> Ordinal comparableDomain Float
band =
    bandHelper Ordinal.comparable


{-| Construct an ordinal scale from an ordinal, non-comparable, domain
    and a continuous range. Based on :
    https://github.com/vega/vega-scale/blob/master/src/scaleBand.js
-}
customBand : Band a domain -> Ordinal domain Float
customBand =
    bandHelper Ordinal.custom


bandHelper :
    ({ domain : List domain, range : List Float } -> Ordinal domain Float)
    -> Band a domain
    -> Ordinal domain Float
bandHelper constructOrdinal { domain, range, config } =
    let
        { paddingInner, paddingOuter, align, round } =
            Maybe.withDefault defaultConfig config

        length =
            List.length domain

        n =
            toFloat length

        space =
            bandSpace paddingInner paddingOuter length

        step =
            if round then
                toFloat <| floor ((stop - start) / space)
            else
                (stop - start) / space

        ( ( start, stop ), reverse ) =
            let
                ( x, y ) =
                    range
            in
                if y < x then
                    ( ( y, x ), List.reverse )
                else
                    ( ( x, y ), identity )

        newStart =
            let
                x =
                    start
                        + (stop - start - step * (n - paddingInner))
                        * align
            in
                if round then
                    toFloat <| Basics.round x
                else
                    x

        bandWidth =
            let
                x =
                    step * (1.0 - paddingInner)
            in
                if round then
                    toFloat <| Basics.round x
                else
                    x

        ordinalRange =
            List.range 1 length
                |> List.map (\i -> start + (toFloat i) * step)
                |> reverse
    in
        constructOrdinal
            { domain = domain
            , range = ordinalRange
            }


{-| Based on :
   https://github.com/vega/vega-scale/blob/master/src/bandSpace.js
-}
bandSpace : Float -> Float -> Int -> Float
bandSpace paddingInner paddingOuter count =
    let
        space =
            (toFloat count) - paddingInner + paddingOuter * 2
    in
        if count == 0 then
            1.0
        else if space > 0.0 then
            space
        else
            1.0
