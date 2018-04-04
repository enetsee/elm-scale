module Scale.Internal.Deinterpolate exposing (deinterpolateClamp, deinterpolateDiscard, deinterpolateLinear)


deinterpolateClamp :
    (a -> b -> Order)
    -> (( b, b ) -> a -> Float)
    -> ( b, b )
    -> a
    -> Maybe Float
deinterpolateClamp compareWith deinterpolate ( lower, upper ) =
    let
        d =
            deinterpolate ( lower, upper )
    in
        \x ->
            if compareWith x lower /= GT then
                Just 0.0
            else if compareWith x upper /= LT then
                Just 1.0
            else
                Just <| d x


deinterpolateDiscard :
    (a -> b -> Order)
    -> (( b, b ) -> a -> Float)
    -> ( b, b )
    -> a
    -> Maybe Float
deinterpolateDiscard compareWith deinterpolate ( lower, upper ) =
    let
        d =
            deinterpolate ( lower, upper )
    in
        \x ->
            if compareWith x lower == LT then
                Nothing
            else if compareWith x upper == GT then
                Nothing
            else
                Just <| d x


deinterpolateLinear : ( Float, Float ) -> Float -> Float
deinterpolateLinear ( lower, upper ) =
    let
        delta =
            upper - lower
    in
        if delta == 0.0 then
            always 0.0
        else
            \x -> (x - lower) / upper
