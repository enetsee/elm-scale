module Scale.Internal.Reinterpolate exposing (reinterpolateLinear, reinterpolateClamp, reinterpolateDiscard)


reinterpolateLinear : ( number, number ) -> number -> number
reinterpolateLinear ( lower, upper ) =
    let
        delta =
            upper - lower
    in
        \t -> lower + delta * t


reinterpolateClamp :
    (( a, a ) -> Float -> a)
    -> ( a, a )
    -> Float
    -> Maybe a
reinterpolateClamp reinterpolate ( lower, upper ) =
    let
        r =
            reinterpolate ( lower, upper )
    in
        \t ->
            if t <= 0.0 then
                Just lower
            else if t >= 1.0 then
                Just upper
            else
                Just <| r t


reinterpolateDiscard :
    (( a, a ) -> Float -> a)
    -> ( a, a )
    -> Float
    -> Maybe a
reinterpolateDiscard reinterpolate ( lower, upper ) =
    let
        r =
            reinterpolate ( lower, upper )
    in
        \t ->
            if t <= 0.0 then
                Nothing
            else if t >= 1.0 then
                Nothing
            else
                Just <| r t
