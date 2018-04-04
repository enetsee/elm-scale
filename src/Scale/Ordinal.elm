module Scale.Ordinal exposing (Ordinal, ordinal, scale, ticks)

{-|
@docs Ordinal, ordinal, scale, ticks
-}


{-| -}
type Ordinal domain range
    = Ordinal (OrdinalInternal domain range)


type alias OrdinalInternal domain range =
    { domain : List domain
    , mapping : domain -> Maybe range
    }


{-| -}
ordinal :
    { a | domain : List domain, mapping : domain -> Maybe range }
    -> Ordinal domain range
ordinal { domain, mapping } =
    Ordinal <| OrdinalInternal domain mapping


{-| -}
scale : Ordinal domain range -> domain -> Maybe range
scale (Ordinal { mapping }) d =
    mapping d


{-| -}
ticks : Ordinal domain range -> a -> List domain
ticks (Ordinal { domain }) _ =
    domain
