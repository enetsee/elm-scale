module Scale.Config exposing (OutsideDomain(..))

{-| Scale Configuration

@docs OutsideDomain

-}


{-| Options for the behavior of a `Scale` when the value it is applied to lies
outside of the defined domain
-}
type OutsideDomain
    = Clamp
    | Discard
    | Allow
