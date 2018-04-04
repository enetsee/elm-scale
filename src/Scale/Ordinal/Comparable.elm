module Scale.Ordinal.Comparable exposing (Comparable, comparable)

{-|
@docs Comparable, comparable
-}

import Dict
import Scale.Ordinal exposing (Ordinal, ordinal)


{-| -}
type alias Comparable a comparableDomain range =
    { a
        | domain : List comparableDomain
        , range : List range
    }


{-| -}
comparable : Comparable a comparableDomain range -> Ordinal comparableDomain range
comparable { domain, range } =
    let
        dict =
            List.map2 (,) domain range |> Dict.fromList
    in
        ordinal { domain = domain, mapping = \d -> Dict.get d dict }
