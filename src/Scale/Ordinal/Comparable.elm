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
            zipCycle domain range |> Dict.fromList
    in
        ordinal { domain = domain, mapping = \d -> Dict.get d dict }


{-| Zip lists cycling through the elements of the second of the two lists if
    it is shorter than the first
-}
zipCycle : List a -> List b -> List ( a, b )
zipCycle xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        _ ->
            zipCycleHelper xs ys [] xs ys


zipCycleHelper :
    List a
    -> List b
    -> List ( a, b )
    -> List a
    -> List b
    -> List ( a, b )
zipCycleHelper xs ys accu cx cy =
    case ( cx, cy ) of
        ( nextX :: restX, nextY :: restY ) ->
            zipCycleHelper xs ys (( nextX, nextY ) :: accu) restX restY

        ( _ :: _, _ ) ->
            zipCycleHelper xs ys accu cx ys

        _ ->
            accu
