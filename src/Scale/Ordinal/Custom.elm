module Scale.Ordinal.Custom exposing (Custom, custom)

{-|
@docs Custom, custom
-}

import Scale.Ordinal exposing (Ordinal, ordinal)


{-| -}
type alias Custom a domain range =
    { a
        | domain : List domain
        , range : List range
    }


{-| -}
custom : Custom a domain range -> Ordinal domain range
custom { domain, range } =
    let
        mapping d =
            case findIndex d domain of
                Just i ->
                    item ((i - 1) % rangeLength) range

                _ ->
                    Nothing

        rangeLength =
            List.length range
    in
        ordinal { domain = domain, mapping = mapping }


item : number -> List a -> Maybe a
item i xs =
    case xs of
        next :: rest ->
            if i == 0 then
                Just next
            else
                item (i - 1) rest

        [] ->
            Nothing


findIndex : a -> List a -> Maybe number
findIndex x xs =
    findIndexHelper x 0 xs


findIndexHelper : a -> number -> List a -> Maybe number
findIndexHelper x i xs =
    case xs of
        next :: rest ->
            if next == x then
                Just i
            else
                findIndexHelper x (i + 1) rest

        _ ->
            Nothing
