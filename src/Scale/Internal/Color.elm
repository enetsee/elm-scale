module Scale.Internal.Color exposing (fromRgba, fromHsla)

import Color exposing (Color)


fromRgba : { a | alpha : Float, blue : Int, green : Int, red : Int } -> Color
fromRgba { red, green, blue, alpha } =
    Color.rgba red green blue alpha


fromHsla : { a | hue : Float, saturation : Float, lightness : Float, alpha : Float } -> Color
fromHsla { hue, saturation, lightness, alpha } =
    Color.hsla hue saturation lightness alpha
