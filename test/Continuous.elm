module Continuous exposing (continuous)

import Expect
import Fuzz
import Test exposing (Test)
import Scale.Config as Config
import Scale.Continuous as Continuous exposing (Continuous)
import Scale.Continuous.Linear as Linear


continuous : Test
continuous =
    Test.describe "Continuous"
        [ Test.describe "Continuous.Linear"
            [ Test.describe "Config.Allow"
                [ Test.fuzz Fuzz.float "unscale << scale === identity" <|
                    \x ->
                        x
                            |> Continuous.scale linearAllow
                            |> Maybe.andThen (Continuous.unscale linearAllow)
                            |> Expect.equal (Just x)
                ]
            , Test.describe "Config.Discard"
                [ Test.fuzz (Fuzz.floatRange -100 100) "unscale << scale === identity for all x in domain" <|
                    \x ->
                        x
                            |> Continuous.scale linearDiscard
                            |> Maybe.andThen (Continuous.unscale linearDiscard)
                            |> Expect.equal (Just x)
                , Test.fuzz (Fuzz.floatRange -1000 -100.1) "scale discards x for all x less than domain" <|
                    \x ->
                        x
                            |> Continuous.scale linearDiscard
                            |> Expect.equal Nothing
                , Test.fuzz (Fuzz.floatRange 101.1 1000) "scale discards x for all x greater than domain" <|
                    \x ->
                        x
                            |> Continuous.scale linearDiscard
                            |> Expect.equal Nothing
                ]
            ]
        ]



-- LINEAR SCALE ----------------------------------------------------------------


linearAllow : Continuous.Continuous Float Float
linearAllow =
    Linear.linearScale
        { domain = ( -100, 100 )
        , range = ( 0, 200 )
        , outsideDomain = Config.Allow
        }


linearClamp : Continuous Float Float
linearClamp =
    Linear.linearScale
        { domain = ( -100, 100 )
        , range = ( 0, 200 )
        , outsideDomain = Config.Clamp
        }


linearDiscard : Continuous Float Float
linearDiscard =
    Linear.linearScale
        { domain = ( -100, 100 )
        , range = ( 0, 200 )
        , outsideDomain = Config.Discard
        }
