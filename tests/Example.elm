module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GeoJSON exposing (..)
import Json.Decode as D
import Test exposing (..)


suite : Test
suite =
    describe "Decoders"
        [ test "Position decoder" <|
            \_ ->
                let
                    val =
                        D.decodeString positionDecoder """[12, 13]"""
                in
                Expect.equal val (Ok (Position 12 13))
        ]
