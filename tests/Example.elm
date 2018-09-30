module Example exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GeoJSON exposing (..)
import Json.Decode as D
import Test exposing (..)


expectDecodeTo : D.Decoder a -> String -> a -> Expectation
expectDecodeTo decoder src v =
    Expect.equal (D.decodeString decoder src) (Ok v)


ring : List ( Float, Float ) -> Ring
ring =
    List.map (\( lon, lat ) -> Position lon lat)


suite : Test
suite =
    describe "Decoders"
        [ test "Position decoder" <|
            \_ ->
                let
                    src =
                        """[12, 13]"""
                in
                expectDecodeTo positionDecoder src (Position 12 13)
        , test "Point decoder" <|
            \_ ->
                let
                    src =
                        """{"type": "Point", "coordinates": [34, 57]}"""
                in
                expectDecodeTo geometryDecoder src (Point (Position 34 57))
        , test "Polygon decoder" <|
            \_ ->
                let
                    src =
                        """
                        {
                            "type": "Polygon",
                            "coordinates": [
                                [[0,0], [0,1], [1,1], [1, 0], [0,0]]
                            ]
                        }
                        """

                    expected =
                        Polygon [ ring [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( 1, 0 ), ( 0, 0 ) ] ]
                in
                expectDecodeTo geometryDecoder src expected
        , test "Feature decoder" <|
            \_ ->
                let
                    src =
                        """
                    {
                        "type": "Feature",
                        "geometry": {
                            "type": "Point",
                            "coordinates": [43, 21]
                        },
                        "properties": "hello"
                    }
                    """

                    expected =
                        { geometry = Point (Position 43 21)
                        , properties = "hello"
                        }
                in
                expectDecodeTo (featureDecoder D.string) src expected
        , test "FeatureCollection decoder" <|
            \_ ->
                let
                    src =
                        """
                    {
                        "type": "FeatureCollection",
                        "features": [
                            {
                                "type": "Feature",
                                "geometry": {
                                    "type": "Point",
                                    "coordinates": [88, 64]
                                },
                                "properties": "someprops"
                            }
                        ]
                    }
                    """

                    expected =
                        { crs = CRS "ok"
                        , features =
                            [ { geometry = Point (Position 88 64), properties = "someprops" }
                            ]
                        }
                in
                expectDecodeTo (featureCollectionDecoder D.string) src expected
        ]
