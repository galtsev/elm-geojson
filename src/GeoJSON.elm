module GeoJSON exposing (..)

import Json.Decode as D exposing (Decoder, field)

type Position = Position Float Float

type alias Ring = List Position

type Geometry = 
    Point Position
    | Polygon (List Ring)
    | MultiPolygon (List (List Ring))

type alias Feature props =
    { geometry : Geometry
    , properties : props
    }

type CRS = CRS String

type alias FeatureCollection props =
    { crs : CRS
    , features : List (Feature props)
    }


positionDecoder : Decoder Position
positionDecoder = 
    let
        toPosition : List Float -> Decoder Position
        toPosition lst = case lst of
            [lon, lat] -> D.succeed (Position lon lat)
            _ -> D.fail "Expecting [lon,lat]"
    in
        D.list D.float |> D.andThen toPosition


geometryDecoder : Decoder Geometry
geometryDecoder = 
    let
        fc : (a -> Geometry) -> Decoder a -> Decoder Geometry
        fc tag = D.map tag << field "coordinates"
        gd : String -> Decoder Geometry
        gd typ = case typ of
            "Point" -> fc Point positionDecoder
            "Polygon" -> fc Polygon << D.list << D.list <| positionDecoder
            "MultiPolygon" -> fc MultiPolygon << D.list << D.list << D.list <| positionDecoder
            other -> D.fail <| "Unexpected type:" ++ other
    in
    field "type" D.string
        |> D.andThen gd

featureDecoder : Decoder props -> Decoder (Feature props)
featureDecoder propsDecoder = D.map2 Feature
    (field "geometry" geometryDecoder)
    (field "properties" propsDecoder)

featureCollectionDecoder : Decoder props -> Decoder (FeatureCollection props)
featureCollectionDecoder propsDecoder = D.map2 FeatureCollection
    (D.succeed (CRS "ok"))
    (field "features" (D.list <| featureDecoder propsDecoder))

