module Data.AirQuality exposing (..)

import Data.Helpers exposing (..)
import Date exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, hardcoded, optional, required)


-- Carbon Monoxide


type alias COData =
    { value : Float -- mixing ratio
    , pressure : Float -- atmospheric pressure, hPa
    , precision : Float
    }


type alias COQuality =
    { time : Date
    , location : Coord
    , data : List COData
    }


coDataDecoder : Decoder COData
coDataDecoder =
    decode COData
        |> required "value" Decode.float
        |> required "pressure" Decode.float
        |> required "precision" Decode.float


coQualityDecoder : Decoder COQuality
coQualityDecoder =
    decode COQuality
        |> required "time" dateDecoder
        |> required "location" locationDecoder
        |> required "data" (Decode.list coDataDecoder)



-- Ozone


type alias OzoneQuality =
    { time : Date
    , location : Coord
    , data : Float -- Ozone layer thickness
    }


ozoneQualityDecoder : Decoder OzoneQuality
ozoneQualityDecoder =
    decode OzoneQuality
        |> required "time" dateDecoder
        |> required "location" locationDecoder
        |> required "data" Decode.float



-- Sulfur Sioxide


type alias SO2Quality =
    { time : Date
    , location : Coord
    , data : List COData
    }


so2QualityDecoder : Decoder SO2Quality
so2QualityDecoder =
    decode SO2Quality
        |> required "time" dateDecoder
        |> required "location" locationDecoder
        |> required "data" (Decode.list coDataDecoder)



-- Nitrogen Dioxide


type alias NO2Data =
    { name : String -- no2, no2_stratm no2_trop, etc
    , value : Float -- volume mixing ratio
    , precision : Float
    }


type alias NO2Quality =
    { time : Date
    , location : Coord
    , data : Dict String NO2Data
    }


no2DataDecoder : Decoder NO2Data
no2DataDecoder =
    decode NO2Data
        |> hardcoded ""
        -- will fill in during decoding container
        |> required "value" Decode.float
        |> required "precision" Decode.float


no2QualityDecoder : Decoder NO2Quality
no2QualityDecoder =
    decode NO2Quality
        |> required "time" dateDecoder
        |> required "location" locationDecoder
        |> required "data"
            (Decode.dict no2DataDecoder
                |> Decode.map
                    (\theDict ->
                        theDict
                            |> Dict.map
                                (\key val ->
                                    { val | name = key }
                                )
                    )
            )
