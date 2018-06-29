module Data.Helpers exposing (..)

import Date exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)


{-| basic types
-}
type Unit
    = Celsius
    | Fahrenheit


type AirQuality
    = CarbonMonoxide
    | Ozone
    | SulfurDioxide
    | NitrogenDioxide


type alias Coord =
    { lat : Float
    , lon : Float
    }


type alias Cloud =
    { all : Maybe Float
    }


type alias Rain =
    { last3h : Maybe Float
    }


type alias Snow =
    { last3h : Maybe Float
    }


type alias Wind =
    { speed : Float
    , direction : Maybe Float -- in degrees
    }


type alias MainDetails =
    { temp : Float
    , tempMin : Maybe Float
    , tempMax : Maybe Float
    , pressure : Maybe Float
    , seaLevel : Maybe Float
    , groundLevel : Maybe Float
    , humidity : Maybe Float
    }


mainDetailsDecoder : Decoder MainDetails
mainDetailsDecoder =
    decode MainDetails
        |> required "temp" Decode.float
        |> optional "temp_min" (Decode.nullable Decode.float) Nothing
        |> optional "temp_max" (Decode.nullable Decode.float) Nothing
        |> optional "pressure" (Decode.nullable Decode.float) Nothing
        |> optional "sea_level" (Decode.nullable Decode.float) Nothing
        |> optional "grnd_level" (Decode.nullable Decode.float) Nothing
        |> optional "humidity" (Decode.nullable Decode.float) Nothing


posixDateDecoder : Decoder Date
posixDateDecoder =
    Decode.map (\a -> Date.fromTime (a * 1000)) Decode.float


dateDecoder : Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                let
                    st =
                        Date.fromString s
                in
                case st of
                    Err e ->
                        Decode.fail e

                    Ok a ->
                        Decode.succeed a
            )


cloudDecoder : Decoder Cloud
cloudDecoder =
    decode Cloud
        |> optional "all" (Decode.nullable Decode.float) Nothing


windDecoder : Decoder Wind
windDecoder =
    decode Wind
        |> required "speed" Decode.float
        |> optional "deg" (Decode.nullable Decode.float) Nothing


snowDecoder : Decoder Snow
snowDecoder =
    decode Snow
        |> optional "3h" (Decode.nullable Decode.float) Nothing


rainDecoder : Decoder Rain
rainDecoder =
    decode Rain
        |> optional "3h" (Decode.nullable Decode.float) Nothing


coordDecoder : Decoder Coord
coordDecoder =
    decode Coord
        |> required "lat" Decode.float
        |> required "lon" Decode.float


locationDecoder : Decoder Coord
locationDecoder =
    decode Coord
        |> required "latitude" Decode.float
        |> required "longtitude" Decode.float
