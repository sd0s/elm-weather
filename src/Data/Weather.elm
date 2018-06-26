module Data.Weather exposing(..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)
import Date exposing (..)
import Data.Helpers exposing(..)

type alias Weather = 
    { conditionId: Int 
    , main: String 
    , description: String 
    , iconId : String  -- Any need?
    }


type alias Sys = 
    { sunrise: Date
    , sunset: Date
    -- add others if necessary
    }

type alias CurrentWeatherResponse = 
    { coord: Coord
    , weather: List Weather
    , main: MainDetails
    , wind: Maybe Wind
    , clouds: Maybe Cloud
    , rain: Maybe Rain
    , snow: Maybe Snow
    , dt: Date 
    , sys: Sys
    , cityId: Int 
    , cityName: String
--    , visibility: Float -- XML response seems to be different from json one
    }

-- SERIALIZATION
sysDecoder: Decoder Sys
sysDecoder = 
    decode Sys
        |> required "sunrise" posixDateDecoder
        |> required "sunset" posixDateDecoder

weatherDecoder: Decoder Weather
weatherDecoder = 
    decode Weather 
        |> required "id" Decode.int
        |> required "main" Decode.string
        |> required "description" Decode.string 
        |> required "icon" Decode.string 

currentWeatherResponseDecoder: Decoder CurrentWeatherResponse
currentWeatherResponseDecoder = 
    decode CurrentWeatherResponse
        |> required "coord" coordDecoder
        |> required "weather" (Decode.list weatherDecoder)
        |> required "main" mainDetailsDecoder
        |> optional "wind" (Decode.nullable windDecoder) Nothing
        |> optional "clouds" (Decode.nullable cloudDecoder) Nothing 
        |> optional "rain" (Decode.nullable rainDecoder) Nothing
        |> optional "snow" (Decode.nullable snowDecoder) Nothing
        |> required "dt" posixDateDecoder
        |> required "sys" sysDecoder
        |> required "id" Decode.int
        |> required "name" Decode.string 

