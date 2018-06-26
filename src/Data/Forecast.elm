module Data.Forecast exposing(..)

import Data.Helpers exposing (..)
import Data.Weather exposing(..)
import Date exposing(..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)

type alias City =
    { id: Int 
    , name: String 
    , coord: Coord
    , country: String -- need ISO 3166 country codes
    }

type alias Forecast = 
    { dt: Date 
    , main: MainDetails 
    , weather: List Weather
    , clouds: Maybe Cloud 
    , wind: Maybe Wind
    , rain : Maybe Rain 
    , snow: Maybe Snow
    , dtTxt: Date -- Not needed
    }

type alias ForecastResponse = 
    { count: Int 
    , forecasts: List Forecast 
    , city: City
    }

--SERIALIZATION
cityDecoder: Decoder City 
cityDecoder = 
    decode City 
        |> required "id" Decode.int
        |> required "name" Decode.string 
        |> required "coord" coordDecoder
        |> required "country" Decode.string


forecastResponseDecoder: Decoder ForecastResponse
forecastResponseDecoder = 
    decode ForecastResponse
        |> required "cnt" Decode.int   
        |> required "list" (Decode.list forecastDecoder)
        |> required "city" cityDecoder

forecastDecoder: Decoder Forecast 
forecastDecoder = 
    decode Forecast 
        |> required "dt" posixDateDecoder 
        |> required "main" mainDetailsDecoder
        |> required "weather" (Decode.list weatherDecoder)
        |> optional "clouds" (Decode.nullable cloudDecoder) Nothing
        |> optional "wind" (Decode.nullable windDecoder) Nothing
        |> optional "rain" (Decode.nullable rainDecoder) Nothing 
        |> optional "snow" (Decode.nullable snowDecoder) Nothing
        |> required "dt_txt" dateDecoder
