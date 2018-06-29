module Request.AirQuality exposing (..)

import Data.AirQuality exposing (..)
import Data.Helpers exposing (..)
import Http
import Request.Helpers exposing (owmAirQualityUrl)


getCOQuality : Coord -> Http.Request COQuality
getCOQuality location =
    Http.get (owmAirQualityUrl CarbonMonoxide location) coQualityDecoder


getOzoneQuality : Coord -> Http.Request OzoneQuality
getOzoneQuality location =
    Http.get (owmAirQualityUrl Ozone location) ozoneQualityDecoder


getSO2Quality : Coord -> Http.Request SO2Quality
getSO2Quality location =
    Http.get (owmAirQualityUrl SulfurDioxide location) so2QualityDecoder


getNO2Quality : Coord -> Http.Request NO2Quality
getNO2Quality location =
    Http.get (owmAirQualityUrl NitrogenDioxide location) no2QualityDecoder
