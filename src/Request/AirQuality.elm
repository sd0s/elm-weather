module Request.AirQuality exposing(..)

import Http
import Data.AirQuality exposing(..)
import Request.Helpers exposing(owmAirQualityUrl) 
import Data.Helpers exposing(..)

getCOQuality: Coord -> Http.Request COQuality 
getCOQuality location = 
     Http.get (owmAirQualityUrl CarbonMonoxide location) coQualityDecoder   

getOzoneQuality: Coord -> Http.Request OzoneQuality
getOzoneQuality location =
    Http.get (owmAirQualityUrl Ozone location) ozoneQualityDecoder

getSO2Quality: Coord -> Http.Request SO2Quality
getSO2Quality location =
    Http.get (owmAirQualityUrl SulfurDioxide location) so2QualityDecoder

getNO2Quality: Coord -> Http.Request NO2Quality
getNO2Quality location =
    Http.get (owmAirQualityUrl NitrogenDioxide location) no2QualityDecoder
