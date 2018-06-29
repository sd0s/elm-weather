module Request.Helpers exposing (..)

import Config exposing (apiKey)
import Data.Helpers exposing (..)


type Service
    = Weather
    | Forecast


type alias ServiceContext =
    { unit : Unit
    , city : String
    , country : String
    }


owmBaseUrl =
    "http://api.openweathermap.org/data/2.5/"


owmPollutionUrl =
    "http://api.openweathermap.org/pollution/v1/"


triggerBaseUrl =
    "http://api.openweathermap.org/data/3.0/triggers"


owmUrl : Service -> ServiceContext -> String
owmUrl service ctx =
    let
        serviceStr =
            case service of
                Weather ->
                    "weather"

                Forecast ->
                    "forecast"

        unitStr =
            case ctx.unit of
                Celsius ->
                    "metric"

                Fahrenheit ->
                    "imperial"
    in
    owmBaseUrl
        ++ serviceStr
        ++ "?q="
        ++ ctx.city
        ++ ","
        ++ ctx.country
        ++ "&units="
        ++ unitStr
        ++ "&appid="
        ++ apiKey


owmAirQualityUrl : AirQuality -> Coord -> String
owmAirQualityUrl airQuality location =
    let
        strQuality =
            case airQuality of
                CarbonMonoxide ->
                    "co"

                Ozone ->
                    "o3"

                SulfurDioxide ->
                    "so2"

                NitrogenDioxide ->
                    "no2"

        strLocation =
            toString location.lat ++ "," ++ toString location.lon
    in
    owmPollutionUrl ++ strQuality ++ "/" ++ strLocation ++ "/current.json?appid=" ++ apiKey


triggerUrl : String
triggerUrl =
    triggerBaseUrl ++ "?appid=" ++ apiKey

triggerUrl2: String -> String 
triggerUrl2 str = 
    triggerBaseUrl ++ str ++ "?appid=" ++ apiKey
