module View.Weather exposing(..)

import Html exposing(..)
import Html.Attributes exposing(..)

import RemoteData exposing (..)

import Data.Helpers exposing(..)
import Data.Weather exposing(..)
import View.Helpers as ViewHelpers exposing (empty)
import Date exposing (..)

viewSpeed: Unit -> Float -> String
viewSpeed unit speed = 
    let
        strUnit = 
            case unit of 
                Celsius -> "m/s"
                Fahrenheit -> "m/h"
    in
    (toString speed) ++ " " ++ strUnit
            
viewDirection: Float -> String
viewDirection direction = 
    (toString direction) ++ " deg"

viewWind : Unit -> Maybe Wind -> String 
viewWind unit wind = 
    wind 
        |> Maybe.map 
            (\wnd ->
                ((viewSpeed unit wnd.speed) ++ ", " ++ (viewDirection wnd.direction))
            )
        |> Maybe.withDefault ""    

viewCloud : List Weather -> String 
viewCloud weather = 
    weather 
        |> List.head
        |> Maybe.map .description
        |> Maybe.withDefault ""

viewPressure: Maybe Float -> String
viewPressure pressure = 
    pressure
        |> Maybe.map 
            (\pr ->
                (toString pr) ++ " hpa"
            )  
        |> Maybe.withDefault ""

viewHumidity : Maybe Float -> String
viewHumidity humidity = 
        humidity
        |> Maybe.map 
            (\pr ->
                (toString pr) ++ " %"
            )  
        |> Maybe.withDefault ""

viewTime: Date -> String 
viewTime date =
        [ date 
            |> Date.hour
        , date 
            |> Date.minute
        ]
        |> List.map toString
        |> String.join ":"

viewCoord : Coord -> String
viewCoord location = 
    "[" ++ (toString location.lat) ++ ", "
        ++ (toString location.lon) ++ "]"

view: String -> String -> Unit -> CurrentWeatherResponse -> Html msg 
view city country unit weather = 
    let
        weatherTableDef = 
            [ ("Wind", viewWind unit weather.wind ) 
            , ("Cloudiness", viewCloud weather.weather)
            , ("Pressure", viewPressure weather.main.pressure)
            , ("Humidity", viewHumidity weather.main.humidity)
            , ("Sunrise", viewTime weather.sys.sunrise)
            , ("Sunset", viewTime weather.sys.sunset)
            , ("Geo coords", viewCoord weather.coord)
            ]

    in 
    div [] 
        [ div [class "weather-city-name"] [Html.text ("Current weather in " ++ city ++ ", " ++ country)]
        , div [class "weather-temperature"] [Html.text (ViewHelpers.viewTemperature unit weather.main.temp)]
        , div [] 
            [
                table [class "weather-table"] 
                [ tbody [] 
                    ( weatherTableDef 
                        |> List.map 
                            (\row ->
                                tr [] 
                                    [ td [] [text (Tuple.first row)]
                                    , td [] [text (Tuple.second row)]
                                    ]
                            )
                    )
                    ]
            ]
    ]