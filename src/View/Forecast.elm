module View.Forecast exposing (view)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Forecast exposing (..)
import Data.Helpers as DataHelper
import Data.Weather as WeatherData
import Date exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import LineChart exposing (..)
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line
import RemoteData exposing (..)
import Svg exposing (..)
import View.Helpers as ViewHelpers
import View.Weather as WeatherView


xAxisConfig : Axis.Config Forecast msg
xAxisConfig =
    Axis.time 800 "Date" (.dt >> Date.toTime)


tempChart : List Forecast -> Svg msg
tempChart forecasts =
    LineChart.view (.dt >> Date.toTime)
        getTemp
        [ LineChart.line Colors.blue Dots.cross "Temp" forecasts
        ]


tempChart2 : DataHelper.Unit -> List Forecast -> Svg msg
tempChart2 unit forecasts =
    let
        strUnit =
            case unit of
                DataHelper.Celsius ->
                    "C"

                DataHelper.Fahrenheit ->
                    "F"
    in
    LineChart.viewCustom
        { x = xAxisConfig
        , y = Axis.default 450 ("Temp (" ++ strUnit ++ ")") getTemp
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.default
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }
        [ LineChart.line Colors.blue Dots.cross "Temp" forecasts
        ]


humidityChart : List Forecast -> Svg msg
humidityChart forecasts =
    LineChart.view1 (.dt >> Date.toTime)
        getHumidity
        forecasts


humidityChart2 : List Forecast -> Svg msg
humidityChart2 forecasts =
    LineChart.viewCustom
        { x = xAxisConfig
        , y = Axis.default 450 "Humidity" getHumidity
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.default
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }
        [ LineChart.line Colors.blue Dots.cross "Humidity" forecasts
        ]


getHumidity : Forecast -> Float
getHumidity forecast =
    forecast.main.humidity
        |> Maybe.withDefault 0


getTemp : Forecast -> Float
getTemp forecast =
    forecast.main.temp



-- TODO: custom time interval


view : String -> String -> DataHelper.Unit -> ForecastResponse -> Html msg
view city country unit forecast =
    div []
        [ div [ class "section-header" ] [ Html.text ("5 day temperature chart for " ++ city ++ ", " ++ country) ]
        , tempChart2 unit forecast.forecasts

        --  , humidityChart2 forecast.forecasts
        , viewDetails city country unit forecast.forecasts
        ]


viewSummary : DataHelper.Unit -> Forecast -> Html msg
viewSummary unit forecast =
    let
        tableDef =
            [ ( "Temperature", ViewHelpers.viewTemperature unit forecast.main.temp )
            , ( "Cloudiness", WeatherView.viewCloud forecast.weather )
            , ( "Wind", WeatherView.viewWind unit forecast.wind )
            , ( "Pressure", WeatherView.viewPressure forecast.main.pressure )
            ]
    in
    div []
        [ table [ class "weather-table" ]
            [ tbody []
                (tableDef
                    |> List.map
                        (\row ->
                            tr []
                                [ td [] [ Html.text (Tuple.first row) ]
                                , td [] [ Html.text (Tuple.second row) ]
                                ]
                        )
                )
            ]
        ]


viewForecast : DataHelper.Unit -> Forecast -> Html msg
viewForecast unit forecast =
    div []
        [ Grid.row []
            [ Grid.col [ Col.md6 ] [ Html.text (WeatherView.viewTime forecast.dt) ]
            , Grid.col [ Col.md6 ] [ viewSummary unit forecast ]
            ]
        ]


{-| Display all forecast details
-}
viewDetails : String -> String -> DataHelper.Unit -> List Forecast -> Html msg
viewDetails city country unit forecasts =
    div []
        [ div [ class "section-header" ] [ Html.text ("Hourly weather and forecasts in " ++ city ++ ", " ++ country) ]
        , div []
            (forecasts
                |> List.map (viewForecast unit)
            )
        ]
