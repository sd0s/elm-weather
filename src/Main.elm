module Main exposing (..)

import RemoteData exposing(..)
import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing(..)

import Data.Weather exposing(..)
import Request.Helpers as RH
import Data.Helpers as DH
import Request.Weather as RW 
import Data.Forecast as DF 
import Request.Forecast as RF 
import Data.AirQuality as DA
import Request.AirQuality as RA

import View.Helpers exposing(..)
import View.Weather as WeatherView
import View.Forecast as ForecastView 

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.CDN as CDN

{-| Entry point for the app. Actual air pollution data seems to be very spotty.
Not getting any current data
-}

-- TYPES 

type Msg 
    = GetForecasts
    | GetCurrentWeatherResponse (WebData CurrentWeatherResponse)
    | GetForecastResponse (WebData DF.ForecastResponse)
    | GetCOQualityResponse (WebData DA.COQuality)
    | GetOzoneQualityResponse (WebData DA.OzoneQuality)
    | GetSO2QualityResponse (WebData DA.SO2Quality)
    | GetNO2QualityResponse (WebData DA.NO2Quality) 
    | SetCity String 
    | SetCountry String 
    | UpdateLocation 
    | UpdateUnit DH.Unit

-- type alias AirQuality =
--     { quality: String
--     }

type alias LocationForm = 
    { city: Maybe String 
    , country: Maybe String 
    }


type alias Model = 
    { currentWeather : WebData CurrentWeatherResponse
    , fiveDayForecast: WebData DF.ForecastResponse
    , coQuality: WebData DA.COQuality
    , ozoneQuality : WebData DA.OzoneQuality
    , so2Quality: WebData DA.SO2Quality
    , no2Quality: WebData DA.NO2Quality
    , unit: DH.Unit
    , city: String
    , country: String
    , locationForm: LocationForm  
    }

initLocationForm : Maybe String -> Maybe String -> LocationForm
initLocationForm city country = 
    { city = city 
    , country = country
    }
-- INIT
initModel: String -> String -> DH.Unit -> Model 
initModel city country unit = 
    { currentWeather = NotAsked
    , fiveDayForecast = NotAsked
    , coQuality = NotAsked
    , ozoneQuality = NotAsked
    , so2Quality = NotAsked
    , no2Quality = NotAsked
    , unit = unit
    , city = city
    , country = country
    , locationForm = initLocationForm (Just city) (Just country)
    }

initialize: String -> String -> DH.Unit -> (Model, Cmd Msg)
initialize city country unit = 
    let
        model = initModel city country unit
    in
    ( model 
    , Cmd.batch 
        [ (getCurrentWeather model)
        , (getForecast model)
        ]
    ) 


init: (Model, Cmd Msg)
init =
    initialize "Rome" "Italy" DH.Celsius

buildContext: Model -> RH.ServiceContext
buildContext model =
    RH.ServiceContext model.unit model.city model.country 


-- SERVICE CALLS
getCoQuality: DH.Coord -> Cmd Msg 
getCoQuality location = 
    RA.getCOQuality location 
        |> RemoteData.sendRequest
        |> Cmd.map GetCOQualityResponse

getOzoneQuality: DH.Coord -> Cmd Msg 
getOzoneQuality location = 
    RA.getOzoneQuality location
        |> RemoteData.sendRequest
        |> Cmd.map GetOzoneQualityResponse

getSO2Quality: DH.Coord -> Cmd Msg 
getSO2Quality location = 
    RA.getSO2Quality location 
        |> RemoteData.sendRequest
        |> Cmd.map GetSO2QualityResponse

getNO2Quality: DH.Coord -> Cmd Msg 
getNO2Quality location = 
    RA.getNO2Quality location 
        |> RemoteData.sendRequest
        |> Cmd.map GetNO2QualityResponse

getCurrentWeather: Model -> Cmd Msg 
getCurrentWeather model = 
    RW.currentWeatherRequest (buildContext model)
        |> RemoteData.sendRequest
        |> Cmd.map GetCurrentWeatherResponse

-- forecast
getForecast: Model -> Cmd Msg 
getForecast model = 
    RF.forecastRequest (buildContext model)
        |> RemoteData.sendRequest
        |> Cmd.map GetForecastResponse

-- air quality


-- UPDATE
update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        GetForecasts ->
            -- TODO:
            (model, Cmd.none)
        GetCurrentWeatherResponse response ->
            -- now have a valid location to use in getting airquality
            let
                cmd = 
                    case response of 
                        RemoteData.Success weather ->
                            Cmd.batch 
                                [ (getCoQuality weather.coord)
                                , (getOzoneQuality weather.coord)
                                , (getSO2Quality weather.coord)
                                , (getNO2Quality weather.coord)
                                ]
                        _-> Cmd.none
            in
                
            ({model | currentWeather = response }
            , cmd )

        GetForecastResponse response ->
            ({ model | fiveDayForecast = response}, Cmd.none)
        
        GetCOQualityResponse response ->
            ({ model | coQuality = response}, Cmd.none)
        GetOzoneQualityResponse response ->
            ({ model | ozoneQuality = response}, Cmd.none)
        GetSO2QualityResponse response ->
            ({ model | so2Quality = response}, Cmd.none)
        GetNO2QualityResponse response ->
            ({ model | no2Quality = response}, Cmd.none)
        SetCity city ->
            let
                originalForm = model.locationForm 
                updatedForm = { originalForm | city = Just city }

            in
            ({ model | locationForm = updatedForm} , Cmd.none)
        SetCountry country ->
            let
                originalForm = model.locationForm 
                updatedForm = { originalForm | country = Just country }

            in
            ({ model | locationForm = updatedForm} , Cmd.none)

        UpdateLocation ->
            -- reinitialize for new location
            case (model.locationForm.city, model.locationForm.country) of 
                (Just city, Just country) ->
                    initialize city country model.unit
                (_, _) ->
                    (model, Cmd.none) 
        
        UpdateUnit unit ->
            -- reinitialize for update of unit
                initialize model.city model.country unit

-- VIEW
locationView: LocationForm -> Html Msg 
locationView locationForm = 
    Html.form [onSubmit UpdateLocation]
        [ fieldset []
            [ input 
                [ placeholder "City"
                , onInput SetCity
                , value (locationForm.city |> Maybe.withDefault "")
                ]
                []
            , input 
                [ placeholder "Country"
                , onInput SetCountry
                , value (locationForm.country |> Maybe.withDefault "")
                ] []
            , button []
                [text "Update Location"]
            ]
        ]

{-| Display and update current unit choice
-}
viewUnitChoices: List (String, msg, Bool) -> Html msg 
viewUnitChoices choices = 
    fieldset [] (List.map radioInput choices)

radioInput: (String, msg, Bool) -> Html msg 
radioInput (name, msg, selected) = 
    label [] 
        [ input [type_ "radio", onClick msg, checked selected] []
        , text name        
        ]

viewUnit : DH.Unit -> Html Msg 
viewUnit unit = 
    div [] 
        [ viewUnitChoices 
            [ ("Celsius", UpdateUnit DH.Celsius, unit == DH.Celsius  )
            , ("Fahrenheit", UpdateUnit DH.Fahrenheit, unit == DH.Fahrenheit)
            ]        
        ]

view: Model -> Html Msg 
view model =     
    Grid.container
        []
        [ CDN.stylesheet
        , Grid.row [] [Grid.col [Col.xs12][ h2 [] [text "Elm weather dashboard"] ]]        
        , Grid.row []
            [ Grid.col [Col.xs6]
                [ model.locationForm 
                    |> locationView
                ]
            , Grid.col [Col.xs4] 
                [ model.unit
                    |> viewUnit                    
                ]
            , Grid.col [][]
            ]
        , Grid.row []
            [ Grid.col [Col.xs4]
                [ model.currentWeather
                    |> webDataView (WeatherView.view model.city model.country model.unit)
                ]
            , Grid.col [Col.xs8]
                [ model.fiveDayForecast
                    |> webDataView (ForecastView.view model.city model.country model.unit)   
                ]
            ]
        ]

main : Program Never Model Msg 
main = 
    Html.program 
        { init = init 
        , view = view 
        , update = update 
        , subscriptions = \_ -> Sub.none -- will probably need for alert
        }

