module Request.Weather exposing (..)

import Data.Weather exposing (..)
import Http
import Request.Helpers as RH


currentWeatherRequest : RH.ServiceContext -> Http.Request CurrentWeatherResponse
currentWeatherRequest ctx =
    Http.get (RH.owmUrl RH.Weather ctx) currentWeatherResponseDecoder
