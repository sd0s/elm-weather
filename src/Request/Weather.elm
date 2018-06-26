module Request.Weather exposing(..)

import Http
import Data.Weather exposing(..)
import Request.Helpers as RH

currentWeatherRequest: RH.ServiceContext -> Http.Request CurrentWeatherResponse
currentWeatherRequest ctx = 
    Http.get (RH.owmUrl RH.Weather ctx) currentWeatherResponseDecoder
