module Request.Forecast exposing (..)

import Data.Forecast exposing (..)
import Http
import Request.Helpers as RH


forecastRequest : RH.ServiceContext -> Http.Request ForecastResponse
forecastRequest ctx =
    Http.get (RH.owmUrl RH.Forecast ctx) forecastResponseDecoder
