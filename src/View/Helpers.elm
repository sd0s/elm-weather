module View.Helpers exposing (..)

import Data.Helpers as DataHelper
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (..)


viewTemperature : DataHelper.Unit -> Float -> String
viewTemperature unit temp =
    let
        strUnit =
            if unit == DataHelper.Celsius then
                "°C"
            else
                "°F"
    in
    toString temp ++ strUnit


loader : Html msg
loader =
    div [ class "loader" ] []


networkError : String -> a -> Html msg
networkError message err =
    let
        x =
            Debug.log "ERR: " err
    in
    -- Just clear the loader for now
    text ""


webDataView : (a -> Html msg) -> WebData a -> Html msg
webDataView view data =
    case data of
        NotAsked ->
            text ""

        Loading ->
            loader

        Failure err ->
            networkError "There was an error communicating with the server." err

        Success val ->
            view val


empty : Html msg
empty =
    text ""
