module Request.Trigger exposing (..)

import Data.Trigger as DT
import Http
import Request.Helpers as RH


getTriggers : Http.Request (List DT.Trigger)
getTriggers =
    Http.get RH.triggerUrl DT.getTriggersDecoder


createTrigger : DT.CreateTrigger -> Http.Request DT.Trigger
createTrigger trigger =
    let
        body =
            trigger
                |> DT.createTriggerEncoder
                |> Http.jsonBody
    in
    Http.post RH.triggerUrl body DT.triggerDecoder

deleteTrigger: String -> Http.Request String
deleteTrigger triggerId = 
    Http.request 
        { method = "DELETE"
        , headers = []
        , url = RH.triggerUrl2 ("/" ++ triggerId) 
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing 
        , withCredentials = False
        }
    
