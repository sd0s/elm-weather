module Data.Trigger exposing (..)

import Data.Helpers exposing (..)
import Date exposing (..)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as Encode


type ConditionType
    = Temp
    | Pressure
    | Humidity
    | WindSpeed
    | WindDirection
    | Clouds
    | UnknownCondition


type ConditionExpressionType
    = GT
    | LT
    | GTE
    | LTE
    | EQ
    | NE
    | UnknownConditionExpression


type alias Condition =
    { name : ConditionType
    , expression : ConditionExpressionType
    , amount : Float
    , id : Maybe String
    }


type alias AlertData =
    { min : Float
    , max : Float
    }


type alias ConditionData =
    { currentValue : AlertData
    , condition : Condition
    }


type alias Alert =
    { id : Maybe String
    , conditions : List ConditionData
    , lastUpdate : Date
    , date : Date
    , coordinates : Coord
    }


type AreaType
    = Point
    | MultiPoint
    | Polygon
    | MultiPolygon
    | UnknownArea


type alias Area =
    { id : Maybe String -- How does this get set?
    , areaType : AreaType
    , coordinates : List Coord
    }


type TimeConditionExpressionType
    = Exact
    | After
    | Before
    | UnknownExpression


type alias TimeCondition =
    { amount : Float -- number of milliseconds
    , expression : TimeConditionExpressionType
    }


type alias TimePeriod =
    { start : TimeCondition
    , end : TimeCondition
    }


type alias Trigger =
    { id : Maybe String --
    , alerts : Dict String Alert
    , area : List Area
    , conditions : List Condition
    , timePeriod : TimePeriod
    }


type alias CreateTrigger =
    { conditions : List Condition
    , timePeriod : TimePeriod
    , area : List Area
    }


stringToTimeConditionExpressionType : String -> TimeConditionExpressionType
stringToTimeConditionExpressionType strExpression =
    case strExpression of
        "exact" ->
            Exact

        "after" ->
            After

        "before" ->
            Before

        _ ->
            UnknownExpression


timeConditionDecoder : Decoder TimeCondition
timeConditionDecoder =
    decode TimeCondition
        |> required "amount" Decode.float
        |> required "expression"
            (Decode.string
                |> Decode.map stringToTimeConditionExpressionType
            )


timePeriodDecoder : Decoder TimePeriod
timePeriodDecoder =
    decode TimePeriod
        |> required "start" timeConditionDecoder
        |> required "end" timeConditionDecoder


mapStringToAreaType : String -> AreaType
mapStringToAreaType strArea =
    case strArea of
        "Point" ->
            Point

        "MultiPoint" ->
            MultiPoint

        "polygon" ->
            Polygon

        "MultiPolygon" ->
            MultiPolygon

        _ ->
            UnknownArea


areaCoordDecoder : Decoder (List Coord)
areaCoordDecoder =
    Decode.list Decode.float
        |> Decode.map
            (\points ->
                let
                    ( lon, lat ) =
                        case points of
                            h :: t ->
                                case t of
                                    h2 :: t2 ->
                                        ( h, h2 )

                                    _ ->
                                        ( h, 0 )

                            _ ->
                                ( 0, 0 )
                in
                [ { lon = lon
                  , lat = lat
                  }
                ]
            )


areaDecoder : Decoder Area
areaDecoder =
    decode Area
        |> optional "_id" (Decode.nullable Decode.string) Nothing
        |> required "type"
            (Decode.string
                |> Decode.map mapStringToAreaType
            )
        |> required "coordinates" areaCoordDecoder



-- punt for the point area for now
-- Decode.oneOf
--     [ areaCoordDecoder
--     , (Decode.list areaCoordDecoder)
--     ]


alertDataDecoder : Decoder AlertData
alertDataDecoder =
    decode AlertData
        |> required "min" Decode.float
        |> required "max" Decode.float


mapStringToConditionType : String -> ConditionType
mapStringToConditionType strCondition =
    case String.toLower strCondition of
        "temp" ->
            Temp

        "pressure" ->
            Pressure

        "humidity" ->
            Humidity

        "wind_speed" ->
            WindSpeed

        "wind_direction" ->
            WindDirection

        "clouds" ->
            Clouds

        _ ->
            UnknownCondition


mapStringToConditionExpressionType : String -> ConditionExpressionType
mapStringToConditionExpressionType strExpression =
    case String.toLower strExpression of
        "$gt" ->
            GT

        "$lt" ->
            LT

        "$gte" ->
            GTE

        "$lte" ->
            LTE

        "$eq" ->
            EQ

        "$ne" ->
            NE

        _ ->
            UnknownConditionExpression


conditionDecoder : Decoder Condition
conditionDecoder =
    decode Condition
        |> required "name"
            (Decode.string
                |> Decode.map mapStringToConditionType
            )
        |> required "expression"
            (Decode.string
                |> Decode.map mapStringToConditionExpressionType
            )
        |> required "amount" Decode.float
        |> optional "_id" (Decode.nullable Decode.string) Nothing


conditionDataDecoder : Decoder ConditionData
conditionDataDecoder =
    decode ConditionData
        |> required "current_value" alertDataDecoder
        |> required "condition" conditionDecoder


alertDecoder : Decoder Alert
alertDecoder =
    decode Alert
        |> hardcoded Nothing
        -- id set with alerts container decoding
        |> required "conditions" (Decode.list conditionDataDecoder)
        |> required "last_update" posixDateDecoder
        |> required "date" posixDateDecoder
        |> required "coordinates" coordDecoder


triggerDecoder : Decoder Trigger
triggerDecoder =
    decode Trigger
        |> optional "_id" (Decode.nullable Decode.string) Nothing
        |> required "alerts"
            (Decode.dict alertDecoder
                |> Decode.map
                    (\theDict ->
                        theDict
                            |> Dict.map
                                (\key val ->
                                    { val | id = Just key }
                                )
                    )
            )
        |> required "area" (Decode.list areaDecoder)
        |> required "conditions" (Decode.list conditionDecoder)
        |> required "time_period" timePeriodDecoder


getTriggersDecoder : Decoder (List Trigger)
getTriggersDecoder =
    Decode.list triggerDecoder


mapTimeConditionExpressionToString : TimeConditionExpressionType -> String
mapTimeConditionExpressionToString condition =
    case condition of
        Exact ->
            "exact"

        After ->
            "after"

        Before ->
            "before"

        _ ->
            "unknown"


timeConditionExpressionEncoder : TimeConditionExpressionType -> Encode.Value
timeConditionExpressionEncoder expression =
    Encode.string (mapTimeConditionExpressionToString expression)


timeConditionEncoder : TimeCondition -> Encode.Value
timeConditionEncoder condition =
    Encode.object
        [ ( "expression", timeConditionExpressionEncoder condition.expression )
        , ( "amount", Encode.float condition.amount )
        ]


timePeriodEncoder : TimePeriod -> Encode.Value
timePeriodEncoder period =
    Encode.object
        [ ( "start", timeConditionEncoder period.start )
        , ( "end", timeConditionEncoder period.end )
        ]


mapConditionTypeToString : ConditionType -> String
mapConditionTypeToString condition =
    case condition of
        Temp ->
            "temp"

        Pressure ->
            "pressure"

        Humidity ->
            "humidity"

        WindSpeed ->
            "wind_speed"

        WindDirection ->
            "wind_direction"

        Clouds ->
            "clouds"

        _ ->
            "unknown"


conditionTypeEncoder : ConditionType -> Encode.Value
conditionTypeEncoder condition =
    Encode.string (mapConditionTypeToString condition)


conditionEncoder : Condition -> Encode.Value
conditionEncoder condition =
    Encode.object
        [ ( "name", conditionTypeEncoder condition.name )
        , ( "expression", conditionExpressionEncoder condition.expression )
        , ( "amount", Encode.float condition.amount )
        ]


mapConditionExpressionToString : ConditionExpressionType -> String
mapConditionExpressionToString condition =
    case condition of
        GT ->
            "$gt"

        LT ->
            "$lt"

        GTE ->
            "$gte"

        LTE ->
            "$lte"

        EQ ->
            "$eq"

        NE ->
            "$ne"

        _ ->
            "unknown"


conditionExpressionEncoder : ConditionExpressionType -> Encode.Value
conditionExpressionEncoder condition =
    Encode.string (mapConditionExpressionToString condition)


mapAreaTypeToString : AreaType -> String
mapAreaTypeToString areaType =
    case areaType of
        Point ->
            "Point"

        MultiPoint ->
            "MultiPoint"

        Polygon ->
            "Polygon"

        MultiPolygon ->
            "MultiPolygon"

        _ ->
            "Unknown"


areaTypeEncoder : AreaType -> Encode.Value
areaTypeEncoder areaType =
    Encode.string (mapAreaTypeToString areaType)


coordEncoder : Coord -> Encode.Value
coordEncoder coord =
    Encode.list
        [ Encode.float coord.lat
        , Encode.float coord.lon
        ]


areaEncoder : Area -> Encode.Value
areaEncoder area =
    Encode.object
        [ ( "type", areaTypeEncoder area.areaType )
        , ( "coordinates"
          , if List.length area.coordinates > 1 then
                Encode.list (List.map coordEncoder area.coordinates)
            else
                area.coordinates
                    |> List.head
                    |> Maybe.map
                        (\coord ->
                            coordEncoder coord
                        )
                    |> Maybe.withDefault (Encode.string "")
          )
        ]


createTriggerEncoder : CreateTrigger -> Encode.Value
createTriggerEncoder trigger =
    Encode.object
        [ ( "time_period", timePeriodEncoder trigger.timePeriod )
        , ( "conditions", Encode.list (List.map conditionEncoder trigger.conditions) )
        , ( "area", Encode.list (List.map areaEncoder trigger.area) )
        ]
