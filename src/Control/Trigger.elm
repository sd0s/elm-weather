module Control.Trigger exposing (..)

{-| Container for creating and displaying list of triggers
-}

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Data.Helpers as DH
import Data.Trigger as Trigger exposing (Trigger)
import Date exposing (Date)
import DatePicker exposing (defaultSettings)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (..)
import Request.Trigger as RT
import Task
import Time exposing (Time, minute)
import View.Helpers exposing (..)


type alias TriggerForm =
    { condition : Trigger.ConditionType
    , expression : Trigger.ConditionExpressionType
    , amount : Maybe Float
    , fromDatePicker : DatePicker.DatePicker
    , toDatePicker : DatePicker.DatePicker
    , fromDate : Maybe Date
    , toDate : Maybe Date
    , fromTimeExpression : Trigger.TimeConditionExpressionType
    , toTimeExpression : Trigger.TimeConditionExpressionType
    }


type alias Model =
    { triggerEditor : TriggerForm
    , triggers : WebData (List Trigger)
    , currentTime : Maybe Time
    , createdTrigger : WebData Trigger
    , location : Maybe DH.Coord -- container sets on each location change
    }


type Msg
    = Create
    | GetTriggersResponse (WebData (List Trigger))
    | ChangeConditionType Trigger.ConditionType
    | ChangeConditionExpressionType Trigger.ConditionExpressionType
    | SetAmount String
    | SetFromDate DatePicker.Msg
    | SetToDate DatePicker.Msg
    | ChangeFromTimeConditionExpressionType Trigger.TimeConditionExpressionType
    | ChangeToTimeConditionExpressionType Trigger.TimeConditionExpressionType
    | CreateTriggerResponse (WebData Trigger)
    | OnGetCurrentTime Time
    | RefreshTriggers Time
    | DeleteTrigger String
    | DeleteTriggerResponse (WebData String)


setLocation : Maybe DH.Coord -> Model -> Model
setLocation coord model =
    { model | location = coord }


cmdGetCurrentTime : Cmd Msg
cmdGetCurrentTime =
    Time.now
        |> Task.perform OnGetCurrentTime


datePickerSettings : Maybe String -> DatePicker.Settings
datePickerSettings placeholder =
    let
        placeholderText =
            case placeholder of
                Nothing ->
                    ""

                Just placeholder_ ->
                    placeholder_
    in
    { defaultSettings
        | inputClassList =
            [ ( "form-control", True )
            ]
        , inputName = Just "date"
        , inputId = Just "datepicker"
        , placeholder = placeholderText
    }


pickerFromDateSettings =
    datePickerSettings <| Just "From Date"


pickerToDateSettings =
    datePickerSettings <| Just "To Date"


initTriggerForm : ( TriggerForm, Cmd Msg )
initTriggerForm =
    let
        ( fromDatePicker, fromDatePickerCmd ) =
            DatePicker.init

        ( toDatePicker, toDatePickerCmd ) =
            DatePicker.init
    in
    ( { condition = Trigger.Temp
      , expression = Trigger.GT
      , amount = Nothing
      , fromDatePicker = fromDatePicker
      , toDatePicker = toDatePicker
      , fromDate = Nothing
      , toDate = Nothing
      , fromTimeExpression = Trigger.After
      , toTimeExpression = Trigger.Before
      }
    , Cmd.batch
        [ Cmd.map SetFromDate fromDatePickerCmd
        , Cmd.map SetToDate toDatePickerCmd
        ]
    )


init : ( Model, Cmd Msg )
init =
    let
        triggerForm =
            initTriggerForm
    in
    ( { triggerEditor = Tuple.first initTriggerForm
      , triggers = NotAsked
      , currentTime = Nothing
      , createdTrigger = NotAsked
      , location = Nothing
      }
    , Cmd.batch
        [ Tuple.second initTriggerForm
        , getTriggers
        , cmdGetCurrentTime
        ]
    )



-- REQUESTS


getTriggers : Cmd Msg
getTriggers =
    RT.getTriggers
        |> RemoteData.sendRequest
        |> Cmd.map GetTriggersResponse


createTrigger : Trigger.CreateTrigger -> Cmd Msg
createTrigger trigger =
    RT.createTrigger trigger
        |> RemoteData.sendRequest
        |> Cmd.map CreateTriggerResponse

deleteTrigger: String -> Cmd Msg 
deleteTrigger triggerId = 
    RT.deleteTrigger triggerId 
        |> RemoteData.sendRequest 
        |> Cmd.map DeleteTriggerResponse

buildTimeCondition : Time -> Time -> Trigger.TimeCondition
buildTimeCondition targetTime currentTime =
    let
        target =
            targetTime |> Time.inMilliseconds

        current =
            currentTime |> Time.inMilliseconds

        ( amount, expression ) =
            if target == current then
                ( target, Trigger.Exact )
            else if target < current then
                ( current - target, Trigger.Before )
            else
                ( target - current, Trigger.After )
    in
    { amount = amount
    , expression = expression
    }


validateTrigger : Maybe DH.Coord -> Maybe Time -> TriggerForm -> Result String Trigger.CreateTrigger
validateTrigger maybeLocation maybeCurrentTime form =
    case ( form.amount, form.fromDate, form.toDate, maybeCurrentTime, maybeLocation ) of
        ( Just amount, Just fromDate, Just toDate, Just time, Just location ) ->
            let
                condition =
                    { name = form.condition
                    , expression = form.expression
                    , amount = amount
                    , id = Nothing
                    }

                fromTime =
                    fromDate |> Date.toTime

                toTime =
                    toDate |> Date.toTime

                startTimeCondition =
                    buildTimeCondition fromTime time

                endTimeCondition =
                    buildTimeCondition toTime time

                timePeriod =
                    { start = startTimeCondition
                    , end = endTimeCondition
                    }

                createTrigger =
                    { conditions = [ condition ]
                    , timePeriod = timePeriod
                    , area =
                        [ { id = Nothing
                          , areaType = Trigger.Point -- just hardcoding to current location
                          , coordinates = [ location ]
                          }
                        ]
                    }
            in
            Result.Ok createTrigger

        ( _, _, _, _, _ ) ->
            Result.Err "Invalid"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create ->
            -- get trigger from trigger form
            let
                validated =
                    validateTrigger model.location model.currentTime model.triggerEditor
            in
            case validated of
                Ok newTrigger ->
                    ( model, createTrigger newTrigger )

                _ ->
                    -- validation error
                    ( model, Cmd.none )

        GetTriggersResponse response ->
            ( { model | triggers = response }, Cmd.none )

        ChangeConditionType condition ->
            let
                editor =
                    model.triggerEditor
            in
            ( { model | triggerEditor = { editor | condition = condition } }, Cmd.none )

        ChangeConditionExpressionType expression ->
            let
                editor =
                    model.triggerEditor
            in
            ( { model | triggerEditor = { editor | expression = expression } }, Cmd.none )

        SetAmount strAmount ->
            let
                amount =
                    strAmount
                        |> String.toFloat
                        |> Result.toMaybe

                trigger =
                    model.triggerEditor

                updatedTrigger =
                    { trigger | amount = amount }
            in
            ( { model | triggerEditor = updatedTrigger }, Cmd.none )

        SetFromDate msg ->
            let
                ( updatedDatePicker, datePickerCmd, dateEvent ) =
                    DatePicker.update pickerFromDateSettings msg model.triggerEditor.fromDatePicker

                date =
                    case dateEvent of
                        DatePicker.Changed newDate ->
                            newDate

                        _ ->
                            model.triggerEditor.fromDate

                trigger =
                    model.triggerEditor

                updatedTrigger =
                    { trigger | fromDate = date, fromDatePicker = updatedDatePicker }
            in
            { model | triggerEditor = updatedTrigger }
                ! [ Cmd.map SetFromDate datePickerCmd ]

        SetToDate msg ->
            let
                ( updatedDatePicker, datePickerCmd, dateEvent ) =
                    DatePicker.update pickerToDateSettings msg model.triggerEditor.toDatePicker

                date =
                    case dateEvent of
                        DatePicker.Changed newDate ->
                            newDate

                        _ ->
                            model.triggerEditor.toDate

                trigger =
                    model.triggerEditor

                updatedTrigger =
                    { trigger | toDate = date, toDatePicker = updatedDatePicker }
            in
            { model | triggerEditor = updatedTrigger }
                ! [ Cmd.map SetToDate datePickerCmd ]

        ChangeFromTimeConditionExpressionType expression ->
            let
                editor =
                    model.triggerEditor
            in
            ( { model
                | triggerEditor = { editor | fromTimeExpression = expression }
              }
            , Cmd.none
            )

        ChangeToTimeConditionExpressionType expression ->
            let
                editor =
                    model.triggerEditor
            in
            ( { model
                | triggerEditor = { editor | toTimeExpression = expression }
              }
            , Cmd.none
            )

        OnGetCurrentTime time ->
            ( { model | currentTime = Just time }, Cmd.none )

        CreateTriggerResponse response ->
            ( { model | createdTrigger = response }, getTriggers )

        -- reload triggers
        RefreshTriggers newTime ->
            ( model, getTriggers )
        
        DeleteTrigger triggerId ->
            (model, deleteTrigger triggerId)

        DeleteTriggerResponse response ->
            let
                cmd = 
                    if RemoteData.isSuccess response then 
                        getTriggers
                    else 
                        Cmd.none 
            in
            (model, cmd)


-- SELECT CONDITION TYPE


targetConditionType : Decoder Trigger.ConditionType
targetConditionType =
    targetValue |> Decode.map optionValueToConditionType


conditionTypeOptions : List ( String, String )
conditionTypeOptions =
    [ ( "1", "Temp" )
    , ( "2", "Pressure" )
    , ( "3", "Humidity" )
    , ( "4", "Wind speed" )
    , ( "5", "Wind Direction" )
    , ( "6", "Clouds" )
    ]


optionValueToConditionType : String -> Trigger.ConditionType
optionValueToConditionType strVal =
    case strVal of
        "1" ->
            Trigger.Temp

        "2" ->
            Trigger.Pressure

        "3" ->
            Trigger.Humidity

        "4" ->
            Trigger.WindSpeed

        "5" ->
            Trigger.WindDirection

        "6" ->
            Trigger.Clouds

        _ ->
            Trigger.UnknownCondition


selectConditionType : Trigger.ConditionType -> Html Msg
selectConditionType current =
    Grid.row [] 
        [Grid.col [Col.xs6]
            [ label [] [ text "Condition Type: " ]
            ]
        , Grid.col [Col.xs6]
                [ select
                    [ on "change"
                        (Decode.map ChangeConditionType targetConditionType)
                    ]
                <|
                    List.map
                        (\condition ->
                            option
                                [ value (Tuple.first condition)
                                , selected <| (current == optionValueToConditionType (Tuple.first condition))
                                ]
                                [ text (Tuple.second condition) ]
                        )
                        conditionTypeOptions
                ]        
        ]

-- SELECT CONDITION EXPRESSION TYPE


conditionExpressionTypeOptions : List ( String, String )
conditionExpressionTypeOptions =
    [ ( "1", "Greater Than" )
    , ( "2", "Less Than" )
    , ( "3", "Greater Than Equal" )
    , ( "4", "Less Than Equal" )
    , ( "5", "Equal" )
    , ( "6", "Not Equal" )
    ]


optionValueToConditionExpressionType : String -> Trigger.ConditionExpressionType
optionValueToConditionExpressionType strVal =
    case strVal of
        "1" ->
            Trigger.GT

        "2" ->
            Trigger.LT

        "3" ->
            Trigger.GTE

        "4" ->
            Trigger.LTE

        "5" ->
            Trigger.EQ

        "6" ->
            Trigger.NE

        _ ->
            Trigger.UnknownConditionExpression


selectConditionExpressionType : Trigger.ConditionExpressionType -> Html Msg
selectConditionExpressionType current =
    Grid.row []
        [ Grid.col [Col.xs6 ]
            [ label [] [ text "Expression Type: " ]
            ]
        , Grid.col [Col.xs6]
            [ select
                [ on "change"
                    (Decode.map ChangeConditionExpressionType targetConditionExpressionType)
                ]
            <|
                List.map
                    (\expression ->
                        option
                            [ value (Tuple.first expression)
                            , selected <| (current == optionValueToConditionExpressionType (Tuple.first expression))
                            ]
                            [ text (Tuple.second expression) ]
                    )
                    conditionExpressionTypeOptions
            ]
        ]


targetConditionExpressionType : Decoder Trigger.ConditionExpressionType
targetConditionExpressionType =
    targetValue |> Decode.map optionValueToConditionExpressionType


viewConditionValue : TriggerForm -> Html Msg
viewConditionValue form =
    Grid.row []
        [ Grid.col [Col.xs6]
            [ label [] [ text "Amount: " ]
            ]
        , Grid.col [Col.xs6]
            [ input
                [ type_ "number"
                , onInput SetAmount
                , value
                    (form.amount
                        |> Maybe.withDefault 0
                        |> toString
                    )
                ]
                []
            ]
        ]

timeConditionExpressionTypeOptions : List ( String, String )
timeConditionExpressionTypeOptions =
    [ ( "1", "Exact" )
    , ( "2", "After" )
    , ( "3", "Before" )
    ]


optionValueToTimeConditionExpressionType : String -> Trigger.TimeConditionExpressionType
optionValueToTimeConditionExpressionType strVal =
    case strVal of
        "1" ->
            Trigger.Exact

        "2" ->
            Trigger.After

        "3" ->
            Trigger.Before

        _ ->
            Trigger.UnknownExpression


selectTimeExpressionType : Trigger.TimeConditionExpressionType -> (Trigger.TimeConditionExpressionType -> msg) -> Html msg
selectTimeExpressionType current callback =
    div []
        [ label [] [ text "Time Expression type: " ]
        , select
            [ on "change"
                (Decode.map callback targetTimeConditionExpressionType)
            ]
          <|
            List.map
                (\expression ->
                    option
                        [ value (Tuple.first expression)
                        , selected <| (current == optionValueToTimeConditionExpressionType (Tuple.first expression))
                        ]
                        [ text (Tuple.second expression) ]
                )
                timeConditionExpressionTypeOptions
        ]


targetTimeConditionExpressionType : Decoder Trigger.TimeConditionExpressionType
targetTimeConditionExpressionType =
    targetValue |> Decode.map optionValueToTimeConditionExpressionType


customDateForm : TriggerForm -> Html Msg
customDateForm editor =
    Grid.row []
        [ Grid.col []
            [ fromDateInput editor
            , toDateInput editor            
            ]
        ]


fromDateInput : TriggerForm -> Html Msg
fromDateInput { fromDatePicker, fromDate } =
    Grid.row []
        [ Grid.col [Col.xs6]
            [ label [] [ text "From Date:" ]
            ]
        , Grid.col [Col.xs6]
            [ DatePicker.view fromDate pickerFromDateSettings fromDatePicker
                |> Html.map SetFromDate
            ]
        ]


toDateInput : TriggerForm -> Html Msg
toDateInput { toDatePicker, toDate } =
    Grid.row [] 
        [ Grid.col [Col.xs6]
            [ label [] [ text "To Date: " ]
            ]
        , Grid.col [Col.xs6]
            [ DatePicker.view toDate pickerToDateSettings toDatePicker
                |> Html.map SetToDate
            ]
        ]

viewTriggerEditor : Model -> Html Msg
viewTriggerEditor model =
    Grid.row []
        [Grid.col []
            [ Grid.row [] 
                [Grid.col [Col.xs12] 
                    [ div [class "section-header" ] [ text "Create Trigger" ]]
                ]
            , Grid.row []
                [ Grid.col [Col.xs12]
                    [ selectConditionType model.triggerEditor.condition
                    , selectConditionExpressionType model.triggerEditor.expression
                    , viewConditionValue model.triggerEditor
                    , customDateForm model.triggerEditor
                    , createTriggerButton
                    ]            
                ]
            ]
        ]
        


createTriggerButton : Html Msg
createTriggerButton =
    Grid.row []
        [ Grid.col []
            [ button
                [ onClick Create ]
                [ text "Create" ]
            ]
        ]

deleteTriggerButton : String -> Html Msg 
deleteTriggerButton triggerId = 
    button
        [ onClick (DeleteTrigger triggerId) ]
        [ text "Delete" ]
    

triggerRow : Trigger -> Html Msg
triggerRow trigger =
    tr []
        [ td [] [ text (trigger.id |> Maybe.withDefault "") ]
        , td [] 
            [ trigger.id
                |> Maybe.map
                    (\id -> deleteTriggerButton id)
                |> Maybe.withDefault (text "")
            ]
        ]


viewTriggers : List Trigger -> Html Msg
viewTriggers triggers =
    div []
        [ div [ class "section-header" ] [ text "Triggers" ]
        , div []
            [ table []
                [ thead []
                    [ th [] [ text "ID" ]
                    , th [] []
                    ]
                , tbody []
                    (List.map triggerRow triggers)
                ]
            ]
        ]


alertRow : ( String, Trigger.Alert ) -> Html msg
alertRow ( key, alert ) =
    tr []
        [ td [] [ text (alert.id |> Maybe.withDefault "") ]
        ]


viewAlerts : List Trigger -> Html msg
viewAlerts triggers =
    -- gather all alerts from triggers
    let
        allAlerts =
            triggers
                |> List.map
                    (\trigger ->
                        trigger.alerts |> Dict.toList
                    )
                |> List.concat
    in
    div []
        [ div [ class "section-header" ] [ text "Alerts" ]
        , div []
            [ table []
                [ thead []
                    [ th [] [ text "Alert ID" ]
                    ]
                , tbody []
                    (List.map alertRow allAlerts)
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewTriggerEditor model
        , model.triggers |> webDataView viewTriggers
        , model.triggers |> webDataView viewAlerts
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.minute RefreshTriggers ]
