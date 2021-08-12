module DatePicker exposing (DatePickerDate, Message, State, configure, init, maxYear, minYear, update)

import Array
import Browser.Dom as Dom
import Date exposing (Date, Interval(..), Unit(..), fromCalendarDate, fromIsoString, numberToMonth)
import Element exposing (Color, Element, alignLeft, alignRight, centerX, centerY, column, el, fill, height, htmlAttribute, inFront, none, padding, paddingEach, px, rgb255, rgba255, row, scrollbars, spacing, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Keyed
import Hatchinq.Attribute as HatchinqAttr exposing (Attribute, custom, toHeight, toId, toInternalConfig, toWidth, withAttributes)
import Hatchinq.Button as Button
import Hatchinq.IconButton exposing (withTextColor)
import Hatchinq.TextField as TextField exposing (withError)
import Hatchinq.Theme exposing (Theme, white)
import Html.Attributes exposing (id, style)
import List
import Random
import Random.Char
import Random.String
import String exposing (padLeft)
import Task
import Utils exposing (iconButton, inactiveColor, msgToCmd, pointerCursor, robotoFont, stopPropagation)



-- TYPES


type alias Config msg =
    { theme : Theme
    , lift : Message msg -> msg
    }


type alias State =
    { dateStringValue : Maybe String
    , dateStringState : TextField.State String
    , showDialog : Bool
    , tmpDate : DatePickerDate
    , currentView : CurrentView
    , currentMonth : DatePickerMonth
    , yearsViewId : Maybe String
    }


type alias DatePickerMonth =
    { year : Int
    , month : Int
    , daysGrid : List (Maybe DatePickerDate)
    }


type alias DatePickerDate =
    { day : Int, month : Int, year : Int }


type CurrentView
    = CalendarView
    | YearsView


type alias InternalConfig =
    { minYear : Int
    , maxYear : Int
    }


minYear : Int -> Attribute InternalConfig
minYear year =
    custom (\v -> { v | minYear = max 0 year })


maxYear : Int -> Attribute InternalConfig
maxYear year =
    custom (\v -> { v | maxYear = min 9999 year })


configure : Config msg -> (List (Attribute InternalConfig) -> View msg -> ( Element msg, Element.Attribute msg ))
configure config =
    view config


init : State
init =
    { dateStringValue = Nothing
    , dateStringState = TextField.init
    , showDialog = False
    , tmpDate = defaultDate
    , currentView = CalendarView
    , currentMonth =
        { year = defaultDate.year, month = defaultDate.month, daysGrid = days defaultDate.year defaultDate.month }
    , yearsViewId = Nothing
    }


defaultDate : DatePickerDate
defaultDate =
    { day = 1, month = 1, year = 1970 }



-- MESSAGES


type Message msg
    = DateStringValueChange String InternalConfig (DateChangedMessage msg)
    | DateStringStateChange (TextField.Message (Message msg) String)
    | YearsViewIdGenerated String
    | OpenDialog DatePickerDate InternalConfig
    | CloseDialog
    | SwitchView CurrentView
    | NextMonth
    | PreviousMonth
    | SelectDate DatePickerDate
    | ChangeYear Int
    | SetDate DatePickerDate InternalConfig (DateChangedMessage msg)
    | NoOp


type alias DateChangedMessage msg =
    DatePickerDate -> msg



-- UPDATE


update : Message msg -> Config msg -> State -> ( State, Cmd msg )
update message { lift } state =
    case message of
        DateStringValueChange newDateString internalConfig dateChanged ->
            let
                newState =
                    { state | dateStringValue = Just newDateString }
            in
            case parseDate newDateString internalConfig of
                Just newDate ->
                    if isValidDate newDate internalConfig then
                        ( { newState | tmpDate = newDate }, msgToCmd <| dateChanged newDate )

                    else
                        ( newState, Cmd.none )

                _ ->
                    ( newState, Cmd.none )

        DateStringStateChange dateStringChangeMessage ->
            let
                ( dateStringState, cmd ) =
                    TextField.update dateStringChangeMessage state.dateStringState
            in
            ( { state | dateStringState = dateStringState }, Cmd.map lift cmd )

        YearsViewIdGenerated yearsViewId ->
            ( { state | yearsViewId = Just yearsViewId }, Cmd.none )

        OpenDialog date internalConfig ->
            let
                cmd =
                    if state.yearsViewId == Nothing then
                        Random.generate YearsViewIdGenerated (Random.String.string 16 Random.Char.english)

                    else
                        Cmd.none

                ( tmpDate, currentMonth ) =
                    if isValidDate date internalConfig then
                        ( date, { year = date.year, month = date.month, daysGrid = days date.year date.month } )

                    else
                        ( state.tmpDate, state.currentMonth )
            in
            ( { state | tmpDate = tmpDate, currentMonth = currentMonth, showDialog = True, currentView = CalendarView }
            , Cmd.map lift cmd
            )

        CloseDialog ->
            ( { state | showDialog = False }, Cmd.none )

        SwitchView v ->
            let
                cmd =
                    case ( state.yearsViewId, v, state.currentView ) of
                        ( Just yearsViewId, YearsView, CalendarView ) ->
                            jumpTo yearsViewId (String.fromInt state.tmpDate.year)

                        _ ->
                            Cmd.none
            in
            ( { state | currentView = v }, Cmd.map lift cmd )

        NextMonth ->
            let
                nextMonth =
                    Date.ceiling Month <| fromCalendarDate state.currentMonth.year (numberToMonth state.currentMonth.month) 2
            in
            ( { state | currentMonth = createDatePickerMonth nextMonth }, Cmd.none )

        PreviousMonth ->
            let
                previousMonth =
                    Date.add Days -1 <| fromCalendarDate state.currentMonth.year (numberToMonth state.currentMonth.month) 1
            in
            ( { state | currentMonth = createDatePickerMonth previousMonth }, Cmd.none )

        SelectDate date ->
            ( { state | tmpDate = date }, Cmd.none )

        ChangeYear year ->
            let
                currentMonth =
                    { year = year, month = state.currentMonth.month, daysGrid = state.currentMonth.daysGrid }

                tmpDate =
                    { day = state.tmpDate.day, month = state.tmpDate.month, year = year }
            in
            ( { state | currentMonth = currentMonth, tmpDate = tmpDate, currentView = CalendarView }, Cmd.none )

        SetDate date internalConfig dateChanged ->
            if isValidDate date internalConfig then
                ( { state | tmpDate = date, dateStringValue = Just <| dateToString date, showDialog = False }
                , msgToCmd <| dateChanged date
                )

            else
                ( state, Cmd.none )

        NoOp ->
            ( state, Cmd.none )


createDatePickerMonth : Date -> DatePickerMonth
createDatePickerMonth date =
    { year = Date.year date
    , month = Date.monthNumber date
    , daysGrid = days (Date.year date) (Date.monthNumber date)
    }


days : Int -> Int -> List (Maybe DatePickerDate)
days year month =
    let
        toDate date =
            { day = Date.day date, month = Date.monthNumber date, year = Date.year date }

        first =
            fromCalendarDate year (numberToMonth month) 1

        until =
            fromCalendarDate year (numberToMonth month) 31

        days_ =
            List.map (\d -> Just <| toDate d) <| Date.range Day 1 first until

        daysToDrop =
            if Date.weekdayNumber first == 7 then
                0

            else
                Date.weekdayNumber first
    in
    List.repeat daysToDrop Nothing ++ days_ ++ [ Just { day = List.length days_ + 1, month = month, year = year } ]


jumpTo : String -> String -> Cmd (Message msg)
jumpTo scrollViewId elementId =
    Dom.getElement scrollViewId
        |> Task.andThen (\scrollableView -> Task.sequence [ Task.succeed scrollableView, Dom.getElement elementId ])
        |> Task.andThen (\views -> Dom.setViewportOf scrollViewId 0 (calculatePosition views))
        |> Task.attempt (\_ -> NoOp)


calculatePosition views =
    case views of
        scrollableView :: yearView :: [] ->
            max 0 (yearView.element.y - scrollableView.element.y - 128)

        _ ->
            0



-- VIEW


type alias View msg =
    { label : String
    , state : State
    , date : DatePickerDate
    , dateChanged : DateChangedMessage msg
    }


view : Config msg -> List (Attribute InternalConfig) -> View msg -> ( Element msg, Element.Attribute msg )
view { theme, lift } attributes { label, state, date, dateChanged } =
    let
        defaultInternalConfig =
            { minYear = 1900
            , maxYear = 2100
            }

        internalConfig =
            toInternalConfig attributes defaultInternalConfig

        textField =
            TextField.configure { theme = theme, lift = DateStringStateChange }

        dateStringValue =
            Maybe.withDefault (dateToString date) state.dateStringValue

        idAttribute =
            case toId attributes of
                Just stringId ->
                    [ HatchinqAttr.id stringId ]

                Nothing ->
                    []

        attrs =
            [ withError
                { default = ""
                , error =
                    if parseDate dateStringValue internalConfig /= Nothing then
                        Nothing

                    else
                        Just "Invalid Date"
                }
            , HatchinqAttr.width <| Maybe.withDefault (px 280) (toWidth attributes)
            , HatchinqAttr.height <| Maybe.withDefault fill (toHeight attributes)
            ]
                ++ idAttribute

        showDialogButton =
            el
                [ alignRight, centerY, paddingEach { left = 0, right = 8, top = 0, bottom = 0 } ]
                (iconButton theme [ withTextColor iconColor ] { icon = "event", onPress = Just <| OpenDialog date internalConfig })

        datePickerTextField =
            Element.map lift <|
                el [ inFront showDialogButton ] <|
                    textField
                        attrs
                        { id = "DateInputField"
                        , label = label
                        , value = dateStringValue
                        , state = state.dateStringState
                        , onChange = Just (\newDateString -> DateStringValueChange newDateString internalConfig dateChanged)
                        , onKeyDown = Nothing
                        }

        datePickerDialog =
            if state.showDialog then
                Element.mapAttribute lift <|
                    inFront <|
                        el
                            [ onClick CloseDialog, width fill, height fill, Background.color <| rgba255 0 0 0 0.4 ]
                            (el [ centerX, centerY ] <| calendarDialogView state internalConfig theme dateChanged)

            else
                inFront none
    in
    ( datePickerTextField, datePickerDialog )


calendarDialogView : State -> InternalConfig -> Theme -> DateChangedMessage msg -> Element (Message msg)
calendarDialogView state internalConfig theme dateChanged =
    let
        button =
            Button.configure { theme = theme }

        textButton =
            button |> withAttributes [ Button.text ]
    in
    el
        [ robotoFont, Background.color white, stopPropagation NoOp, width fill ]
    <|
        column
            [ width fill ]
            [ controls state theme
            , if state.currentView == CalendarView then
                Element.Keyed.el [ width fill ] ( "calendar", calendarView state theme )

              else
                Element.Keyed.el [ width fill ] ( "year", yearsView state internalConfig theme )
            , row
                [ alignRight, paddingEach { top = 0, bottom = 8, left = 0, right = 8 } ]
                [ textButton [] { label = "CANCEL", onPress = Just CloseDialog }
                , textButton [] { label = "OK", onPress = Just <| SetDate state.tmpDate internalConfig dateChanged }
                ]
            ]


controls : State -> Theme -> Element (Message msg)
controls state theme =
    let
        internalTmpDate =
            fromCalendarDate state.tmpDate.year (numberToMonth state.tmpDate.month) state.tmpDate.day
    in
    el [ Background.color theme.colors.primary.color, height <| px 100, width <| px 310 ] <|
        el [ paddingEach { left = 24, right = 24, top = 0, bottom = 0 }, centerY ] <|
            column
                [ spacing 8 ]
                [ el
                    [ Font.color <|
                        if state.currentView == YearsView then
                            white

                        else
                            inactiveColor
                    , Font.size 16
                    , pointerCursor
                    , onClick <| SwitchView YearsView
                    ]
                    (text <| String.fromInt state.tmpDate.year)
                , el
                    [ Font.color <|
                        if state.currentView == CalendarView then
                            white

                        else
                            inactiveColor
                    , Font.size 34
                    , pointerCursor
                    , onClick <| SwitchView CalendarView
                    ]
                    (text <| Date.format "EEE, MMM d" internalTmpDate)
                ]


calendarView : State -> Theme -> Element (Message msg)
calendarView state theme =
    let
        currentMonthAndYearText =
            Date.format "MMMM y" <| fromCalendarDate state.currentMonth.year (numberToMonth state.currentMonth.month) 1

        weekDayView : String -> Element (Message msg)
        weekDayView weekDay =
            el [ Font.size 12, Font.color <| rgba255 0 0 0 0.38, width <| px 36 ] (el [ centerX ] <| text weekDay)

        daysGrid =
            Array.fromList state.currentMonth.daysGrid

        gridRow start end =
            row
                []
            <|
                List.map (dayView state theme) <|
                    List.map (\i -> Maybe.withDefault Nothing <| Array.get i daysGrid) <|
                        List.range start end
    in
    column
        [ padding 8, spacing 16, width fill, height <| px 300 ]
        [ row
            [ width fill ]
            [ el
                [ alignLeft, pointerCursor ]
                (iconButton theme [ withTextColor iconColor ] { icon = "navigate_before", onPress = Just PreviousMonth })
            , el [ centerX, Font.size 16, Font.color dateColor ] (text currentMonthAndYearText)
            , el
                [ alignRight, pointerCursor ]
                (iconButton theme [ withTextColor iconColor ] { icon = "navigate_next", onPress = Just NextMonth })
            ]
        , row [ centerX ] <| List.map weekDayView [ "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa" ]
        , column
            [ centerX ]
            [ gridRow 0 6, gridRow 7 13, gridRow 14 20, gridRow 21 27, gridRow 28 34, gridRow 35 41 ]
        ]


dayView : State -> Theme -> Maybe DatePickerDate -> Element (Message msg)
dayView state theme maybeDate =
    case maybeDate of
        Just date ->
            el
                [ Font.size 14
                , height <| px 36
                , width <| px 36
                , pointerCursor
                , htmlAttribute <| style "border-radius" "50%"
                , Background.color <|
                    if date == state.tmpDate then
                        theme.colors.primary.color

                    else
                        white
                , Font.color <|
                    if date == state.tmpDate then
                        white

                    else
                        dateColor
                , onClick <| SelectDate date
                ]
                (el [ centerX, centerY ] <| text <| String.fromInt date.day)

        Nothing ->
            el [ height <| px 36, width <| px 36 ] none


yearsView : State -> InternalConfig -> Theme -> Element (Message msg)
yearsView state internalConfig theme =
    let
        idAttribute =
            Maybe.withDefault [] <| Maybe.map (\yearsViewId -> [ htmlAttribute <| id yearsViewId ]) state.yearsViewId

        attributes =
            [ centerX, scrollbars, height <| px 300, width fill, paddingEach { left = 0, right = 0, top = 0, bottom = 4 } ]
    in
    column
        (attributes ++ idAttribute)
    <|
        List.map
            (\year ->
                el
                    [ pointerCursor
                    , width fill
                    , Font.size <|
                        if state.tmpDate.year == year then
                            24

                        else
                            16
                    , height <| px 40
                    , Font.color <|
                        if state.tmpDate.year == year then
                            theme.colors.primary.color

                        else
                            dateColor
                    , htmlAttribute <| id <| String.fromInt year
                    , onClick <| ChangeYear year
                    ]
                    (el [ centerY, centerX ] <| text <| String.fromInt year)
            )
            (List.range internalConfig.minYear internalConfig.maxYear)


iconColor : Color
iconColor =
    rgb255 45 45 45


dateColor : Color
dateColor =
    rgba255 0 0 0 0.87


dateToString : DatePickerDate -> String
dateToString date =
    String.join "."
        [ padLeft 2 '0' (String.fromInt date.day)
        , padLeft 2 '0' (String.fromInt date.month)
        , padLeft 4 '0' (String.fromInt date.year)
        ]


parseDate : String -> InternalConfig -> Maybe DatePickerDate
parseDate dateString internalConfig =
    let
        ( d, m, y ) =
            case String.split "." dateString of
                dayString :: monthString :: yearString :: [] ->
                    if String.length dayString /= 2 || String.length monthString /= 2 then
                        ( Nothing, Nothing, Nothing )

                    else
                        ( String.toInt dayString
                        , String.toInt monthString
                        , String.toInt yearString
                        )

                _ ->
                    ( Nothing, Nothing, Nothing )
    in
    case ( d, m, y ) of
        ( Just day, Just month, Just year ) ->
            if isValidDate { day = day, month = month, year = year } internalConfig then
                Just { day = day, month = month, year = year }

            else
                Nothing

        _ ->
            Nothing


isValidDate : DatePickerDate -> InternalConfig -> Bool
isValidDate date internalConfig =
    date.year >= internalConfig.minYear && date.year <= internalConfig.maxYear && existsDate date


existsDate : DatePickerDate -> Bool
existsDate date =
    let
        dateString =
            String.join
                "-"
                [ padLeft 4 '0' <| String.fromInt date.year
                , padLeft 2 '0' <| String.fromInt date.month
                , padLeft 2 '0' <| String.fromInt date.day
                ]
    in
    case fromIsoString dateString of
        Ok _ ->
            True

        _ ->
            False
