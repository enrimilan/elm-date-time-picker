module Examples exposing (Message(..), State, init, main, theme, timePicker, timePickerConfig, update, view)

import Browser
import DatePicker exposing (DatePickerDate, maxYear, minYear)
import Element exposing (Element, centerX, column, el, fill, focusStyle, layoutWith, padding, row, spacing, text, width)
import Element.Font as Font
import Hatchinq.Color exposing (rgba)
import Hatchinq.Theme as Theme exposing (..)
import Html exposing (Html)
import TimePicker exposing (Time, TimePeriod(..))


main : Program {} State Message
main =
    Browser.element
        { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- STATE


type alias State =
    { time : Time
    , timePickerState : TimePicker.State
    , date : DatePickerDate
    , datePickerState : DatePicker.State
    }


init : {} -> ( State, Cmd Message )
init _ =
    ( { time = { hour = 1, minute = 37, period = PM }
      , timePickerState = TimePicker.init
      , date = { day = 23, month = 5, year = 2007 }
      , datePickerState = DatePicker.init
      }
    , Cmd.none
    )



-- MESSAGES


type Message
    = TimePickerMessage (TimePicker.Message Message)
    | TimeChanged Time
    | DatePickerMessage (DatePicker.Message Message)
    | DateChanged DatePickerDate



-- UPDATE


update : Message -> State -> ( State, Cmd Message )
update message state =
    case message of
        TimePickerMessage timePickerMessage ->
            let
                ( timePickerState, cmd ) =
                    TimePicker.update timePickerMessage timePickerConfig state.timePickerState
            in
            ( { state | timePickerState = timePickerState }, cmd )

        TimeChanged time ->
            ( { state | time = time }, Cmd.none )

        DatePickerMessage datePickerMessage ->
            let
                ( datePickerState, cmd ) =
                    DatePicker.update datePickerMessage datePickerConfig state.datePickerState
            in
            ( { state | datePickerState = datePickerState }, cmd )

        DateChanged date ->
            ( { state | date = date }, Cmd.none )



-- VIEW


timePicker =
    TimePicker.configure timePickerConfig


timePickerConfig =
    { theme = theme
    , lift = TimePickerMessage
    }


datePicker =
    DatePicker.configure datePickerConfig


datePickerConfig =
    { theme = theme
    , lift = DatePickerMessage
    }


theme : Theme
theme =
    let
        ( r, g, b ) =
            ( 25, 118, 210 )

        a =
            1
    in
    Theme.withColors (rgba r g b a) (rgba r g b a) Theme.default


view : State -> Html Message
view state =
    let
        ( timePickerTextField, timePickerDialog ) =
            timePicker
                []
                { label = "Time"
                , state = state.timePickerState
                , time = state.time
                , timeChanged = TimeChanged
                }

        ( datePickerTextField, datePickerDialog ) =
            datePicker
                [ minYear 1900, maxYear 2100 ]
                { label = "Date"
                , state = state.datePickerState
                , date = state.date
                , dateChanged = DateChanged
                }

        noOutline =
            focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }

        content =
            column
                [ spacing 16, padding 16, width fill ]
                [ Theme.stylesheet theme
                , el [ Font.size 24 ] <| text "ELM Material Date / Time pickers"
                , row [ spacing 8, centerX ] [ datePickerTextField, text <| Debug.toString state.date ]
                , row [ spacing 8, centerX ] [ timePickerTextField, text <| Debug.toString state.time ]
                ]
    in
    layoutWith
        { options = [ noOutline ] }
        [ timePickerDialog, datePickerDialog ]
        content
