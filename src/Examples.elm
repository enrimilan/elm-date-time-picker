module Examples exposing (Message(..), State, init, main, theme, timePicker, timePickerConfig, update, view)

import Browser
import Element exposing (Element, column, focusStyle, layoutWith, padding, spacing, text)
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
    }


init : {} -> ( State, Cmd Message )
init _ =
    ( { time = { hour = 1, minute = 37, period = PM }
      , timePickerState = TimePicker.init
      }
    , Cmd.none
    )



-- MESSAGES


type Message
    = TimePickerMessage (TimePicker.Message Message)
    | TimeChanged Time



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



-- VIEW


timePicker =
    TimePicker.configure timePickerConfig


timePickerConfig =
    { theme = theme
    , lift = TimePickerMessage
    }


theme : Theme
theme =
    let
        ( r, g, b ) =
            ( 33, 150, 243 )

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

        noOutline =
            focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }

        content =
            column
                [ spacing 16, padding 16 ]
                [ Theme.stylesheet theme
                , timePickerTextField
                , text <| Debug.toString state.time
                ]
    in
    layoutWith
        { options = [ noOutline ] }
        [ timePickerDialog ]
        content
