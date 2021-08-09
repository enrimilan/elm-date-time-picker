module TimePicker exposing (Message, State, Time, TimePeriod(..), configure, init, update)

import Delay exposing (TimeUnit(..))
import Element exposing (Color, Element, alignRight, centerX, centerY, column, el, fill, height, html, htmlAttribute, inFront, none, paddingEach, px, rgb255, rgba255, row, spacing, text, toRgb, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Keyed
import Hatchinq.Attribute as HatchinqAttr exposing (Attribute, toHeight, toId, toWidth, withAttributes)
import Hatchinq.Button as Button
import Hatchinq.IconButton as IconButton exposing (withTextColor)
import Hatchinq.TextField as TextField exposing (withError)
import Hatchinq.Theme exposing (Theme)
import Html
import Html.Attributes exposing (style)
import Html.Events exposing (custom, on)
import Json.Decode as Decode exposing (Decoder, at, float, int, map4)
import List exposing (range)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task



-- TYPES


type alias Config msg =
    { theme : Theme
    , lift : Message msg -> msg
    }


type alias State =
    { timeStringValue : Maybe String
    , timeStringState : TextField.State String
    , showDialog : Bool
    , tmpTime : Time
    , clockView : ClockView
    , mousePosition : MousePosition
    , mouseOverClock : Bool
    , animating : Bool
    , isDragging : Bool
    }


type alias Time =
    { hour : Hour, minute : Minute, period : TimePeriod }


type alias Hour =
    Int


type alias Minute =
    Int


type TimePeriod
    = AM
    | PM


type ClockView
    = HoursView
    | MinutesView


type alias MousePosition =
    { offsetX : Int
    , offsetY : Int
    , offsetHeight : Float
    , offsetWidth : Float
    }


type alias Point =
    { x : Float, y : Float }


type alias InternalConfig =
    {}


configure : Config msg -> (List (Attribute InternalConfig) -> View msg -> ( Element msg, Element.Attribute msg ))
configure config =
    view config


init : State
init =
    { timeStringValue = Nothing
    , timeStringState = TextField.init
    , showDialog = False
    , tmpTime = { hour = 12, minute = 0, period = AM }
    , clockView = HoursView
    , mousePosition = { offsetX = 0, offsetY = 0, offsetHeight = 0, offsetWidth = 0 }
    , mouseOverClock = False
    , animating = False
    , isDragging = False
    }



-- MESSAGES


type Message msg
    = TimeStringValueChange String (TimeChangedMessage msg)
    | TimeStringStateChange (TextField.Message (Message msg) String)
    | OpenDialog Time
    | CloseDialog
    | SwitchView ClockView
    | SetTimePeriod TimePeriod
    | MouseOverClock Bool
    | StartDrag MousePosition
    | MouseMove MousePosition
    | EndDrag
    | FinishAnimation
    | SetTime Time (TimeChangedMessage msg)
    | NoOp


type alias TimeChangedMessage msg =
    Time -> msg



-- UPDATE


update : Message msg -> Config msg -> State -> ( State, Cmd msg )
update message { lift } state =
    case message of
        TimeStringValueChange newTimeString timeChanged ->
            let
                newState =
                    { state | timeStringValue = Just newTimeString }
            in
            case parseTime newTimeString of
                Just newTime ->
                    if isValidTime newTime then
                        ( { newState | tmpTime = newTime }, msgToCmd <| timeChanged newTime )

                    else
                        ( newState, Cmd.none )

                _ ->
                    ( newState, Cmd.none )

        TimeStringStateChange timeStringChangeMessage ->
            let
                ( timeStringState, cmd ) =
                    TextField.update timeStringChangeMessage state.timeStringState
            in
            ( { state | timeStringState = timeStringState }, Cmd.map lift cmd )

        OpenDialog time ->
            let
                tmpTime =
                    if isValidTime time then
                        time

                    else
                        state.tmpTime
            in
            ( { state | tmpTime = tmpTime, showDialog = True, clockView = HoursView }, Cmd.none )

        CloseDialog ->
            ( { state | showDialog = False }, Cmd.none )

        SwitchView v ->
            ( { state | clockView = v, animating = True }
            , Cmd.map lift <| Delay.after animationDurationSeconds Second FinishAnimation
            )

        SetTimePeriod period ->
            let
                tmpTime =
                    state.tmpTime
            in
            ( { state | tmpTime = { tmpTime | period = period } }, Cmd.none )

        MouseOverClock mouseOverClock ->
            ( { state | mouseOverClock = mouseOverClock }, Cmd.none )

        StartDrag mousePosition ->
            let
                newState =
                    handleMouseMove state mousePosition
            in
            ( { newState | isDragging = True }, Cmd.none )

        MouseMove mousePosition ->
            if state.isDragging then
                ( handleMouseMove state mousePosition, Cmd.none )

            else
                ( state, Cmd.none )

        EndDrag ->
            if state.mouseOverClock && state.clockView == HoursView then
                ( { state | isDragging = False, clockView = MinutesView, animating = True }
                , Cmd.map lift <| Delay.after animationDurationSeconds Second FinishAnimation
                )

            else
                ( { state | isDragging = False }, Cmd.none )

        FinishAnimation ->
            ( { state | animating = False }, Cmd.none )

        SetTime time timeChanged ->
            if isValidTime time then
                ( { state | tmpTime = time, timeStringValue = Just <| timeToString time, showDialog = False }
                , msgToCmd <| timeChanged time
                )

            else
                ( state, Cmd.none )

        NoOp ->
            ( state, Cmd.none )


handleMouseMove : State -> MousePosition -> State
handleMouseMove state mousePosition =
    let
        newTmpTime =
            { hour =
                if state.clockView == HoursView then
                    Maybe.withDefault
                        state.tmpTime.hour
                        (shortestDistanceHour (Point (toFloat mousePosition.offsetX) (toFloat mousePosition.offsetY)))

                else
                    state.tmpTime.hour
            , minute =
                if state.clockView == MinutesView then
                    Maybe.withDefault
                        state.tmpTime.minute
                        (shortestDistanceMinute (Point (toFloat mousePosition.offsetX) (toFloat mousePosition.offsetY)))

                else
                    state.tmpTime.minute
            , period = state.tmpTime.period
            }
    in
    { state | mousePosition = mousePosition, tmpTime = newTmpTime }


shortestDistanceHour : Point -> Maybe Hour
shortestDistanceHour p =
    let
        compareHourDistance ( _, d1 ) ( _, d2 ) =
            compare d1 d2

        distances =
            List.map (\( hour, _ ) -> hour) <| List.sortWith compareHourDistance <| List.map (distance p) hourPositions
    in
    List.head distances


shortestDistanceMinute : Point -> Maybe Minute
shortestDistanceMinute p =
    let
        compareMinuteDistance ( _, d1 ) ( _, d2 ) =
            compare d1 d2

        distances =
            List.map (\( minute, _ ) -> minute) <| List.sortWith compareMinuteDistance <| List.map (distance p) minutePositions
    in
    List.head distances


distance : Point -> ( Hour, Point ) -> ( Hour, Float )
distance p1 ( hour, p2 ) =
    ( hour, sqrt ((p2.x - p1.x) ^ 2 + (p2.y - p1.y) ^ 2) )


isValidTime : Time -> Bool
isValidTime { hour, minute } =
    hour >= 1 && hour <= 12 && minute >= 0 && minute <= 59


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.succeed msg |> Task.perform identity



-- VIEW


type alias View msg =
    { label : String
    , state : State
    , time : Time
    , timeChanged : TimeChangedMessage msg
    }


view : Config msg -> List (Attribute InternalConfig) -> View msg -> ( Element msg, Element.Attribute msg )
view { theme, lift } attributes { label, state, time, timeChanged } =
    let
        textField =
            TextField.configure { theme = theme, lift = TimeStringStateChange }

        iconButton =
            IconButton.configure { theme = theme }

        timeStringValue =
            Maybe.withDefault (timeToString time) state.timeStringValue

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
                    if parseTime timeStringValue /= Nothing then
                        Nothing

                    else
                        Just "Invalid Time Format"
                }
            , HatchinqAttr.width <| Maybe.withDefault (px 280) (toWidth attributes)
            , HatchinqAttr.height <| Maybe.withDefault fill (toHeight attributes)
            ]
                ++ idAttribute

        showDialogButton =
            el
                [ alignRight, centerY, paddingEach { left = 0, right = 8, top = 0, bottom = 0 } ]
                (iconButton [withTextColor <| rgb255 45 45 45 ] { icon = "event", onPress = Just <| OpenDialog time })

        timePickerTextField =
            Element.map lift <|
                el [ inFront showDialogButton ] <|
                    textField
                        attrs
                        { id = "TimeInputField"
                        , label = label
                        , value = timeStringValue
                        , state = state.timeStringState
                        , onChange = Just (\newTimeString -> TimeStringValueChange newTimeString timeChanged)
                        , onKeyDown = Nothing
                        }

        timePickerDialog =
            if state.showDialog then
                Element.mapAttribute lift <|
                    inFront <|
                        el
                            [ onClick CloseDialog, width fill, height fill, Background.color <| rgba255 0 0 0 0.4 ]
                            (el [ centerX, centerY ] <| clockDialogView state theme timeChanged)

            else
                inFront none
    in
    ( timePickerTextField, timePickerDialog )


clockDialogView : State -> Theme -> TimeChangedMessage msg -> Element (Message msg)
clockDialogView state theme timeChanged =
    let
        robotoFont =
            Font.family
                [ Font.external
                    { name = "Roboto"
                    , url = "https://fonts.googleapis.com/css?family=Roboto"
                    }
                , Font.sansSerif
                ]

        button =
            Button.configure { theme = theme }

        textButton =
            button |> withAttributes [ Button.text ]
    in
    el
        [ robotoFont
        , Element.Events.onMouseUp EndDrag
        , Background.color white
        , htmlAttribute <| custom "click" (Decode.succeed { message = NoOp, stopPropagation = True, preventDefault = True })
        ]
    <|
        column
            [ spacing 32 ]
            [ controls state theme
            , if state.clockView == HoursView then
                Element.Keyed.el
                    [ width fill ]
                    ( "hours", analogClock state (List.map (drawHour state theme) hourPositions) theme )

              else
                Element.Keyed.el
                    [ width fill ]
                    ( "minutes", analogClock state (List.map (drawMinute state theme) minutePositions) theme )
            , row
                [ alignRight, paddingEach { top = 0, bottom = 8, left = 0, right = 8 } ]
                [ textButton [] { label = "CANCEL", onPress = Just CloseDialog }
                , textButton [] { label = "OK", onPress = Just <| SetTime state.tmpTime timeChanged }
                ]
            ]


controls : State -> Theme -> Element (Message msg)
controls state theme =
    let
        hourControl =
            el
                [ onClick (SwitchView HoursView)
                , Font.color <|
                    if state.clockView == HoursView then
                        white

                    else
                        inactiveColor
                , pointerCursor
                ]
            <|
                (text <| format state.tmpTime.hour)

        minuteControl =
            el
                [ onClick (SwitchView MinutesView)
                , Font.color <|
                    if state.clockView == MinutesView then
                        white

                    else
                        inactiveColor
                , pointerCursor
                ]
            <|
                (text <| format state.tmpTime.minute)

        periodControls =
            [ el
                [ onClick <| SetTimePeriod AM
                , Font.color <|
                    if state.tmpTime.period == AM then
                        white

                    else
                        inactiveColor
                , pointerCursor
                ]
              <|
                text "AM"
            , el
                [ onClick <| SetTimePeriod PM
                , Font.color <|
                    if state.tmpTime.period == PM then
                        white

                    else
                        inactiveColor
                , pointerCursor
                ]
              <|
                text "PM"
            ]
    in
    el [ Background.color theme.colors.primary.color, height <| px 100, width <| px 310 ] <|
        el [ paddingEach { left = 50, right = 24, top = 0, bottom = 0 }, centerY, alignRight ] <|
            el
                [ Font.color white
                , Font.size 60
                ]
            <|
                row
                    [ spacing 20 ]
                    [ row [] [ hourControl, el [ Font.color inactiveColor ] <| text ":", minuteControl ]
                    , column [ Font.size 18, spacing 16 ] periodControls
                    ]


analogClock : State -> List (Element.Attribute (Message msg)) -> Theme -> Element (Message msg)
analogClock state dial theme =
    let
        angle =
            case state.clockView of
                HoursView ->
                    state.tmpTime.hour * 30

                MinutesView ->
                    state.tmpTime.minute * 6

        maybePoint =
            List.head <|
                List.map (\( _, p ) -> p) <|
                    List.filter
                        (\( a, _ ) ->
                            a
                                == (if angle == 0 || angle == 360 then
                                        359

                                    else
                                        angle
                                   )
                        )
                        allPositions

        animationTime =
            String.fromFloat animationDurationSeconds ++ "s"

        ( x2Values, y2Values ) =
            valuesForAnimation state

        begin =
            "0s"

        handAnimations =
            [ Svg.animate
                [ SvgAttr.attributeType "xml"
                , SvgAttr.attributeName "x2"
                , SvgAttr.begin begin
                , SvgAttr.dur animationTime
                , SvgAttr.values <| String.join ";" <| List.map String.fromFloat x2Values
                , SvgAttr.calcMode "linear"
                , SvgAttr.repeatCount "1"
                ]
                []
            , Svg.animate
                [ SvgAttr.attributeType "xml"
                , SvgAttr.attributeName "y2"
                , SvgAttr.begin begin
                , SvgAttr.dur animationTime
                , SvgAttr.values <| String.join ";" <| List.map String.fromFloat y2Values
                , SvgAttr.calcMode "linear"
                , SvgAttr.repeatCount "1"
                ]
                []
            ]

        mousePositionDecoder =
            map4 MousePosition
                (at [ "offsetX" ] int)
                (at [ "offsetY" ] int)
                (at [ "target", "offsetHeight" ] float)
                (at [ "target", "offsetWidth" ] float)
    in
    el
        [ width <| px clockWidth
        , height <| px clockHeight
        , centerX
        , htmlAttribute <| style "border-radius" "50%"
        , Background.color <| rgba255 0 0 0 0.07
        , htmlAttribute <| on "mousemove" (Decode.map MouseMove mousePositionDecoder)
        , htmlAttribute <| on "mousedown" (Decode.map StartDrag mousePositionDecoder)
        , Element.Events.onMouseUp EndDrag
        , Element.Events.onMouseEnter <| MouseOverClock True
        , Element.Events.onMouseLeave <| MouseOverClock False
        ]
    <|
        el
            ([ width fill
             , height fill
             , inFront <|
                case maybePoint of
                    Just endPoint ->
                        html <|
                            Svg.svg
                                [ SvgAttr.style "pointer-events: none"
                                , SvgAttr.width <| String.fromInt clockWidth
                                , SvgAttr.height <| String.fromInt clockHeight
                                ]
                            <|
                                if state.animating then
                                    [ pin theme
                                    , hand endPoint handAnimations theme
                                    , Svg.circle
                                        [ SvgAttr.r "20"
                                        , SvgAttr.fill <| toRgbString theme.colors.primary.color
                                        , SvgAttr.cx <| String.fromFloat endPoint.x
                                        , SvgAttr.cy <| String.fromFloat endPoint.y
                                        ]
                                        [ Svg.animate
                                            [ SvgAttr.attributeType "xml"
                                            , SvgAttr.attributeName "cx"
                                            , SvgAttr.begin begin
                                            , SvgAttr.dur animationTime
                                            , SvgAttr.values <| String.join ";" <| List.map String.fromFloat x2Values
                                            , SvgAttr.calcMode "linear"
                                            , SvgAttr.repeatCount "1"
                                            ]
                                            []
                                        , Svg.animate
                                            [ SvgAttr.attributeType "xml"
                                            , SvgAttr.attributeName "cy"
                                            , SvgAttr.begin begin
                                            , SvgAttr.dur animationTime
                                            , SvgAttr.values <| String.join ";" <| List.map String.fromFloat y2Values
                                            , SvgAttr.calcMode "linear"
                                            , SvgAttr.repeatCount "1"
                                            ]
                                            []
                                        ]
                                    ]

                                else
                                    [ pin theme
                                    , hand endPoint [] theme
                                    ]

                    _ ->
                        none
             ]
                ++ dial
            )
            none


valuesForAnimation : State -> ( List Float, List Float )
valuesForAnimation state =
    let
        minuteAngle =
            (Tuple.first <| minutePosition state.tmpTime.minute) * 6

        hourAngle =
            (Tuple.first <| hourPosition state.tmpTime.hour) * 30

        angles =
            if state.clockView == MinutesView then
                if hourAngle > minuteAngle then
                    List.map pointPosition <| List.reverse <| List.range minuteAngle hourAngle

                else
                    List.map pointPosition <| List.range hourAngle minuteAngle

            else if hourAngle > minuteAngle then
                List.map pointPosition <| List.range minuteAngle hourAngle

            else
                List.map pointPosition <| List.reverse <| List.range hourAngle minuteAngle
    in
    ( List.map (\( _, p ) -> p.x) angles, List.map (\( _, p ) -> p.y) angles )


pin : Theme -> Svg (Message msg)
pin theme =
    Svg.circle
        [ SvgAttr.cx <| String.fromFloat clockCenter.x
        , SvgAttr.cy <| String.fromFloat clockCenter.y
        , SvgAttr.r "6"
        , SvgAttr.fill <| toRgbString theme.colors.primary.color
        ]
        []


hand : Point -> List (Svg (Message msg)) -> Theme -> Svg (Message msg)
hand end children theme =
    Svg.line
        [ SvgAttr.x1 <| String.fromFloat clockCenter.x
        , SvgAttr.x2 <| String.fromFloat end.x
        , SvgAttr.y1 <| String.fromFloat clockCenter.y
        , SvgAttr.y2 <| String.fromFloat end.y
        , SvgAttr.stroke <| toRgbString theme.colors.primary.color
        , SvgAttr.strokeWidth "2"
        ]
        children


drawHour : State -> Theme -> ( Hour, Point ) -> Element.Attribute (Message msg)
drawHour state theme ( hour, point ) =
    let
        ( circleFill, fontColor ) =
            if state.tmpTime.hour == hour && not state.animating then
                ( toRgbString theme.colors.primary.color, "white" )

            else
                ( "transparent", "black" )
    in
    inFront <| html <| thumbWithNumber point circleFill fontColor hour


drawMinute : State -> Theme -> ( Minute, Point ) -> Element.Attribute (Message msg)
drawMinute state theme ( minute, point ) =
    let
        ( circleFill, fontColor ) =
            if (state.tmpTime.minute == minute) && not state.animating then
                ( toRgbString theme.colors.primary.color, "white" )

            else
                ( "transparent", "black" )
    in
    inFront <|
        if modBy 5 minute == 0 then
            html <| thumbWithNumber point circleFill fontColor minute

        else if state.tmpTime.minute == minute && modBy 5 state.tmpTime.minute /= 0 && not state.animating then
            html <| thumbWithDot point theme

        else
            none


thumbWithNumber : Point -> String -> String -> Int -> Svg (Message msg)
thumbWithNumber position backgroundColor fontColor number =
    Svg.svg
        [ SvgAttr.style "pointer-events: none"
        , SvgAttr.width <| String.fromInt clockWidth
        , SvgAttr.height <| String.fromInt clockHeight
        ]
        [ Svg.g
            []
            [ Svg.circle
                [ SvgAttr.cx <| String.fromFloat position.x
                , SvgAttr.cy <| String.fromFloat position.y
                , SvgAttr.r "20"
                , SvgAttr.fill backgroundColor
                ]
                []
            , Svg.foreignObject
                [ SvgAttr.x <| String.fromFloat (position.x - 20)
                , SvgAttr.y <| String.fromFloat (position.y - 20)
                , SvgAttr.width <| String.fromInt 40 ++ "px"
                , SvgAttr.height <| String.fromInt 40 ++ "px"
                ]
                [ Html.div
                    [ Html.Attributes.style "height" <| String.fromInt 40 ++ "px"
                    , Html.Attributes.style "width" <| String.fromInt 40 ++ "px"
                    , Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "justify-content" "center"
                    , Html.Attributes.style "align-items" "center"
                    , Html.Attributes.style "margin" "0 auto"
                    , Html.Attributes.style "color" fontColor
                    ]
                    [ Html.text <| format number
                    ]
                ]
            ]
        ]


thumbWithDot : Point -> Theme -> Svg (Message msg)
thumbWithDot position theme =
    Svg.svg
        [ SvgAttr.style "pointer-events: none"
        , SvgAttr.width <| String.fromInt clockWidth
        , SvgAttr.height <| String.fromInt clockHeight
        ]
        [ Svg.g
            []
            [ Svg.circle
                [ SvgAttr.cx <| String.fromFloat position.x
                , SvgAttr.cy <| String.fromFloat position.y
                , SvgAttr.r "20"
                , SvgAttr.fill <| toRgbString theme.colors.primary.color
                ]
                []
            , Svg.circle
                [ SvgAttr.cx <| String.fromFloat position.x
                , SvgAttr.cy <| String.fromFloat position.y
                , SvgAttr.r "2"
                , SvgAttr.fill "white"
                ]
                []
            ]
        ]


format : Int -> String
format hourOrMinute =
    if hourOrMinute < 10 then
        "0" ++ String.fromInt hourOrMinute

    else
        String.fromInt hourOrMinute


clockWidth : Int
clockWidth =
    260


clockHeight : Int
clockHeight =
    260


inactiveColor : Color
inactiveColor =
    rgba255 255 255 255 0.54


white : Color
white =
    rgb255 255 255 255


pointerCursor : Element.Attribute (Message msg)
pointerCursor =
    htmlAttribute <| style "cursor" "pointer"


hourPositions : List ( Hour, Point )
hourPositions =
    List.map hourPosition (range 1 12)


hourPosition : Hour -> ( Hour, Point )
hourPosition hour =
    let
        x =
            clockCenter.x + (radius * cos (degrees <| toFloat (hour * 30 + 270)))

        y =
            clockCenter.y + (radius * sin (degrees <| toFloat (hour * 30 + 270)))
    in
    ( hour, Point x y )


minutePositions : List ( Minute, Point )
minutePositions =
    List.map minutePosition (range 0 59)


minutePosition : Minute -> ( Minute, Point )
minutePosition minute =
    let
        x =
            clockCenter.x + (radius * cos (degrees <| toFloat (minute * 6 + 270)))

        y =
            clockCenter.y + (radius * sin (degrees <| toFloat (minute * 6 + 270)))
    in
    ( minute, Point x y )


allPositions : List ( Int, Point )
allPositions =
    List.map pointPosition (range 0 359)


pointPosition : Int -> ( Int, Point )
pointPosition pos =
    let
        x =
            clockCenter.x + (radius * cos (degrees <| toFloat (pos + 270)))

        y =
            clockCenter.y + (radius * sin (degrees <| toFloat (pos + 270)))
    in
    ( pos, Point x y )


clockCenter : Point
clockCenter =
    Point 130 130


radius =
    100


animationDurationSeconds : Float
animationDurationSeconds =
    0.2


timePeriodToString : TimePeriod -> String
timePeriodToString timePeriod =
    case timePeriod of
        AM ->
            "AM"

        PM ->
            "PM"


timePeriodFromString : String -> Maybe TimePeriod
timePeriodFromString timePeriodString =
    case timePeriodString of
        "AM" ->
            Just AM

        "PM" ->
            Just PM

        _ ->
            Nothing


timeToString : Time -> String
timeToString time =
    String.concat [ format time.hour, ":", format time.minute, " ", timePeriodToString time.period ]


parseTime : String -> Maybe Time
parseTime timeString =
    let
        ( h, m, p ) =
            case String.split ":" timeString of
                hourString :: rest ->
                    case String.split " " (String.concat rest) of
                        minuteString :: timePeriodString ->
                            if String.length hourString /= 2 || String.length minuteString /= 2 then
                                ( Nothing, Nothing, Nothing )

                            else
                                ( String.toInt hourString
                                , String.toInt minuteString
                                , timePeriodFromString (String.concat timePeriodString)
                                )

                        _ ->
                            ( Nothing, Nothing, Nothing )

                _ ->
                    ( Nothing, Nothing, Nothing )
    in
    case ( h, m, p ) of
        ( Just hour, Just minute, Just timePeriod ) ->
            if hour >= 1 && hour <= 12 && minute >= 0 && minute <= 59 then
                Just { hour = hour, minute = minute, period = timePeriod }

            else
                Nothing

        _ ->
            Nothing


toRgbString : Color -> String
toRgbString color =
    let
        rgb =
            toRgb color
    in
    String.concat
        [ "rgb("
        , String.fromInt <| round (rgb.red * 255)
        , ","
        , String.fromInt <| round (rgb.green * 255)
        , ","
        , String.fromInt <| round (rgb.blue * 255)
        , ")"
        ]
