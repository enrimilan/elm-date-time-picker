module Utils exposing (iconButton, inactiveColor, msgToCmd, pointerCursor, robotoFont, stopPropagation)

import Element exposing (Color, htmlAttribute, rgba255)
import Element.Font as Font
import Hatchinq.IconButton as IconButton
import Html.Attributes exposing (style)
import Html.Events exposing (custom)
import Json.Decode exposing (succeed)
import Task


iconButton theme =
    IconButton.configure { theme = theme }


robotoFont : Element.Attribute msg
robotoFont =
    Font.family
        [ Font.external
            { name = "Roboto"
            , url = "https://fonts.googleapis.com/css?family=Roboto"
            }
        , Font.sansSerif
        ]


pointerCursor : Element.Attribute msg
pointerCursor =
    htmlAttribute <| style "cursor" "pointer"


stopPropagation : msg -> Element.Attribute msg
stopPropagation noOpMsg =
    htmlAttribute <| custom "click" (succeed { message = noOpMsg, stopPropagation = True, preventDefault = True })


inactiveColor : Color
inactiveColor =
    rgba255 255 255 255 0.54


msgToCmd : msg -> Cmd msg
msgToCmd msg =
    Task.succeed msg |> Task.perform identity
