module AppCss exposing (..)

import Html.Attributes exposing (style)
import Html exposing (Attribute)


inputTextStyle : List (Attribute msg)
inputTextStyle =
  [ style "height" "90px"
    , style "width" "100%"
  ]

inputTextStyleWrong : List (Attribute msg)
inputTextStyleWrong =
  style "background-color" "red" :: inputTextStyle

errorMsg : Attribute msg
errorMsg =
  style "color" "red"

padding10 : Attribute msg
padding10 = style "padding" "0 10px"