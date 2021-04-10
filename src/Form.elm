module Form exposing (..)

import Cart exposing (Product)
import Html exposing (Html, Attribute, input, div, text, button)
import Html.Attributes exposing (type_, step, placeholder, value)
import Html.Events exposing (onInput, onClick)
import AppCss exposing (errorMsg, inputTextStyle, inputTextStyleWrong)
import Html exposing (label)

type Msg
    = SubmitProduct (Maybe Product)
    | Name String
    | Price String
    | Quantity String


type alias Model =
  { product: Product
  , wrongMsg: Maybe Msg
  }
init : Model
init =
  Model initProduct Nothing

initProduct : Product
initProduct =
  Product "" 0.0 0

update : Msg -> Model -> Model
update msg ({product} as model) =
  case msg of
    Name name ->
      { model | product = { product | name = name } }

    SubmitProduct newProduct ->
      case newProduct of
        Nothing ->
          { model | wrongMsg = Just (SubmitProduct newProduct)}

        Just _ ->
          { model | product = initProduct, wrongMsg = Nothing }

    Price price ->
      case (String.toFloat price) of
        Just value ->
          { model | product = { product | price = value}, wrongMsg = Nothing }

        Nothing ->
          { model | wrongMsg = Just (Price price)}

    Quantity quantity ->
      case (String.toInt quantity) of
        Just value ->
          { model | product = { product | quantity = value }, wrongMsg = Nothing }

        Nothing ->
          { model | wrongMsg = Just (Quantity quantity)}

checkProduct : Product -> Bool
checkProduct product
  = product.name /= initProduct.name
  && product.price /= initProduct.price
  && product.quantity /= initProduct.quantity

maybeProduct : Product -> Maybe Product
maybeProduct product =
  if (checkProduct product) then
      Just product
  else
      Nothing

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.product.name Name []
    , viewInputFloat "Price" (String.fromFloat model.product.price) Price []
    , viewInput "number" "Quantity" (String.fromInt model.product.quantity) Quantity []
    , viewValidation model.wrongMsg
    , viewInputSubmit "Add product" (SubmitProduct (maybeProduct model.product))
    ]

inputStyle : Maybe Msg -> List (Attribute msg)
inputStyle msg =
  case msg of
    Nothing ->
      inputTextStyle
    Just _ ->
      inputTextStyleWrong

viewInput : String -> String -> String -> (String -> msg) -> List (Attribute msg) -> Html msg
viewInput t p v toMsg listAttr =
  div []
    [ label [] [text p]
    , input ([ type_ t, placeholder p, value v, onInput toMsg ] ++ listAttr) []
    ]

viewInputSubmit : String -> msg -> Html msg
viewInputSubmit v toMsg =
  button [ onClick toMsg ] [ text v ]


viewInputFloat: String -> String -> (String -> msg) -> List (Attribute msg)  -> Html msg
viewInputFloat p v toMsg listAttr =
  div []
    [ label [] [text p]
    , input ([ type_ "number", step "0.01", placeholder p, value v, onInput toMsg ] ++ listAttr) []
    ]


viewValidation : Maybe Msg -> Html msg
viewValidation msg =
  case msg of
    Nothing ->
      div [ ] [ ]

    Just wrongMsg ->
      viewErrorMsg wrongMsg


viewErrorMsg : Msg -> Html msg
viewErrorMsg msg =
  case msg of
    Price _ ->
      div [ errorMsg ] [ text "Price must containe only number" ]

    SubmitProduct _ ->
      div [ errorMsg ] [ text "All fields must be filled" ]

    Name _ ->
      div [ errorMsg ] [ text "Name must be filled" ]

    Quantity _ ->
      div [ errorMsg ] [ text "Quantity must containe only number" ]



