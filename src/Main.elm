module Main exposing (..)

import Browser
import Cart exposing (Product)
import Html exposing (Html, text)
import Http
import Json.Decode exposing (..)
import Html exposing (ul)
import Html exposing (li)
import Html exposing (button)
import Html.Events exposing (onClick)
import Html exposing (div)
import Debug exposing (toString)



productDecoder : Decoder Product
productDecoder =
  map3 Product
    (field "name" string)
    (field "price" float)
    (field "quantity" int)

productListDecoder : Decoder (List Product)
productListDecoder =
    list productDecoder

errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Fail Http.Error
  | Load
  | Succ Cart.Cart


init : () -> (Model, Cmd Msg)
init _ =
  ( Load
  , Http.get
      { url = "./products.json"
      , expect = Http.expectJson GotRes productListDecoder
      }
  )



-- UPDATE


type Msg
  = GotRes (Result Http.Error (List Product))
  | AddProduct Product
  | DeleteProduct Cart.ProductName
  | AddQuantity Cart.ProductName


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotRes result ->
      case result of
        Ok products ->
          (Succ (Cart.Cart products 0 |> Cart.calcTotal), Cmd.none)

        Err err ->
          (Fail err, Cmd.none)

    AddProduct product ->
      case model of
        Succ cart ->
          (Succ (Cart.addProduct cart product |> Cart.calcTotal ), Cmd.none)

        Fail _ ->
          Debug.todo "branch 'Fail _' not implemented"

        Load ->
          Debug.todo "branch 'Load' not implemented"

    DeleteProduct productName->
      case model of
        Succ cart ->
          (Succ (Cart.deleteProduct cart productName |> Cart.calcTotal), Cmd.none)

        Fail _ ->
          Debug.todo "branch 'Fail _' not implemented"

        Load ->
          Debug.todo "branch 'Load' not implemented"

    AddQuantity _ ->
      Debug.todo "branch 'AddQuantity _' not implemented"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Fail err ->
      text (errorToString err)

    Load ->
      text "Loading..."

    Succ cart ->
      elementCart cart

elementCart : Cart.Cart -> Html Msg
elementCart cart =
  div []
    [ ul [] (List.map elementProduct cart.products)
    , div [] [ text (String.fromFloat cart.total)]
    ]

elementProduct : Product -> Html Msg
elementProduct product =
  li [] [text (product.name ++ " price = " ++ String.fromFloat product.price)
    , button [ onClick (DeleteProduct product.name) ] [ text "delete"]
    ]

-- button [ onClick (DeleteProduct product.name) ] [ text "delete"]
