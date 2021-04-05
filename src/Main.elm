module Main exposing (..)

import Browser
import Html exposing (Html, text, ul, li)
import Http
import Json.Decode exposing (..)

type alias Product =
  { name : String
  , price : Float
  , quantity : Int
  }

productDecoder : Decoder Product
productDecoder =
  map3 Product
    (field "name" string)
    (field "price" float)
    (field "quantity" int)

productListDecoder : Decoder (List Product)
productListDecoder =
    list productDecoder



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
  = Fail
  | Load
  | Succ (List Product)


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


update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    GotRes result ->
      case result of
        Ok products ->
          (Succ products, Cmd.none)

        Err _ ->
          (Fail, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Fail ->
      text "I was unable to load cart"

    Load ->
      text "Loading..."

    Succ products ->
      ul [] (List.map elementProduct products)

elementProduct : Product -> Html msg
elementProduct product =
  li [] [ text product.name ]