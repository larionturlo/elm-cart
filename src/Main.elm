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

        Err err ->
          (Fail err, Cmd.none)



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

    Succ products ->
      ul [] (List.map elementProduct products)

elementProduct : Product -> Html msg
elementProduct product =
  li [] [ text product.name ]