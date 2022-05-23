port module Main exposing (..)

import Browser
import Cart exposing (Product)
import Http
import Json.Decode exposing (..)
import Html exposing (Html, text, div, a, h1, hr, h2, p)
import Form
import Html.Attributes exposing (href)
import AppCss exposing (padding10)



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

-- PORTS

port getVersion : (String -> msg) -> Sub msg

-- MODEL


type Model
  = Fail Http.Error
  | Load
  | Succ Cart.Cart Form.Model String


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
  | CartMsg Cart.Msg
  | FormMsg Form.Msg
  | Version String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotRes result ->
      case result of
        Ok products ->
          (Succ (Cart.Cart products 0 |> Cart.calcTotal) Form.init "--", Cmd.none)

        Err err ->
          (Fail err, Cmd.none)

    CartMsg cartmsg->
      case model of
        Succ cart form v->
          (Succ (Cart.update cartmsg cart) form v, Cmd.none)

        Fail _ ->
          Debug.todo "branch 'Fail _' not implemented"

        Load ->
          Debug.todo "branch 'Load' not implemented"

    FormMsg formMsg ->
      case model of
        Succ cart form v ->
          Debug.log "form" (Succ (addProductFromForm formMsg cart) (Form.update formMsg form) v, Cmd.none) 

        Fail _ ->
          Debug.todo "branch 'Fail _' not implemented"

        Load ->
          Debug.todo "branch 'Load' not implemented"
    Version vn ->
      case model of
        Succ cart form _ ->
          Debug.log "form" (Succ cart form vn, Cmd.none)

        _ ->
          Debug.todo "branch not implemented"


addProductFromForm : Form.Msg -> Cart.Cart -> Cart.Cart
addProductFromForm formMsg cart =
  case formMsg of
    Form.SubmitProduct maybeProduct ->
      case maybeProduct of
        Just product ->
          Debug.log product.name Cart.update (Cart.AddProduct product) cart
        Nothing ->
          cart

    _ ->
      cart




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  getVersion Version



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Fail err ->
      text (errorToString err)

    Load ->
      text "Loading..."

    Succ cart form v->
      div [ padding10 ] 
        [ h1 [] [ text "Cart" ]
        , Html.map (\msg-> CartMsg msg) (Cart.view cart)
        , hr [] []
        , h2 [] [ text "Add poduct"]
        , Html.map (\msg-> FormMsg msg) (Form.view form)
        , hr [] []
        , p [] [ text ("Created on elm and tauri:" ++ v)]
        , a [ href "https://github.com/larionturlo/elm-cart" ] [ text "Source code"]
        ]
