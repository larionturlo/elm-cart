module Cart exposing (..)

import List
import Html exposing (Html, div, ul, text, li, button)
import Html.Events exposing (onClick)

type alias ProductName = String
type alias Product =
  { name : ProductName
  , price : Float
  , quantity : Int
  }

type alias Cart =
  { products : List Product
  , total : Float
  }

type Msg
  = AddProduct Product
  | DeleteProduct ProductName
  | AddQuantity ProductName

update : Msg -> Cart -> Cart
update msg cart =
  case msg of
    AddProduct product ->
      addProduct cart product

    DeleteProduct productName->
      deleteProduct cart productName

    AddQuantity _ ->
      Debug.todo "branch 'AddQuantity _' not implemented"

view : Cart -> Html Msg
view cart =
  div []
    [ ul [] (List.map productView cart.products)
    , div [] [ text (String.fromFloat cart.total)]
    ]

productView : Product -> Html Msg
productView product =
  li [] [text (product.name ++ " price = " ++ String.fromFloat product.price)
    , button [ onClick (DeleteProduct product.name) ] [ text "delete"]
    ]

addProduct : Cart -> Product -> Cart
addProduct cart product =
    { cart | products = List.append [product] cart.products }
    |> calcTotal
deleteProduct: Cart -> ProductName -> Cart
deleteProduct cart productName =
    { cart | products = List.filter (hasntName productName) cart.products }
    |> calcTotal

hasntName : ProductName -> Product -> Bool
hasntName productName product =
    product.name /= productName

calcTotal : Cart -> Cart
calcTotal cart =
    { cart | total = List.sum (List.map calcCostProduct cart.products)}

calcCostProduct : Product -> Float
calcCostProduct product =
  product.price * toFloat product.quantity

