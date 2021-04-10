module Cart exposing (..)

import List
import Html exposing (Html, div, ul, text, li, button, span)
import Html.Events exposing (onClick)
import List exposing (product)
import AppCss exposing (padding10)

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

type ChangeQuantityWay = Increment | Decrement

type Msg
  = AddProduct Product
  | DeleteProduct ProductName
  | ChangeQuantity ChangeQuantityWay ProductName

update : Msg -> Cart -> Cart
update msg cart =
  case msg of
    AddProduct product ->
      addProduct cart product

    DeleteProduct productName->
      deleteProduct cart productName

    ChangeQuantity change productName ->
      { cart | products = calcQuantity cart.products productName change }
      |> calcTotal

view : Cart -> Html Msg
view cart =
  div []
    [ ul [] (List.map viewProduct cart.products)
    , div [] [ text ("Total coast: " ++ (String.fromFloat cart.total))]
    ]

viewProduct : Product -> Html Msg
viewProduct product =
  li [] [text (product.name ++ " Price: " ++ String.fromFloat product.price)
    , span [ padding10 ]
      [ text "Quantity"
      , button [ onClick (ChangeQuantity Increment product.name) ][ text "+"]
      , span [] [ text (String.fromInt product.quantity) ]
      , button [ onClick (ChangeQuantity Decrement product.name) ][ text "-"]
      ]
    , button [ onClick (DeleteProduct product.name) ] [ text "delete"]
    ]

addProduct : Cart -> Product -> Cart
addProduct cart product =
  if checkProductNameExist cart.products product.name then
    updateProduct cart product
  else
    { cart | products = product :: cart.products }
    |> calcTotal

checkProductNameExist : List Product -> ProductName -> Bool
checkProductNameExist products prodName =
  List.foldl (||) False (List.map (hasName prodName) products)

updateProduct: Cart -> Product -> Cart
updateProduct cart newProduct =
  { cart | products = List.map (changeProduct newProduct) cart.products }
changeProduct: Product -> Product -> Product
changeProduct newProduct oldProduct =
  if hasName newProduct.name oldProduct then
    {oldProduct | quantity = newProduct.quantity}
  else
    oldProduct

deleteProduct: Cart -> ProductName -> Cart
deleteProduct cart productName =
    { cart | products = List.filter (hasntName productName) cart.products }
    |> calcTotal

hasntName : ProductName -> Product -> Bool
hasntName productName product =
    product.name /= productName

hasName : ProductName -> Product -> Bool
hasName productName product =
    product.name == productName

calcTotal : Cart -> Cart
calcTotal cart =
    { cart | total = List.sum (List.map calcCostProduct cart.products)}

calcCostProduct : Product -> Float
calcCostProduct product =
  product.price * toFloat product.quantity

calcQuantity : List Product -> ProductName -> ChangeQuantityWay -> List Product
calcQuantity products productName change =
  List.map (changeQuantity productName change) products

changeQuantity : ProductName -> ChangeQuantityWay -> Product -> Product
changeQuantity productName change prod =
  if productName /= prod.name then
    prod
  else
    case change of
      Increment ->
        { prod | quantity = prod.quantity + 1 }
      Decrement ->
        { prod | quantity = decrementQuntity prod.quantity }

decrementQuntity: Int -> Int
decrementQuntity quant =
  if quant == 1 then
    quant
  else
    quant - 1

