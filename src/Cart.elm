module Cart exposing (..)

import List
import List exposing (product)

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

addProduct : Cart -> Product -> Cart
addProduct cart product =
    { cart | products = List.append [product] cart.products }

deleteProduct: Cart -> ProductName -> Cart
deleteProduct cart productName =
    { cart | products = List.filter (hasntName productName) cart.products }

hasntName : ProductName -> Product -> Bool
hasntName productName product =
    product.name /= productName

calcTotal : Cart -> Cart
calcTotal cart =
    { cart | total = List.sum (List.map calcCostProduct cart.products)}

calcCostProduct : Product -> Float
calcCostProduct product =
  product.price * toFloat product.quantity

