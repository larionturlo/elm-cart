module Cart exposing (..)

import List

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
addProduct model product =
    { model | products = List.append [product] model.products }

deleteProduct: Cart -> ProductName -> Cart
deleteProduct model productName =
    { model | products = List.filter (hasntName productName) model.products }

hasntName : ProductName -> Product -> Bool
hasntName productName product =
    product.name /= productName


