{-
  Inventory.hs

  Implements Product and Inventory data types
  to maintain an Inventory system
-}
import Data.List

type Price    = Double
type ID       = Integer
type Quantity = Integer
type Value    = Double

-- implement simple Product and Inventory data types
data Product = Product Price ID Quantity

data Inventory = Inv [Product] Value

-- Set up print functions for each new data type
instance Show Product where
  show (Product p id q) = "$" ++ show p ++ " ID:" ++ show id ++ 
                        " " ++ show q

instance Show Inventory where
  show (Inv ps v) = concat (map (\x -> (show x) ++ "\n" ) ps)

-- Product functions
setPrice newPrice (Product _ id q) = Product newPrice id q
setID newID (Product p _ q) = Product p newID q
setQuantity newQuan (Product p id _) = Product p id newQuan

getPrice (Product p _ _) = p
getID (Product _ id _) = id
getQuan (Product _ _ q) = q

-- sells one item
sell (Product p id q) = Product p id (q - 1)
-- receives x items, adding them to q
receive x (Product p id q) = Product p id (q + x)
-- compares ID values to ensure distinct IDs are used
sameID (Product _ id1 _) (Product _ id2 _) = id1 == id2
compID (Product _ id1 _) (Product _ id2 _) 
  | id1 <= id2 = LT
  | otherwise  = GT

-- Inventory functions

getVal (Inv _ v) = v
getProds (Inv ps _) = ps
-- creates a new inventory from a given list of products
newInv ps = Inv (sortBy compID ps) (foldl 
                (\x (Product p _ q) -> x + p * (fromIntegral q)) 0.0 ps) 
               
-- adds a new unique item to the inventory  
add newProd (Inv curProds _) = 
  case (find (sameID newProd) curProds) of
    Nothing -> Right (newInv (newProd : curProds))
    _       -> Left "Cannot add product to inventory without unique ID"

-- remove item by product
removeByProd p (Inv prods _) = newInv $ filter (sameID p) prods

-- remove product by ID
removeByID pID (Inv prods _) = 
  newInv $ filter (\p -> (getID p) /= pID) prods 


