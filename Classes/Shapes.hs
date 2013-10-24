{-
  Shapes.hs

  Creates a class to perform functions to find area and perimeter for
  some abstract type (in this case various shapes that implement instances
  of these functions

-}

-- area takes a shape a and finds its area 
-- as a Double
class Area a where
   area :: a -> Double

-- perim takes a shape A and finds its
-- perimeter or circumference as a Double
-- *NOTE* the compiler flags this for not having default
-- functionality, but this is expected
class Perimeter a where
  perim  :: a -> Double
  circum :: a -> Double 


-- sample basic data types
data Rectangle = Rect {x::Double, y::Double} 
                 deriving (Show, Eq)

data Square    = Square {l::Double} 
                 deriving (Show, Eq) 

data Circle    = Cir {r::Double}
                 deriving (Show, Eq)

-- implement Area and Perimeter for each data type

instance Area Rectangle where
  area (Rect x y) = x * y

instance Perimeter Rectangle where
  perim (Rect x y) = 2*x + 2*y

instance Area Square where
  area (Square r) = r * r

instance Perimeter Square where
  perim (Square r) = r * 4

instance Area Circle where
  area (Cir r) = pi * r * r

instance Perimeter Circle where
  circum (Cir r) = 2 * pi * r
