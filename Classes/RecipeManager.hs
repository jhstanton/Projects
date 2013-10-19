{-
  RecipeManager.hs

  Implements Recipe data type to store recipe information as well as 
  a manager newtype to store the recipes for maintenance purposes.

  This also implements simple functions to easily write a recipe or
  an entire manager of recipes to files.
-}
import Data.List
import Control.Monad

type RecipeName = String
-- These are used to describe the meal, and are used for sorting. Stored together
type Style      = String
type Course     = String
type MainIng    = String
-- Quantity and Ingrediant will be used to
-- more easily maintain adjustments to recipes
type Quantity   = String
type Ingrediant = String
type Step       = String

data Recipe = Recipe RecipeName (Style, Course, MainIng) [(Quantity, Ingrediant)] [Step]

-- Prints the recipe nicely
instance Show Recipe where
  show (Recipe n _ is steps) = 
    n ++ "\n\n" ++ concat (map (\(amt, ing) ->  
    amt ++ ": " ++ ing ++ "\n") is) ++ "\n" ++ intercalate "\n" steps ++ "\n\n"

-- Recipe getter methods
name    (Recipe name _ _ _ )          = name
style   (Recipe _ (style,_,_) _ _ )   = style
course  (Recipe _ (_, course, _) _ _) = course
mainIng (Recipe _ (_, _, ing) _ _)    = ing
ings    (Recipe _ _ is _ )            = is
steps   (Recipe _ _ _ steps)          = steps

descriptors (Recipe _ ds _ _ )        = ds

-- Writes a given recipe to a given file. 
-- ** WARNING ** If file recipefile already exists it will be overwritten
writeRec recipeFile recipe = writeFile recipeFile (show recipe) 


-- manages the recipes, uses newtype since only one field is requiredo
newtype Manager = Manager [Recipe] 
                  deriving (Show)

getRecipes (Manager recipes) = recipes

-- Sorts the recipes in various ways (using the descriptors) and returns a new manager
sortByStyle  (Manager recipes) = Manager (sortRecipes style recipes [])
sortByCourse (Manager recipes) = Manager (sortRecipes course recipes [])
sortByIng    (Manager recipes) = Manager (sortRecipes mainIng recipes [])


-- Used to sort recipes by internal component. F is a getter function for recipes
-- Takes a list of recipes and returns a new list of recipes
sortRecipes _ [] acc         = acc
sortRecipes f (r:rs) []      = sortRecipes f rs [r]
sortRecipes f (r:rs) (a:acc) = 
  if f r <= f a 
  then sortRecipes f rs (r:a:acc)
  else sortRecipes f rs (a : (sortRecipes f [r] acc))

{- Writes all recipes to individual .txt files in the current working directory
 ******  WARNING  ******  names of the .txt files are determined by the name of the dish
 and no further input is taken from function caller. MAY TRUNCATE EXISTING FILES IN CURRENT
 WORKING DIRECTORY -}
writeRecipesByName (Manager recipes) = mapM_ (\r -> writeRec (name r) r) recipes

{- Writes recipes to files with names derives from fileNames. If file names list is too
   long it will be truncated, and vice versa. Recipes are truncated at end. 
   ** WARNING ** Truncates any files that already exist
-}
writeRecipesByList fileNames (Manager recipes) = 
  mapM_ (\(f,r) -> writeRec f r) $ zip fileNames recipes 

-- Tiny test data
man = Manager [(Recipe "Crab Rangoon" ("Chinese", "Appetizer", "Crab") 
                 [("1 cup", "Cream cheese"), ("2 cups", "flour")] ["Make dough", "Stuff"]),
               (Recipe "Fettucine Alfredo" ("Italian", "Entree", "Chicken")
                 [("4 cup", "Heavy Cream"), ("2 lb", "Chicken")] ["Cook chicken", "Do stuff"]),
               (Recipe "Hamburger" ("American", "Entree", "Beef") 
                 [("1 lb", "Hamburger"), ("1", "Onion")] ["Form patties", "Brown beef"])]
               
