module Main where


data Gender
  = Male
  | Female
  deriving (Eq, Ord, Show)

data PersonInfo = PersonInfo
  { age
      :: Int
  , gender
      :: Gender
  , likesIceCream
      :: Bool
  , isMarried
      :: Bool
  }
  deriving Show


main
  :: IO ()
main = do
  print $ (800 :: Integer) ^ (7 :: Integer)
  putStrLn "typechecks."
