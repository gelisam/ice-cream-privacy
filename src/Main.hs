module Main where

import Data.Foldable
import Data.Ratio


---------------
-- real data --
---------------

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

realData
  :: [PersonInfo]
realData
  = [ PersonInfo 8 Female True False
    , PersonInfo 18 Male False False
    , PersonInfo 24 Female False False
    , PersonInfo 30 Male False True
    , PersonInfo 36 Female True True
    , PersonInfo 66 Female True True
    , PersonInfo 84 Male True True
    ]


-------------------
-- compute stats --
-------------------

-- traverses the list twice, ok for short and reused lists
mean
  :: Integral a
  => [a]
  -> Ratio a
mean xs
  = sum xs
  % fromIntegral (length xs)

-- traverses the list twice, ok for short and reused lists
median
  :: Integral a
  => [a]
  -> Ratio a
median xs
  | odd n
    = -- [0,1,2,3,4] n = 5
      --      ^      5-1 = 4
      --      ^      4/2 = 2
      fromIntegral (xs !! ((n - 1) `div` 2))
  | otherwise  -- even n
    = -- [0,1,2,3,4,5,6,7] n = 8
      --                   8-2 = 6
      --                   6/2 = 3
      --       [3,4,5,6,7] drop 3
      --       [3,4]       take 2
      --       3.5         mean
      mean $ take 2 $ drop ((n-2) `div` 2) xs
  where
    n = length xs

data Stats = Stats
  { count
      :: Int
  , medianAge
      :: Ratio Int
  , meanAge
      :: Ratio Int
  }
  deriving Show

computeStats
  :: (PersonInfo -> Bool)
  -> [PersonInfo]
  -> Stats
computeStats p allData
  = Stats
      { count
          = length matchingAges
      , medianAge
          = median matchingAges
      , meanAge
          = mean matchingAges
      }
  where
    matchingAges
      :: [Int]
    matchingAges
      = fmap age
      $ filter p
      $ allData

predicates
  :: [(String, PersonInfo -> Bool)]
predicates
  = [ ("all", \_ -> True)
    , ("women", \x -> gender x == Female)
    , ("ice-cream", likesIceCream)
    , ("married", isMarried)
    , ( "women-who-like-ice-cream"
      , \x -> gender x == Female && likesIceCream x
      )
    ]


-----------------
-- print stats --
-----------------

main
  :: IO ()
main = do
  forM_ predicates $ \(groupName, predicate) -> do
    putStrLn groupName
    print $ computeStats predicate realData
