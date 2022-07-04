{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Foldable
import Data.Ratio
import Text.Printf


------------------
-- real dataset --
------------------

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

realDataset
  :: [PersonInfo]
realDataset
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
  deriving (Eq, Show)

computeStats
  :: [PersonInfo]
  -> Stats
computeStats infos
  = Stats
      { count
          = length infos
      , medianAge
          = median (fmap age infos)
      , meanAge
          = mean (fmap age infos)
      }


------------------------
-- brute force attack --
------------------------

bruteForce
  :: Eq b
  => (a -> b)
  -> b
  -> [a]
  -> [a]
bruteForce computeB actualB
  = filter (\a -> computeB a == actualB)

possiblePersonInfos
  :: [PersonInfo]
possiblePersonInfos
    = PersonInfo
  <$> [0..120]
  <*> [Male,Female]
  <*> [False,True]
  <*> [False,True]


-----------------
-- print stats --
-----------------

printPersonInfo
  :: PersonInfo
  -> IO ()
printPersonInfo (PersonInfo {..}) = do
  printf "%2d %c %c %c\n"
    age
    (if gender == Female then 'F' else 'M')
    (if likesIceCream then 'I' else '-')
    (if isMarried then 'M' else '-')

printStats
  :: String
  -> Stats
  -> IO ()
printStats name (Stats {..}) = do
  printf "%2s * %2d  %2.2f | %2.2f\n"
    name
    count
    (realToFrac medianAge :: Double)
    (realToFrac meanAge :: Double)

predicates
  :: [(String, PersonInfo -> Bool)]
predicates
  = [ ("", \_ -> True)
    , ("F", \x -> gender x == Female)
    , ("I", likesIceCream)
    , ("M", isMarried)
    , ( "FI"
      , \x -> gender x == Female && likesIceCream x
      )
    ]

main
  :: IO ()
main = do
  let computeGroupStats
        :: [PersonInfo]
        -> [(String, PersonInfo -> Bool, Stats)]
      computeGroupStats dataset
        = [ ( groupName
            , predicate
            , computeStats
            $ filter predicate
            $ dataset
            )
          | (groupName, predicate) <- predicates
          ]
      publishedGroupStats
        :: [(String, PersonInfo -> Bool, Stats)]
      publishedGroupStats
        = computeGroupStats realDataset
  putStrLn "Given the following published stats:"
  forM_ publishedGroupStats $ \(groupName, _, stats) -> do
    printStats groupName stats

  putStrLn "==="

  -- brute forcing all 7 people would be untractable, demonstrate the idea
  -- by brute forcing the first 2
  let knownTail
        :: [PersonInfo]
      knownTail
        = drop 2 realDataset
      possibleDatasets
        :: [[PersonInfo]]
      possibleDatasets = do
        person1 <- possiblePersonInfos
        person2 <- possiblePersonInfos
        pure (person1 : person2 : knownTail)
      attackableSurface
        :: [(String, PersonInfo -> Bool, Stats)]
        -> [Stats]
      attackableSurface
        = fmap (\(_,_,stats) -> stats)
      plausibleDatasets
        :: [[PersonInfo]]
      plausibleDatasets
        = bruteForce
            (attackableSurface . computeGroupStats)
            (attackableSurface publishedGroupStats)
            possibleDatasets
  putStrLn "The attacker can infer that the real dataset is one"
  putStrLn "of the following:"
  forM_ plausibleDatasets $ \plausibleDataset -> do
    putStrLn "---"
    forM_ plausibleDataset $ \personInfo -> do
      printPersonInfo personInfo
