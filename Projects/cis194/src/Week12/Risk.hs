{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random
import Data.Ord
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die
------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

---------------------------------------------------
-- Excersize 2 --------------------
-- -----------------------
sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

judgeBattle aAry dAry bf =
  foldl acu bf scores
  where 
    acu bf0 (a, b) = Battlefield (attackers bf0 + a) (defenders bf0 + b)
    scores = [(if a>b then (0, -1) else (-1,0)) 
                | (a,b) <- zip aAry dAry]

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = 
  let
    nAtt = min 3 (attackers bf - 1)
    nDef = min 2 (defenders bf)
    diceAtt = sortDesc <$> dice nAtt
    diceDef = sortDesc <$> dice nDef
  in
    liftM3 judgeBattle diceAtt diceDef (return bf)

{- 
runRand (diceAtt bf) (mkStdGen 1)

nAtt bf = min 3 (attackers bf - 1)

diceAtt bf = sortDesc <$> dice (nAtt bf)

nDef bf = max 2 (defenders bf)

diceDef bf = sortDesc <$> dice (nDef bf)
-}

-- exercise 3 ----------------------
-- invade until attackers<2 or no defenders left
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield a d) 
  | a < 2 || d == 0 = return bf
  | otherwise = battle bf >>= invade

-- exercise 4 ----------------------
-- 1000 test run
successProb :: Battlefield -> Rand StdGen Double
successProb bf =
  do 
    battleResults <- replicateM 1000 (invade bf)
    return (((sum . map isWin) battleResults) / 1000.0)

isWin (Battlefield a d ) = if a > d then 1 else 0

-- evalRandIO (successProb $ Battlefield 100 100)
