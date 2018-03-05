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

---------------------------------------------------
-- Excersize 3 --------------------

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

{-
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = 
  do
    nAtt <- min 3 (attackers bf - 1)
    nDef <- max 2 (defenders bf)
    diceAtt <- sortDesc <$> dice nAtt
    diceDef <- sortDesc <$> dice nDef
-}
  