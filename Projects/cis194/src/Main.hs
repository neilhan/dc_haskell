module Main where

import Data.List (find)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  putStrLn (show (toDigits 123))
  putStrLn (show (toDigitsRev 123))

-- to print a solution for Towers of Hanoi
-- history (
--   findHanoiSolution
--       ([PegState "a" [], PegState "b" [1,2,3],PegState "c" []])
--       [(TowersWithHistory ([PegState "a" [1,2,3], PegState "b" [], PegState "c" []]) [] []) ]
-- )

-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
data PegState = PegState {
                name :: Peg,
                disks :: [Int]
                } deriving (Show, Eq)

data TowersWithHistory = TowersWithHistory {
                         pegs :: [PegState],
                         history :: [Move],
                         pegsHistory :: [[PegState]]
                         } deriving (Show, Eq)


findHanoiSolution :: [PegState] -> [TowersWithHistory] -> TowersWithHistory
findHanoiSolution pegs towers
  | (containsSolution pegs towers)  = getTheSolution pegs towers
  | otherwise     =
        findHanoiSolution pegs (towers >>= findAllMoves)


containsSolution ps towers =
    ps `elem` [pegs t | t<-towers]

getTheSolution ps towers =
    fromMaybe (error "not expected") (find (\t-> (ps == (pegs t))) towers)


initPegState :: Peg->Int->PegState
initPegState peg numDisk
  | numDisk > 0 = PegState peg [1..numDisk]
  | otherwise = PegState peg []


-- is a move legal?
-- the new disk can't be great than the current top disk of the Peg
isLegalMove :: PegState -> PegState -> Bool
isLegalMove (PegState _ [])    (PegState _ _ )  = False
isLegalMove (PegState _ (a:_)) (PegState _ [])  = True
isLegalMove (PegState _ (a:_)) (PegState _ (b:_))
  | a < b       = True
  | otherwise = False


-- find the TowersWithHistory that moved the init state to end state
findAllMoves :: TowersWithHistory -> [TowersWithHistory]
findAllMoves (TowersWithHistory pegs history pegsHistory) =
    [(TowersWithHistory
        (buildNewPegs pegs p_from p_to)
        (((name p_from),(name p_to)):history)
        ((buildNewPegs pegs p_from p_to):pegsHistory)
      ) |
        p_from <- pegs,
        p_to <- pegs,
        p_from /= p_to,
        isLegalMove p_from p_to,
       (buildNewPegs pegs p_from p_to) `notElem` pegsHistory
    ]

isSolution :: TowersWithHistory -> [PegState] -> Bool
isSolution (TowersWithHistory pegs history _) targetPegStates
  | pegs == targetPegStates = True
  | length history > 5   = error "too many moves"
  | otherwise            = False

-- giving the current pegs and move, build the new state as type [Peg]
buildNewPegs :: [PegState] -> PegState -> PegState -> [PegState]
buildNewPegs pegs p_from p_to =
    [(newPeg p p_from p_to) | p <- pegs]
    where newPeg p p_from p_to =
            if ((p /= p_from) && (p /= p_to))
            then p
            else if p == p_from
                 then removeTop p_from
                 else addToTop (getTop p_from) p_to

-- get the top of The PegState
getTop :: PegState -> Int
getTop (PegState n (d:ds)) = d
getTop _ = error "PegState has no disk to be removed"

-- add the Int to the top of the Peg
addToTop :: Int -> PegState -> PegState
addToTop d (PegState n ds) = PegState n (d:ds)

-- remove top disk from the PegState
removeTop :: PegState -> PegState
removeTop (PegState n (d:ds)) = PegState n ds
removeTop _ = error "PegState has no disk to be removed"


-- -------------------------------------------------------------------------------
-- validates a Integer as a credit card number.
-- double every second digit from the right, from [1,3,8,6] -> [2, 3, 16, 6]
-- add digits, 2, 3, 16, 6 -> 2 + 3 + 1 + 6 + 6 = 18
-- remainder of by 10, 18%10 = 8
-- if remainder = 0, it's valid, return true
validate :: Integer -> Bool
validate inputInteger = case cardNumberRemainder inputInteger of
                           0 -> True
                           otherwise -> False

cardNumberRemainder cardNum = (sumDigits $ doubleEveryOther (toDigitsRev cardNum)) `rem` 10


toDigitsRev :: Integer -> [Integer]
toDigitsRev inputInteger = reverse $ toDigits inputInteger

toDigits :: Integer -> [Integer]
toDigits inputInteger
  | inputInteger > 0 = map (read::String->Integer) (map charToStr (show inputInteger))
  | otherwise        = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (a : b : xs) = a : (b * 2) : (doubleEveryOther xs)
doubleEveryOther a         = a

doubleEveryOtherFromRight :: [Integer] -> [Integer]
doubleEveryOtherFromRight xs = reverse (doubleEveryOther $ reverse xs)

-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer]->Integer
sumDigits ints = sum (ints >>= toDigits)

charToStr c = [c]
