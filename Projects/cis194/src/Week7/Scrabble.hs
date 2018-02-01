{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, InstanceSigs #-}

module Week7.Scrabble where

import Data.Monoid
import Data.Char
import Data.List.Split
import Week7.Sized
import Week7.JoinList
import Week7.Buffer
import Week7.Editor

-- exercise 3, scoring metric by Scrabble ----------------
-- count Scrabble score
-- cache Scrabble score for every line in a buffer

-- Score type
newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

-- Monoid instance for Score
{-
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  { - # MINIMAL mempty, mappend # - }
        -- Defined in ‘GHC.Base’
-}
instance Monoid Score where
    mempty = Score 0
    mappend = (+)

--
score :: Char -> Score
score c
    | elem (toUpper c) "AEIOULNSTR" = 1
    | elem (toUpper c) "DG"         = 2
    | elem (toUpper c) "BCMP"       = 3
    | elem (toUpper c) "FHVWY"      = 4
    | elem (toUpper c) "K"          = 5
    | elem (toUpper c) "JX"         = 8
    | elem (toUpper c) "QZ"         = 10
    | otherwise = 0

--
scoreString :: String -> Score
scoreString s = foldl (+) 0 (fmap score s)

-- score a line
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- exercise 4 -------------------------------------
-- instance for instance Buffer (JoinList (Score, Size) String)

valueOfScore (Score i) = i

instance Buffer (JoinList (Score, Size) String) where
    -- | Convert a buffer to a String.
    toString Empty = ""
    toString (Single _ s) = s
    toString (Append _ a b) = (toString a) ++ "\n" ++ (toString b)

    -- | Create a buffer from a String.
    fromString :: String -> (JoinList (Score, Size) String)
    fromString s = foldl (+++) Empty [Single ((scoreString l), 1) l | l <- lines s]

    -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
    -- for out-of-bounds indices.
    line :: Int -> (JoinList (Score, Size) String) -> Maybe String
    line = indexJ

    -- | @replaceLine n ln buf@ returns a modified version of @buf@,
    --   with the @n@th line replaced by @ln@.  If the index is
    --   out-of-bounds, the buffer should be returned unmodified.
    replaceLine :: Int -> String -> (JoinList (Score, Size) String) -> (JoinList (Score, Size) String)
    replaceLine i str a = takeJ (i-1) a +++ Single (scoreString str, 1) str +++ dropJ (i+1) a

    -- | Compute the number of lines in the buffer.
    numLines :: (JoinList (Score, Size) String) -> Int
    numLines Empty = 0
    numLines (Single m _) = (getSize . size) m
    numLines (Append m _ _) = (getSize . size) m

    -- | Compute the value of the buffer, i.e. the amount someone would
    --   be paid for publishing the contents of the buffer.
    value :: (JoinList (Score, Size) String) -> Int
    value Empty = 0
    value (Single (sc, sz) str) = valueOfScore sc
    value (Append (sc, sz) _ _) = valueOfScore sc

-- try Editor Buffer

main = runEditor editor $ (fromString (unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]) :: (JoinList (Score, Size) String))
