module Ch03.Type
  (BookInfo,
  Customer(..),
  CustomerID,
  )
  where

data BookInfo = Book Int String [String]
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody
  deriving (Show)

data Customer = Customer {customerId :: CustomerID,
                          customerName :: String,
                          customerAddress :: [String]
                          } deriving (Show)

data Author = Author {authorId :: CustomerID,
                      authorName :: String,
                      authorAddress :: [String]
                      } deriving (Show)

data Tree a = TreeNode a (Tree a) (Tree a)
            | Empty
              deriving (Show)

fromMaybe defVal wrapped = case wrapped of
                             Nothing -> defVal
                             Just value -> value

-- pattern matching
myNot True = False
myNot False = True

-- guards
lend amount banance
  | amount <= 0 = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise = Just newBalance
  where reserve = 100
        newBalance = balance - amount


{-|
let customer1 = Customer 1 "customer1" ["adr1", "adr2"]
-}
