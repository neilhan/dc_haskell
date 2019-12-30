{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Ch05.Json (JsonValue(..),
                  JAry(..),
                  JObj(..),
                  getString,
                  getInt,
                  getDouble,
                  getBool,
                  getObject,
                  getArray,
                  isNull) where

import Control.Arrow (second)

data JsonValue = JsonString String
               | JsonNumber Double
               | JsonBool Bool
               | JsonNull
               | JsonObject (JObj JsonValue) -- [(String, JsonValue)]
               | JsonArray (JAry JsonValue) --[JsonValue]
                  deriving (Eq, Ord, Show)

newtype JAry a = JAry {fromJAry :: [a]
                      } deriving (Eq, Ord, Show)

newtype JObj a = JObj {fromJObj :: [(String, a)]
                      } deriving (Eq, Ord, Show)

getString :: JsonValue -> Maybe String
getString (JsonString s) = Just s
getString _ = Nothing

getInt (JsonNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble (JsonNumber n) = Just n
getDouble _ = Nothing

getBool (JsonBool b) = Just b
getBool _ = Nothing

getObject (JsonObject o) = Just o
getObject _ = Nothing

getArray (JsonArray a) = Just a
getArray _ = Nothing

isNull v = v == JsonNull

type JsonError = String

-- Json Type Class -------------------------
class Json a where
  toJsonValue :: a -> JsonValue
  fromJsonValue :: JsonValue -> Either JsonError a

instance Json JsonValue where
  toJsonValue = id
  fromJsonValue = Right

instance {-# OVERLAPPING #-} Json String where  -- can OVERLAPS, [char] can be OVERLAPPABLE
  toJsonValue = JsonString
  fromJsonValue (JsonString b) = Right b
  fromJsonValue _ = Left "not a JsonString"

instance Json Bool where
  toJsonValue = JsonBool
  fromJsonValue (JsonBool b) = Right b
  fromJsonValue _ = Left "Not a Json Bool"


doubleToJsonValue :: (Double -> a) -> JsonValue -> Either JsonError a
doubleToJsonValue f (JsonNumber v) = Right (f v)
doubleToJsonValue _ _ = Left "Not a Json Number"

instance Json Int where
  toJsonValue = JsonNumber . realToFrac
  fromJsonValue = doubleToJsonValue round

instance Json Integer where
  toJsonValue = JsonNumber . realToFrac
  fromJsonValue = doubleToJsonValue round

instance Json Double where
  toJsonValue = JsonNumber
  fromJsonValue = doubleToJsonValue id

instance (Json a) => Json (JAry a) where
  toJsonValue = jaryToJsonValue
  fromJsonValue = jaryFromJsonValue  -- Either JsonError a

jaryToJsonValue :: (Json a) => JAry a -> JsonValue
jaryToJsonValue = JsonArray . JAry . map toJsonValue . fromJAry

jaryFromJsonValue :: (Json a) => JsonValue -> Either JsonError (JAry a)
jaryFromJsonValue (JsonArray (JAry a)) = whenRight JAry (mapEithers fromJsonValue a)
jaryFromJsonValue _ = Left "Not a Json Array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                      Left err -> Left err
                      Right ys -> case f x of
                                  Left err -> Left err
                                  Right y -> Right (y:ys)

mapEithers _ _ = Right []

instance (Json a) => Json (JObj a) where
  -- (JObj a) -> JsonObject
  toJsonValue = JsonObject . JObj . map (second toJsonValue) . fromJObj
  -- JsonObject -> Either String a -- JObj a
  fromJsonValue (JsonObject (JObj a)) =
    whenRight JObj (mapEithers unwrap a)
    where unwrap (k,v) = whenRight ((,) k) (fromJsonValue v)
  fromJsonValue _ = Left "Not a Json Object"
